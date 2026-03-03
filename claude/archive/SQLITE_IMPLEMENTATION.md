# SQLite Implementation Guide for cot.land Package Registry

## Problem

The cot.land package registry (`~/cot-land/pkg`) stores all data in a single `data/registry.json` file. The `Registry` struct loads the entire file into a `Map(string, Package)` on startup and fully rewrites it on every mutation (`save()` calls `writeFile` with the entire JSON blob). This is unsuitable for production:

- **Crash corruption**: A crash mid-write corrupts the entire registry
- **O(n) rewrites**: Every `create()` or `addVersion()` serializes and writes all packages
- **No concurrent access**: Single-process, single-file, no locking
- **No search indexing**: Linear scan over all packages for search queries

SQLite solves all of these: atomic transactions, indexed queries, concurrent readers, and FTS5 for full-text search.

---

## Phase A: Compiler Enhancement — `"libs"` Field in cot.json [DONE]

### Goal

Allow `cot.json` to specify native libraries to link against:

```json
{
  "name": "pkg",
  "safe": true,
  "libs": ["sqlite3"]
}
```

When `cot build` runs, it appends `-lsqlite3` to the linker invocation.

### File 1: `~/cotlang/cot/compiler/project.zig`

The `ProjectConfig` struct (line 8) is parsed by `std.json.parseFromSlice` with `ignore_unknown_fields`. Adding a `libs` field to this struct **won't work** because `[]const []const u8` doesn't parse cleanly from JSON arrays with Zig's `std.json.parseFromSlice`.

Instead, add a `getLibs()` method to `LoadedConfig` that uses the same raw JSON parsing pattern as `getTask()` (line 55-67):

```zig
// Add this method to LoadedConfig (after listTasks, before deinit)

/// Read the "libs" array from cot.json for native library linking.
/// Returns library names (e.g., ["sqlite3"]) or null if not specified.
pub fn getLibs(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
    if (self.file_contents.len == 0) return null;
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
    defer parsed.deinit();
    const root = parsed.value;
    if (root != .object) return null;
    const libs = root.object.get("libs") orelse return null;
    if (libs != .array) return null;
    var names = std.ArrayListUnmanaged([]const u8){};
    for (libs.array.items) |item| {
        if (item != .string) continue;
        names.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
    }
    if (names.items.len == 0) return null;
    return names.toOwnedSlice(allocator) catch null;
}
```

**Where to insert**: After the `listTasks` method (after line 86), before `deinit` (line 88).

### File 2: `~/cotlang/cot/compiler/main.zig`

The linker invocation is in `compileAndLinkFull()` at lines 1389-1449. The function builds a `link_args` array and spawns `zig cc`. Native libraries must be appended as `-l<name>` flags **before** the child process spawns (line 1432).

**Step 1**: Load cot.json at the top of `compileAndLinkFull()`. The function currently receives `input_file` but doesn't load the project config. Add config loading right after the existing variable declarations:

At line 1286 (inside `compileAndLinkFull`, after the opening brace), add:

```zig
    // Load cot.json for project-level link settings (libs, etc.)
    var project_config = project.loadConfig(allocator, null) catch null;
    defer if (project_config) |*pc| pc.deinit();
```

**Step 2**: After the platform-specific `-lSystem` / `-lc` block (after line 1430), insert the libs loop:

```zig
    // Append user-specified native libraries from cot.json "libs" array
    if (project_config) |*pc| {
        if (pc.getLibs(allocator)) |libs| {
            for (libs) |lib_name| {
                const flag = std.fmt.allocPrint(allocator, "-l{s}", .{lib_name}) catch continue;
                link_args.append(allocator, flag) catch {};
            }
        }
    }
```

**Insert point**: Between line 1430 (`}` closing the Linux `-lc` block) and line 1432 (`var child = ...`).

### Verification

After making these changes:

```bash
cd ~/cot-land/pkg
# Add to cot.json: "libs": ["sqlite3"]
cot build src/main.cot -o pkg
```

The linker invocation should include `-lsqlite3`. On macOS, `libsqlite3` is part of the SDK (in dyld shared cache), so no Homebrew install is needed. Verify with:

```bash
# Confirm sqlite3 is available to the linker
zig cc -print-file-name=libsqlite3.tbd
# Or simply: the build should succeed without "library not found" errors
```

### Test

Add a test to `project.zig`:

```zig
test "loadConfig: getLibs reads libs array" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\    "name": "test",
        \\    "libs": ["sqlite3", "curl"]
        \\}
    ;
    var loaded = try parseConfig(allocator, json);
    loaded.file_contents = json;
    defer loaded.deinit();
    const libs = loaded.getLibs(allocator).?;
    defer allocator.free(libs);
    try std.testing.expectEqual(@as(usize, 2), libs.len);
    try std.testing.expectEqualStrings("sqlite3", libs[0]);
    try std.testing.expectEqualStrings("curl", libs[1]);
    allocator.free(libs[0]);
    allocator.free(libs[1]);
}

test "loadConfig: getLibs returns null when no libs" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    try std.testing.expect(loaded.getLibs(allocator) == null);
}
```

---

## Phase B: `std/sqlite` — Low-Level FFI Bindings [DONE]

### Goal

New stdlib module: `~/cotlang/cot/stdlib/sqlite.cot`

Provides `extern fn` declarations for the SQLite C API, plus Cot-friendly helpers for string conversion and result extraction.

### SQLite C API Surface Needed

Only ~15 functions are required for a full registry backend:

| SQLite Function | Purpose |
|----------------|---------|
| `sqlite3_open` | Open database file |
| `sqlite3_close` | Close database |
| `sqlite3_exec` | Execute SQL without results (DDL, INSERT) |
| `sqlite3_prepare_v2` | Compile SQL to statement |
| `sqlite3_step` | Execute/advance statement |
| `sqlite3_finalize` | Destroy statement |
| `sqlite3_bind_text` | Bind string parameter |
| `sqlite3_bind_int64` | Bind integer parameter |
| `sqlite3_column_text` | Read text column |
| `sqlite3_column_bytes` | Read column byte length |
| `sqlite3_column_int64` | Read integer column |
| `sqlite3_column_count` | Number of result columns |
| `sqlite3_reset` | Reset statement for re-execution |
| `sqlite3_errmsg` | Get error message |
| `sqlite3_changes` | Rows modified by last statement |

### Critical FFI Details

**1. String conversion**: Cot strings are `(ptr, len)` pairs. SQLite C functions expect null-terminated C strings for:
- `sqlite3_open(filename, &db)` — filename must be null-terminated
- `sqlite3_exec(db, sql, ...)` — SQL must be null-terminated

However, `sqlite3_prepare_v2(db, sql, nByte, &stmt, NULL)` accepts an `nByte` parameter, so SQL strings **do not** need null-termination when using prepare. This is the preferred path.

**2. Output pointers**: `sqlite3_open` writes a `sqlite3*` via an output pointer:
```c
int sqlite3_open(const char *filename, sqlite3 **ppDb);
```
In Cot, allocate 8 bytes for the output pointer, pass its address, then read the result:
```cot
var db_ptr = alloc(0, 8)  // space for sqlite3*
sqlite3_open(cstr_path, db_ptr)
var db = @intToPtr(*i64, db_ptr).*  // read the sqlite3* handle
dealloc(db_ptr)
```

**3. Column text ownership**: `sqlite3_column_text` returns a pointer owned by SQLite. It is only valid until the next `sqlite3_step`, `sqlite3_reset`, or `sqlite3_finalize` call. You **must** copy the bytes into Cot-managed memory before moving on.

**4. SQLITE_TRANSIENT**: When binding text with `sqlite3_bind_text`, the 5th parameter is a destructor. Passing `SQLITE_TRANSIENT` (-1 cast to a function pointer) tells SQLite to make its own copy of the string data. This is essential because Cot strings may be garbage-collected.

### Full Implementation

```cot
// sqlite.cot — SQLite3 FFI bindings for the Cot standard library.
//
// Provides low-level access to SQLite3 via extern fn declarations.
// All sqlite3 pointers/handles are passed as i64 (opaque handles).
//
// Reference: https://sqlite.org/c3ref/funclist.html

import "std/sys"
import "std/string"

// ===== SQLite result codes =====

const SQLITE_OK: i64 = 0
const SQLITE_ERROR: i64 = 1
const SQLITE_ROW: i64 = 100
const SQLITE_DONE: i64 = 101
const SQLITE_TRANSIENT: i64 = -1

// ===== Core SQLite3 extern functions =====
// All pointer types mapped to i64 (Cot FFI convention, see sys.cot).

extern fn sqlite3_open(filename: i64, ppDb: i64) i64
extern fn sqlite3_close(db: i64) i64
extern fn sqlite3_exec(db: i64, sql: i64, callback: i64, arg: i64, errmsg: i64) i64
extern fn sqlite3_prepare_v2(db: i64, sql: i64, nByte: i64, ppStmt: i64, pzTail: i64) i64
extern fn sqlite3_step(stmt: i64) i64
extern fn sqlite3_finalize(stmt: i64) i64
extern fn sqlite3_reset(stmt: i64) i64

// Bind parameters (1-indexed)
extern fn sqlite3_bind_text(stmt: i64, idx: i64, text: i64, nByte: i64, destructor: i64) i64
extern fn sqlite3_bind_int64(stmt: i64, idx: i64, value: i64) i64
extern fn sqlite3_bind_null(stmt: i64, idx: i64) i64

// Read columns (0-indexed)
extern fn sqlite3_column_text(stmt: i64, col: i64) i64
extern fn sqlite3_column_bytes(stmt: i64, col: i64) i64
extern fn sqlite3_column_int64(stmt: i64, col: i64) i64
extern fn sqlite3_column_count(stmt: i64) i64

// Metadata
extern fn sqlite3_errmsg(db: i64) i64
extern fn sqlite3_changes(db: i64) i64

// ===== String helpers =====

/// Convert a Cot string to a null-terminated C string.
/// Allocates len+1 bytes, copies data, appends null byte.
/// Caller must call freeCstr() when done.
fn cstr(s: string) i64 {
    var len = @lenOf(s)
    var buf = alloc(0, len + 1)
    memcpy(buf, @ptrOf(s), len)
    @intToPtr(*u8, buf + len).* = @intCast(u8, 0)
    return buf
}

/// Free a C string allocated by cstr().
fn freeCstr(ptr: i64) void {
    dealloc(ptr)
}

/// Read a text column from a prepared statement as a Cot string.
/// Copies the data — safe to use after sqlite3_step/finalize.
fn columnString(stmt: i64, col: i64) string {
    var text_ptr = sqlite3_column_text(stmt, col)
    if (text_ptr == 0) { return "" }
    var nbytes = sqlite3_column_bytes(stmt, col)
    if (nbytes <= 0) { return "" }
    var buf = alloc(0, nbytes)
    memcpy(buf, text_ptr, nbytes)
    return @string(buf, nbytes)
}

// ===== High-level wrappers =====

/// Open a SQLite database. Returns the db handle (i64).
/// On failure, returns 0.
fn sqliteOpen(path: string) i64 {
    var c_path = cstr(path)
    var db_out = alloc(0, 8)
    var rc = sqlite3_open(c_path, db_out)
    freeCstr(c_path)
    var db = @intToPtr(*i64, db_out).*
    dealloc(db_out)
    if (rc != SQLITE_OK) {
        if (db != 0) { sqlite3_close(db) }
        return 0
    }
    return db
}

/// Close a SQLite database.
fn sqliteClose(db: i64) void {
    sqlite3_close(db)
}

/// Execute a SQL string that returns no data (CREATE TABLE, INSERT, etc.).
/// Uses sqlite3_prepare_v2 + step + finalize (no null-termination needed).
/// Returns SQLITE_OK on success, error code on failure.
fn sqliteExec(db: i64, sql: string) i64 {
    var stmt_out = alloc(0, 8)
    var rc = sqlite3_prepare_v2(db, @ptrOf(sql), @lenOf(sql), stmt_out, 0)
    var stmt = @intToPtr(*i64, stmt_out).*
    dealloc(stmt_out)
    if (rc != SQLITE_OK) { return rc }
    rc = sqlite3_step(stmt)
    sqlite3_finalize(stmt)
    if (rc == SQLITE_DONE) { return SQLITE_OK }
    return rc
}

/// Prepare a SQL statement. Returns the stmt handle (i64).
/// On failure, returns 0.
/// Uses nByte parameter — SQL string does NOT need null-termination.
fn sqlitePrepare(db: i64, sql: string) i64 {
    var stmt_out = alloc(0, 8)
    var rc = sqlite3_prepare_v2(db, @ptrOf(sql), @lenOf(sql), stmt_out, 0)
    var stmt = @intToPtr(*i64, stmt_out).*
    dealloc(stmt_out)
    if (rc != SQLITE_OK) { return 0 }
    return stmt
}

/// Bind a Cot string to a prepared statement parameter (1-indexed).
/// Uses SQLITE_TRANSIENT so SQLite copies the string data.
fn bindText(stmt: i64, idx: i64, value: string) i64 {
    return sqlite3_bind_text(stmt, idx, @ptrOf(value), @lenOf(value), SQLITE_TRANSIENT)
}

/// Bind an integer to a prepared statement parameter (1-indexed).
fn bindInt(stmt: i64, idx: i64, value: i64) i64 {
    return sqlite3_bind_int64(stmt, idx, value)
}
```

### Placement

Save as `~/cotlang/cot/stdlib/sqlite.cot`. It imports `std/sys` and `std/string` which are existing stdlib modules.

### Usage Pattern

```cot
import "std/sqlite"

var db = sqliteOpen("data/registry.db")
if (db == 0) { exit(1) }

// Create table
sqliteExec(db, "CREATE TABLE IF NOT EXISTS packages (name TEXT PRIMARY KEY, description TEXT)")

// Insert with prepared statement
var stmt = sqlitePrepare(db, "INSERT INTO packages (name, description) VALUES (?1, ?2)")
bindText(stmt, 1, "json-utils")
bindText(stmt, 2, "JSON utilities for Cot")
sqlite3_step(stmt)
sqlite3_finalize(stmt)

// Query
var q = sqlitePrepare(db, "SELECT name, description FROM packages WHERE name = ?1")
bindText(q, 1, "json-utils")
if (sqlite3_step(q) == SQLITE_ROW) {
    var name = columnString(q, 0)
    var desc = columnString(q, 1)
}
sqlite3_finalize(q)

sqliteClose(db)
```

---

## Phase C: `src/db.cot` — High-Level Registry Database Layer

### Goal

New file: `~/cot-land/pkg/src/db.cot`

Provides a `Db` struct that wraps SQLite with methods matching the current `Registry` API surface. This is the bridge between raw SQLite and the registry's domain model.

### Schema

```sql
CREATE TABLE IF NOT EXISTS packages (
    name TEXT PRIMARY KEY,
    description TEXT NOT NULL,
    author TEXT NOT NULL,
    created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS versions (
    package_name TEXT NOT NULL,
    number TEXT NOT NULL,
    published TEXT NOT NULL,
    checksum TEXT NOT NULL,
    PRIMARY KEY (package_name, number),
    FOREIGN KEY (package_name) REFERENCES packages(name)
);

CREATE TABLE IF NOT EXISTS dependencies (
    package_name TEXT NOT NULL,
    version_number TEXT NOT NULL,
    dep_name TEXT NOT NULL,
    version_req TEXT NOT NULL,
    PRIMARY KEY (package_name, version_number, dep_name),
    FOREIGN KEY (package_name, version_number) REFERENCES versions(package_name, number)
);

-- FTS5 virtual table for search (replaces search_index.cot linear scan)
CREATE VIRTUAL TABLE IF NOT EXISTS packages_fts USING fts5(
    name,
    description,
    content='packages',
    content_rowid='rowid'
);

-- Triggers to keep FTS in sync
CREATE TRIGGER IF NOT EXISTS packages_ai AFTER INSERT ON packages BEGIN
    INSERT INTO packages_fts(rowid, name, description)
    VALUES (new.rowid, new.name, new.description);
END;

CREATE TRIGGER IF NOT EXISTS packages_ad AFTER DELETE ON packages BEGIN
    INSERT INTO packages_fts(packages_fts, rowid, name, description)
    VALUES ('delete', old.rowid, old.name, old.description);
END;

CREATE TRIGGER IF NOT EXISTS packages_au AFTER UPDATE ON packages BEGIN
    INSERT INTO packages_fts(packages_fts, rowid, name, description)
    VALUES ('delete', old.rowid, old.name, old.description);
    INSERT INTO packages_fts(rowid, name, description)
    VALUES (new.rowid, new.name, new.description);
END;
```

### Implementation

```cot
/// db.cot — SQLite-backed storage for the package registry.
///
/// Replaces the JSON file store in registry.cot with proper database
/// storage. Provides the same logical operations (create, get, addVersion,
/// search) but backed by SQLite with indexed queries and FTS5 search.

import "std/sqlite"
import "std/string"
import "std/list"
import "std/log"
import "package"

struct Db {
    handle: i64,

    /// Open (or create) the registry database at the given path.
    /// Creates tables and indexes if they don't exist.
    static fn open(path: string) ?Db {
        var db = sqliteOpen(path)
        if (db == 0) {
            logError("failed to open database")
            return null
        }
        var d = Db { handle: db }

        // Enable WAL mode for concurrent readers
        sqliteExec(db, "PRAGMA journal_mode=WAL")
        // Enable foreign keys
        sqliteExec(db, "PRAGMA foreign_keys=ON")

        d.createTables()
        return d
    }

    fn close() void {
        sqliteClose(self.handle)
    }

    fn createTables() void {
        sqliteExec(self.handle, "CREATE TABLE IF NOT EXISTS packages (name TEXT PRIMARY KEY, description TEXT NOT NULL, author TEXT NOT NULL, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
        sqliteExec(self.handle, "CREATE TABLE IF NOT EXISTS versions (package_name TEXT NOT NULL, number TEXT NOT NULL, published TEXT NOT NULL, checksum TEXT NOT NULL, PRIMARY KEY (package_name, number), FOREIGN KEY (package_name) REFERENCES packages(name))")
        sqliteExec(self.handle, "CREATE TABLE IF NOT EXISTS dependencies (package_name TEXT NOT NULL, version_number TEXT NOT NULL, dep_name TEXT NOT NULL, version_req TEXT NOT NULL, PRIMARY KEY (package_name, version_number, dep_name), FOREIGN KEY (package_name, version_number) REFERENCES versions(package_name, number))")

        // FTS5 search index
        sqliteExec(self.handle, "CREATE VIRTUAL TABLE IF NOT EXISTS packages_fts USING fts5(name, description, content='packages', content_rowid='rowid')")

        // FTS sync triggers
        sqliteExec(self.handle, "CREATE TRIGGER IF NOT EXISTS packages_ai AFTER INSERT ON packages BEGIN INSERT INTO packages_fts(rowid, name, description) VALUES (new.rowid, new.name, new.description); END")
        sqliteExec(self.handle, "CREATE TRIGGER IF NOT EXISTS packages_ad AFTER DELETE ON packages BEGIN INSERT INTO packages_fts(packages_fts, rowid, name, description) VALUES ('delete', old.rowid, old.name, old.description); END")
        sqliteExec(self.handle, "CREATE TRIGGER IF NOT EXISTS packages_au AFTER UPDATE ON packages BEGIN INSERT INTO packages_fts(packages_fts, rowid, name, description) VALUES ('delete', old.rowid, old.name, old.description); INSERT INTO packages_fts(rowid, name, description) VALUES (new.rowid, new.name, new.description); END")
    }

    // ===== Package CRUD =====

    /// Check if a package exists.
    fn has(name: string) bool {
        var stmt = sqlitePrepare(self.handle, "SELECT 1 FROM packages WHERE name = ?1")
        if (stmt == 0) { return false }
        bindText(stmt, 1, name)
        var found = sqlite3_step(stmt) == SQLITE_ROW
        sqlite3_finalize(stmt)
        return found
    }

    /// Create a new package. Returns false if already exists.
    fn createPackage(name: string, description: string, author: string) bool {
        if (self.has(name)) { return false }
        var stmt = sqlitePrepare(self.handle, "INSERT INTO packages (name, description, author) VALUES (?1, ?2, ?3)")
        if (stmt == 0) { return false }
        bindText(stmt, 1, name)
        bindText(stmt, 2, description)
        bindText(stmt, 3, author)
        var rc = sqlite3_step(stmt)
        sqlite3_finalize(stmt)
        return rc == SQLITE_DONE
    }

    /// Get a package by name, including all its versions and dependencies.
    fn getPackage(name: string) ?Package {
        var stmt = sqlitePrepare(self.handle, "SELECT name, description, author FROM packages WHERE name = ?1")
        if (stmt == 0) { return null }
        bindText(stmt, 1, name)
        if (sqlite3_step(stmt) != SQLITE_ROW) {
            sqlite3_finalize(stmt)
            return null
        }
        var pkg_name = columnString(stmt, 0)
        var desc = columnString(stmt, 1)
        var author = columnString(stmt, 2)
        sqlite3_finalize(stmt)

        var pkg = Package.init(pkg_name, desc, author)

        // Load versions
        var vstmt = sqlitePrepare(self.handle, "SELECT number, published, checksum FROM versions WHERE package_name = ?1 ORDER BY rowid")
        if (vstmt == 0) { return pkg }
        bindText(vstmt, 1, name)
        while (sqlite3_step(vstmt) == SQLITE_ROW) {
            var ver_num = columnString(vstmt, 0)
            var published = columnString(vstmt, 1)
            var checksum = columnString(vstmt, 2)

            // Load dependencies for this version
            var deps = self.loadDeps(name, ver_num)

            var ver = Version {
                number: ver_num,
                published: published,
                checksum: checksum,
                dependencies: deps,
            }
            pkg.versions.append(ver)
        }
        sqlite3_finalize(vstmt)

        return pkg
    }

    /// Load dependencies for a specific package version.
    fn loadDeps(pkg_name: string, ver_num: string) List(Dependency) {
        var deps: List(Dependency) = .{}
        var stmt = sqlitePrepare(self.handle, "SELECT dep_name, version_req FROM dependencies WHERE package_name = ?1 AND version_number = ?2")
        if (stmt == 0) { return deps }
        bindText(stmt, 1, pkg_name)
        bindText(stmt, 2, ver_num)
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            var dep = Dependency {
                name: columnString(stmt, 0),
                version_req: columnString(stmt, 1),
            }
            deps.append(dep)
        }
        sqlite3_finalize(stmt)
        return deps
    }

    /// Add a version to an existing package. Returns false if package
    /// doesn't exist or version already exists.
    fn addVersion(name: string, ver: Version) bool {
        if (not self.has(name)) { return false }

        // Check if version exists
        var check = sqlitePrepare(self.handle, "SELECT 1 FROM versions WHERE package_name = ?1 AND number = ?2")
        if (check == 0) { return false }
        bindText(check, 1, name)
        bindText(check, 2, ver.number)
        var exists = sqlite3_step(check) == SQLITE_ROW
        sqlite3_finalize(check)
        if (exists) { return false }

        // Begin transaction
        sqliteExec(self.handle, "BEGIN")

        // Insert version
        var stmt = sqlitePrepare(self.handle, "INSERT INTO versions (package_name, number, published, checksum) VALUES (?1, ?2, ?3, ?4)")
        if (stmt == 0) {
            sqliteExec(self.handle, "ROLLBACK")
            return false
        }
        bindText(stmt, 1, name)
        bindText(stmt, 2, ver.number)
        bindText(stmt, 3, ver.published)
        bindText(stmt, 4, ver.checksum)
        var rc = sqlite3_step(stmt)
        sqlite3_finalize(stmt)
        if (rc != SQLITE_DONE) {
            sqliteExec(self.handle, "ROLLBACK")
            return false
        }

        // Insert dependencies
        var i: int = 0
        while (i < ver.dependencies.count) {
            var dep = ver.dependencies.get(i)
            var dstmt = sqlitePrepare(self.handle, "INSERT INTO dependencies (package_name, version_number, dep_name, version_req) VALUES (?1, ?2, ?3, ?4)")
            if (dstmt != 0) {
                bindText(dstmt, 1, name)
                bindText(dstmt, 2, ver.number)
                bindText(dstmt, 3, dep.name)
                bindText(dstmt, 4, dep.version_req)
                sqlite3_step(dstmt)
                sqlite3_finalize(dstmt)
            }
            i += 1
        }

        sqliteExec(self.handle, "COMMIT")
        return true
    }

    /// Get all package names.
    fn allNames() List(string) {
        var names: List(string) = .{}
        var stmt = sqlitePrepare(self.handle, "SELECT name FROM packages ORDER BY name")
        if (stmt == 0) { return names }
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            names.append(columnString(stmt, 0))
        }
        sqlite3_finalize(stmt)
        return names
    }

    /// Get total package count.
    fn count() int {
        var stmt = sqlitePrepare(self.handle, "SELECT COUNT(*) FROM packages")
        if (stmt == 0) { return 0 }
        if (sqlite3_step(stmt) != SQLITE_ROW) {
            sqlite3_finalize(stmt)
            return 0
        }
        var n = sqlite3_column_int64(stmt, 0)
        sqlite3_finalize(stmt)
        return n
    }

    // ===== Search (FTS5) =====

    /// Search packages using FTS5 full-text search.
    /// Returns results ranked by relevance.
    fn search(query: string) List(SearchResult) {
        var results: List(SearchResult) = .{}
        if (@lenOf(query) == 0) { return results }

        // Use FTS5 MATCH with rank for ordering
        var stmt = sqlitePrepare(self.handle, "SELECT p.name, p.description, rank FROM packages_fts f JOIN packages p ON p.rowid = f.rowid WHERE packages_fts MATCH ?1 ORDER BY rank LIMIT 50")
        if (stmt == 0) {
            // Fallback: FTS might not be available, use LIKE
            return self.searchFallback(query)
        }
        bindText(stmt, 1, query)
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            var result = SearchResult {
                name: columnString(stmt, 0),
                description: columnString(stmt, 1),
                score: 100,
            }
            results.append(result)
        }
        sqlite3_finalize(stmt)
        return results
    }

    /// Fallback search using LIKE when FTS5 is unavailable.
    fn searchFallback(query: string) List(SearchResult) {
        var results: List(SearchResult) = .{}
        var pattern = "%" + query + "%"
        var stmt = sqlitePrepare(self.handle, "SELECT name, description FROM packages WHERE name LIKE ?1 OR description LIKE ?1 ORDER BY name LIMIT 50")
        if (stmt == 0) { return results }
        bindText(stmt, 1, pattern)
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            var result = SearchResult {
                name: columnString(stmt, 0),
                description: columnString(stmt, 1),
                score: 50,
            }
            results.append(result)
        }
        sqlite3_finalize(stmt)
        return results
    }
}
```

### Notes on Implementation

1. **WAL mode** (`PRAGMA journal_mode=WAL`): Allows concurrent readers while one writer is active. Essential for a web server.

2. **Transaction usage**: `addVersion` wraps its multi-table insert in `BEGIN`/`COMMIT` to ensure atomicity.

3. **FTS5 with content sync triggers**: The `content='packages'` option makes FTS5 a "content table" that mirrors the `packages` table. Triggers keep it in sync automatically on INSERT/UPDATE/DELETE.

4. **SearchResult reuse**: The `SearchResult` struct from `search_index.cot` is reused. Import it or redefine it here.

---

## Phase D: Registry Migration

### Goal

Replace the `Map`-based `Registry` with the SQLite-backed `Db`, keeping the public API identical so handler files need no changes.

### File 1: `~/cot-land/pkg/src/registry.cot` — Rewrite

Replace the entire file. The new `Registry` wraps `Db` and exposes the same methods:

```cot
/// Package metadata registry — SQLite-backed store.
///
/// Provides the same API as the original JSON-file registry,
/// but backed by SQLite for durability, indexing, and concurrent access.

import "std/string"
import "std/list"
import "std/log"
import "db"
import "package"
import "search_index"

/// SQLite-backed package registry.
struct Registry {
    db: Db,

    /// Open the registry database, creating tables if needed.
    static fn init(db_path: string) Registry {
        if (Db.open(db_path)) |db| {
            var reg = Registry { db: db }
            infoKv("opened registry database", "path", db_path)
            return reg
        }
        // Fatal: can't open database
        logError("failed to open registry database")
        exit(1)
        // unreachable, but Cot needs a return
        var db: Db = .{}
        return Registry { db: db }
    }

    /// Get a package by name.
    fn get(name: string) ?Package {
        return self.db.getPackage(name)
    }

    /// Check if a package exists.
    fn has(name: string) bool {
        return self.db.has(name)
    }

    /// Register a new package (no versions yet).
    fn create(name: string, description: string, author: string) bool {
        return self.db.createPackage(name, description, author)
    }

    /// Add a version to an existing package.
    fn addVersion(name: string, ver: Version) bool {
        return self.db.addVersion(name, ver)
    }

    /// Get all package names.
    fn allNames() List(string) {
        return self.db.allNames()
    }

    /// Get total package count.
    fn count() int {
        return self.db.count()
    }
}
```

**Key point**: Every method signature matches the original `Registry` exactly. The handler files (`api_packages.cot`, `api_versions.cot`, etc.) call `reg.get(name)`, `reg.has(name)`, `reg.create(...)`, `reg.addVersion(...)` — all unchanged.

### File 2: `~/cot-land/pkg/src/main.cot` — Change init path

Change line 70:
```cot
// Before:
var reg = Registry.init("data/registry.json")

// After:
var reg = Registry.init("data/registry.db")
```

Also remove these imports that are no longer needed by `registry.cot`:
- `import "std/map"` — only if no other file uses it (check first)
- `import "std/json"` — only if no other file uses it

Note: `main.cot` itself doesn't import `map` or `json` directly. The `registry.cot` file's imports change, but since Cot resolves imports per-file, this is isolated.

### File 3: `~/cot-land/pkg/cot.json` — Add libs

```json
{
  "name": "pkg",
  "version": "0.1.0",
  "main": "src/main.cot",
  "safe": true,
  "libs": ["sqlite3"]
}
```

### File 4: `~/cot-land/pkg/src/search_index.cot` — Simplify (optional)

The `searchPackages` function in `search_index.cot` currently does a linear scan. It's still used in `main.cot` dispatch (line 166) and `api_search.cot`. Two options:

**Option A (minimal change)**: Keep `search_index.cot` as-is. It still works — it takes a `List(Package)` and searches in-memory. The main dispatch builds this list from `reg.allNames()` + `reg.get()`, which now comes from SQLite. Performance is fine for small registries.

**Option B (delegate to FTS5)**: Add a `searchPackages` wrapper in `registry.cot` that calls `self.db.search(query)` directly, and update the call sites in `main.cot` and `api_search.cot` to use it instead of building the package list.

**Recommendation**: Start with Option A (zero risk), migrate to Option B later.

### Files That Need NO Changes

These files call `Registry` methods that are API-compatible:

- `src/api_packages.cot` — calls `reg.get()`, `reg.has()`, `reg.create()`, `reg.allNames()`
- `src/api_versions.cot` — calls `reg.get()`, `reg.addVersion()`
- `src/api_search.cot` — calls `reg.allNames()`, `reg.get()`
- `src/web_pages.cot` — calls `reg.get()`, `reg.allNames()`
- `src/api_auth.cot` — no registry interaction
- `src/web_templates.cot` — no registry interaction
- `src/web_static.cot` — no registry interaction
- `src/server.cot` — no registry interaction
- `src/router.cot` — no registry interaction
- `src/request.cot` — no registry interaction
- `src/response.cot` — no registry interaction
- `src/files.cot` — no registry interaction
- `src/package.cot` — pure data structs

---

## Phase E: Data Migration & Verification

### Startup Migration

If `data/registry.json` exists when the server starts, load its contents into SQLite, then rename it to `data/registry.json.bak`. Add this to `Registry.init()`:

```cot
static fn init(db_path: string) Registry {
    if (Db.open(db_path)) |db| {
        var reg = Registry { db: db }

        // One-time migration from JSON file
        reg.migrateFromJson()

        infoKv("opened registry database", "path", db_path)
        return reg
    }
    // ... error handling
}

fn migrateFromJson() void {
    var json_path = "data/registry.json"
    if (not fileExists(json_path)) { return }

    info("migrating data/registry.json to SQLite...")
    var content = readFile(json_path) catch { return }
    var root = parse(content)
    if (root == 0) { return }
    if (jsonTag(root) != JSON_OBJECT) {
        jsonFree(root)
        return
    }

    var keys = jsonObjectKeys(root)
    var migrated: int = 0
    var i: int = 0
    while (i < keys.count) {
        var name = keys.get(i)
        var pkg_json = jsonObjectGet(root, name)
        if (pkg_json != 0 and jsonTag(pkg_json) == JSON_OBJECT) {
            var desc = jsonObjectGetString(pkg_json, "description")
            var author = jsonObjectGetString(pkg_json, "author")
            self.db.createPackage(name, desc, author)

            var versions_json = jsonObjectGet(pkg_json, "versions")
            if (versions_json != 0 and jsonTag(versions_json) == JSON_OBJECT) {
                var ver_keys = jsonObjectKeys(versions_json)
                var j: int = 0
                while (j < ver_keys.count) {
                    var ver_num = ver_keys.get(j)
                    var ver_json = jsonObjectGet(versions_json, ver_num)
                    if (ver_json != 0) {
                        var deps: List(Dependency) = .{}
                        var deps_json = jsonObjectGet(ver_json, "dependencies")
                        if (deps_json != 0 and jsonTag(deps_json) == JSON_OBJECT) {
                            var dep_keys = jsonObjectKeys(deps_json)
                            var k: int = 0
                            while (k < dep_keys.count) {
                                var dep_name = dep_keys.get(k)
                                var dep_ver = jsonObjectGetString(deps_json, dep_name)
                                deps.append(Dependency { name: dep_name, version_req: dep_ver })
                                k += 1
                            }
                        }
                        var ver = Version {
                            number: ver_num,
                            published: jsonObjectGetString(ver_json, "published"),
                            checksum: jsonObjectGetString(ver_json, "checksum"),
                            dependencies: deps,
                        }
                        self.db.addVersion(name, ver)
                    }
                    j += 1
                }
            }
            migrated += 1
        }
        i += 1
    }
    jsonFree(root)

    // Rename JSON file to .bak
    // Note: Cot doesn't have rename() yet, so we copy and delete
    writeFile("data/registry.json.bak", content) catch {}
    deleteFile(json_path) catch {}

    infoKv("migration complete", "packages", intToString(migrated))
}
```

**Important**: The migration function needs `import "std/json"` and `import "std/fs"` in `registry.cot` temporarily for the migration code. These can be removed once migration is no longer needed.

Alternatively, write the migration as a separate one-shot script (`src/migrate.cot`) to keep `registry.cot` clean.

### Verification Checklist

#### 1. Compiler changes (Phase A)

```bash
cd ~/cotlang/cot
zig build test  # Run project.zig tests including new getLibs tests
```

#### 2. Stdlib sqlite.cot (Phase B)

```bash
cd ~/cot-land/pkg
# Verify sqlite.cot type-checks (import it from a test file)
echo 'import "std/sqlite"
test "sqlite open close" {
    var db = sqliteOpen(":memory:")
    @assert(db != 0)
    sqliteExec(db, "CREATE TABLE t (id INTEGER)")
    sqliteClose(db)
}' > /tmp/test_sqlite.cot
cot test /tmp/test_sqlite.cot
```

#### 3. db.cot (Phase C)

```bash
# Add tests to db.cot (at bottom of file)
cot test src/db.cot
```

Suggested tests for `db.cot`:

```cot
test "create and retrieve package" {
    if (Db.open(":memory:")) |db| {
        @assert(db.createPackage("json", "JSON parser", "alice"))
        @assert(db.has("json"))
        if (db.getPackage("json")) |pkg| {
            @assertEq(pkg.name, "json")
            @assertEq(pkg.description, "JSON parser")
        }
        db.close()
    }
}

test "duplicate package rejected" {
    if (Db.open(":memory:")) |db| {
        @assert(db.createPackage("foo", "desc", "bob"))
        @assert(not db.createPackage("foo", "other", "bob"))
        db.close()
    }
}

test "add version with dependencies" {
    if (Db.open(":memory:")) |db| {
        db.createPackage("my-lib", "A library", "alice")
        var deps: List(Dependency) = .{}
        deps.append(Dependency { name: "json", version_req: "^1.0.0" })
        var ver = Version {
            number: "1.0.0",
            published: "2026-03-01T00:00:00Z",
            checksum: "sha256:abc",
            dependencies: deps,
        }
        @assert(db.addVersion("my-lib", ver))
        @assert(not db.addVersion("my-lib", ver))  // duplicate

        if (db.getPackage("my-lib")) |pkg| {
            @assertEq(pkg.versions.count, 1)
            @assertEq(pkg.versions.get(0).dependencies.count, 1)
            @assertEq(pkg.versions.get(0).dependencies.get(0).name, "json")
        }
        db.close()
    }
}
```

#### 4. Full type-check (Phase D)

```bash
cd ~/cot-land/pkg
cot check src/main.cot    # Must pass with zero errors
```

#### 5. Existing tests still pass

```bash
cot test src/registry.cot
cot test src/package.cot
cot test src/search_index.cot
cot test src/api_packages.cot
cot test src/api_versions.cot
```

Note: Registry tests need updating since they construct `Registry` directly. The test setup changes from:
```cot
var packages: Map(string, Package) = .{}
var reg = Registry { packages: packages, data_path: "/tmp/test.json" }
```
to:
```cot
var reg = Registry.init(":memory:")
```

Using `:memory:` for tests means no disk I/O and automatic cleanup.

#### 6. Build and run (if register allocator bug is fixed)

```bash
cot build src/main.cot -o pkg
./pkg
# In another terminal:
curl http://localhost:8080/api/packages
curl -X POST -H "Authorization: Bearer test" \
     -d '{"name":"hello","description":"test","author":"me"}' \
     http://localhost:8080/api/packages
curl http://localhost:8080/api/packages/hello
```

---

## Implementation Order

1. **Phase A** first — compiler must support `"libs"` before anything else compiles
2. **Phase B** next — stdlib sqlite.cot, verified with `:memory:` test
3. **Phase C** — db.cot, verified with unit tests using `:memory:`
4. **Phase D** — swap registry.cot, update main.cot and cot.json
5. **Phase E** — migration code (only needed if registry.json has real data)

Each phase can be verified independently before proceeding to the next.

---

## Appendix: Known Blockers

### Compiler register allocator crash

`cot build src/main.cot` currently crashes in `codegen.native.regalloc.liveness.computeLiveness` when the project exceeds ~883 virtual registers. This blocks building the server binary. The crash is unrelated to SQLite and existed before this migration.

**Workaround**: `cot check` (type-checking) works fine. All module-level `cot test` commands work. Only the final binary link fails.

**Resolution**: Fix the register allocator or reduce function sizes that produce too many virtual registers.

### rename() not in stdlib

Cot doesn't have a `rename()` syscall wrapper. The migration code works around this by writing a `.bak` copy and deleting the original. If `rename()` is added to stdlib, the migration can be simplified.

### String concatenation in SQL

The `searchFallback` method uses `"%" + query + "%"` for LIKE patterns. This relies on Cot's `+` operator for string concatenation. If this doesn't work, use `string_concat` from `std/string` or build the pattern manually.
