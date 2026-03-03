# Implementation: `c_sources` Field for Bundling C Code

## Goal

Allow `cot.json` to specify C source files that get compiled and linked into the final binary. This eliminates dynamic library dependencies and enables cross-compilation for packages that wrap C libraries (SQLite, libpq, etc.).

```json
{
  "name": "my-project",
  "c_sources": ["vendor/sqlite3.c"],
  "c_flags": ["-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_THREADSAFE=0"]
}
```

## Why

Currently `"libs": ["sqlite3"]` passes `-lsqlite3` to the linker, which requires the system to have `libsqlite3` installed. This breaks cross-compilation (`--target=x86_64-linux` from macOS) because the linker can't find the linux version of the library.

Since the linker is already `zig cc` (a full C compiler + linker), we can just pass C source files directly. Zig compiles them for the target architecture and links them in. Zero system dependencies.

## Current Architecture

### cot.json parsing: `compiler/project.zig`

`LoadedConfig.getLibs()` (lines ~88-105) reads the `"libs"` array from cot.json and returns `?[]const []const u8`.

```zig
pub fn getLibs(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
    // Parses JSON, extracts "libs" array, returns slice of strings
}
```

### Linker invocation: `compiler/main.zig`

`compileAndLinkFull()` (lines ~1405-1479) builds a `zig cc` command:

```
zig cc -o <output> <object.o> [platform flags] [-l<lib> ...]
```

The libs are appended at lines ~1451-1459:

```zig
if (project_config) |*pc| {
    if (pc.getLibs(allocator)) |libs| {
        for (libs) |lib_name| {
            const flag = std.fmt.allocPrint(allocator, "-l{s}", .{lib_name}) catch continue;
            link_args.append(allocator, flag) catch {};
        }
    }
}
```

## Changes Required

### 1. `compiler/project.zig` — Add `getCsources()` and `getCflags()`

Add two new methods on `LoadedConfig`, identical pattern to `getLibs()`:

```zig
/// Read "c_sources" array from cot.json.
/// Returns paths like ["vendor/sqlite3.c"].
pub fn getCsources(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
    if (self.file_contents.len == 0) return null;
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
    defer parsed.deinit();
    const root = parsed.value;
    if (root != .object) return null;
    const sources = root.object.get("c_sources") orelse return null;
    if (sources != .array) return null;
    var paths = std.ArrayListUnmanaged([]const u8){};
    for (sources.array.items) |item| {
        if (item != .string) continue;
        paths.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
    }
    if (paths.items.len == 0) return null;
    return paths.toOwnedSlice(allocator) catch null;
}

/// Read "c_flags" array from cot.json.
/// Returns flags like ["-DSQLITE_THREADSAFE=0"].
pub fn getCflags(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
    // Same pattern as above, reading "c_flags" key
}
```

Add tests following the existing pattern (line ~192):

```zig
test "getCsources" {
    var config = LoadedConfig{
        .file_contents =
            \\{"name":"test","c_sources":["vendor/sqlite3.c","vendor/foo.c"]}
        ,
    };
    const sources = config.getCsources(std.testing.allocator).?;
    defer std.testing.allocator.free(sources);
    try std.testing.expectEqual(@as(usize, 2), sources.len);
    try std.testing.expectEqualStrings("vendor/sqlite3.c", sources[0]);
    try std.testing.expectEqualStrings("vendor/foo.c", sources[1]);
}
```

### 2. `compiler/main.zig` — Pass C sources and flags to `zig cc`

In `compileAndLinkFull()`, after the existing libs loop (~line 1459), add:

```zig
// C flags (must come before C source files)
if (project_config) |*pc| {
    if (pc.getCflags(allocator)) |c_flags| {
        for (c_flags) |flag| {
            link_args.append(allocator, flag) catch {};
        }
    }
}

// C source files — zig cc compiles these for the target and links them in
if (project_config) |*pc| {
    if (pc.getCsources(allocator)) |c_files| {
        for (c_files) |c_file| {
            // Resolve relative to project root (where cot.json lives)
            const resolved = if (pc.project_dir) |dir|
                std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir, c_file }) catch continue
            else
                c_file;
            link_args.append(allocator, resolved) catch {};
        }
    }
}
```

**Important:** C source files must be resolved relative to the project directory (where `cot.json` lives), not the current working directory. Check how `pc.project_dir` or equivalent is tracked — `getLibs()` doesn't need this because `-l` flags are resolved by the system linker, but file paths need to be absolute or relative to the right directory.

The resulting `zig cc` command becomes:

```
zig cc -o pkg output.o -lSystem -DSQLITE_THREADSAFE=0 vendor/sqlite3.c
```

When cross-compiling:

```
zig cc -o pkg output.o -target x86_64-linux-gnu -lc -lpthread -DSQLITE_THREADSAFE=0 vendor/sqlite3.c
```

### 3. `stdlib/sqlite.cot` — No changes needed

The `extern fn` declarations stay exactly the same. The only difference is the symbols come from the compiled `sqlite3.c` instead of a dynamic library.

### 4. Migrate cot.land's `cot.json`

Before:
```json
{
  "name": "pkg",
  "version": "0.1.0",
  "main": "src/main.cot",
  "safe": true,
  "libs": ["sqlite3"]
}
```

After:
```json
{
  "name": "pkg",
  "version": "0.1.0",
  "main": "src/main.cot",
  "safe": true,
  "c_sources": ["vendor/sqlite3.c"],
  "c_flags": ["-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_THREADSAFE=0"]
}
```

And place the SQLite amalgamation at `~/cot-land/pkg/vendor/sqlite3.c`. Download from https://sqlite.org/download.html (the amalgamation tarball — single `sqlite3.c` + `sqlite3.h`).

## Testing

1. **Unit test** — `getCsources()` and `getCflags()` parsing in `project.zig`
2. **Build test** — Create a minimal project with `c_sources` pointing to a trivial C file:
   ```c
   // test.c
   int add_c(int a, int b) { return a + b; }
   ```
   ```cot
   extern fn add_c(a: i64, b: i64) i64
   test "c source linking" {
       @assertEq(add_c(2, 3), 5)
   }
   ```
3. **Cross-compile test** — `cot build --target=x86_64-linux` with `c_sources` should produce a linux binary without needing system libraries
4. **cot.land build** — `cot build src/main.cot -o pkg` with the migrated cot.json should produce a working binary that no longer requires `libsqlite3` on the system
5. **cot.land cross-compile** — `cot build src/main.cot -o pkg --target=x86_64-linux` should succeed (currently fails with "unable to find dynamic system library 'sqlite3'")

## Scope

This is a ~50 line change across two files:
- `compiler/project.zig`: ~25 lines (two new methods + tests)
- `compiler/main.zig`: ~15 lines (append c_flags and c_sources to link_args)

No changes to the type checker, IR, codegen, or any other compiler phase. The C compilation is handled entirely by `zig cc` which is already the linker.

## Future: Package-Level `c_sources`

Once the package registry supports this field, registry packages can bundle C source:

```
~/.cache/cot/packages/sqlite/1.0.0/
  cot.json          {"c_sources": ["sqlite3.c"], "c_flags": [...]}
  sqlite.cot        (extern fn declarations + Cot wrappers)
  sqlite3.c         (amalgamation)
  sqlite3.h         (header)
```

When a project does `cot add sqlite`, the resolver downloads the package. At build time, the compiler reads each dependency's `cot.json`, collects all `c_sources` paths (resolved relative to the package directory), and passes them all to `zig cc`. This requires the compiler to walk the dependency tree's `cot.json` files — a separate task from this implementation.

For now, `c_sources` only works for the top-level project's `cot.json`.
