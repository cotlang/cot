//! Frontend pipeline runner for the LSP.
//! Runs Source -> Scanner -> Parser -> Checker and collects diagnostics.
//! Supports cross-file analysis via analyzeWithImports() for import resolution.

const std = @import("std");
const source = @import("../frontend/source.zig");
const scanner_mod = @import("../frontend/scanner.zig");
const ast_mod = @import("../frontend/ast.zig");
const parser_mod = @import("../frontend/parser.zig");
const checker_mod = @import("../frontend/checker.zig");
const types_mod = @import("../frontend/types.zig");
const errors_mod = @import("../frontend/errors.zig");

const Source = source.Source;
const Scanner = scanner_mod.Scanner;
const Ast = ast_mod.Ast;
const Parser = parser_mod.Parser;
const Checker = checker_mod.Checker;
const Scope = checker_mod.Scope;
const SharedGenericContext = checker_mod.SharedGenericContext;
const TypeRegistry = types_mod.TypeRegistry;
const TypeIndex = types_mod.TypeIndex;
const NodeIndex = ast_mod.NodeIndex;
const Error = errors_mod.Error;
const ErrorReporter = errors_mod.ErrorReporter;

/// Module-level error list for collecting errors via ErrorHandler callback.
/// Safe because the LSP server is single-threaded.
var error_list: std.ArrayListUnmanaged(Error) = .{};
var error_allocator: std.mem.Allocator = undefined;

fn collectError(err: Error) void {
    // Dupe the message so it survives beyond the reporter's lifetime
    const msg_dupe = error_allocator.dupe(u8, err.msg) catch return;
    error_list.append(error_allocator, .{
        .span = err.span,
        .msg = msg_dupe,
        .err_code = err.err_code,
    }) catch {};
}

/// A parsed dependency file (imported by the main file).
pub const DepFile = struct {
    uri: []const u8, // "file:///path/to/stdlib/list.cot"
    path: []const u8, // filesystem path
    src: Source,
    tree: Ast,
};

/// Tracks which dependency file defined a symbol.
pub const SymbolOrigin = struct {
    dep_idx: usize, // index into dep_files
};

pub const AnalysisResult = struct {
    arena: std.heap.ArenaAllocator,
    src: Source,
    tree: Ast,
    type_reg: TypeRegistry,
    global_scope: Scope,
    generics: SharedGenericContext,
    checker: ?Checker,
    errors: []const Error,
    parse_ok: bool,
    dep_files: []DepFile,
    symbol_origins: std.StringHashMap(SymbolOrigin),

    pub fn deinit(self: *AnalysisResult) void {
        const alloc = self.arena.allocator();
        // Free duped error messages (we own these from collectError)
        for (self.errors) |err| {
            alloc.free(@constCast(err.msg));
        }
        if (self.errors.len > 0) alloc.free(@constCast(self.errors));

        if (self.checker) |*c| c.deinit();
        self.generics.deinit(self.arena.allocator());
        self.global_scope.deinit();
        self.type_reg.deinit();

        // Clean up dependency files
        for (self.dep_files) |*dep| {
            var d = dep.*;
            d.tree.deinit();
            d.src.deinit();
        }
        if (self.dep_files.len > 0) alloc.free(self.dep_files);

        self.symbol_origins.deinit();
        self.tree.deinit();
        self.src.deinit();
        self.arena.deinit();
    }
};

fn makeEmptyResult(arena: std.heap.ArenaAllocator, allocator: std.mem.Allocator, src_val: Source, tree_val: Ast, type_reg_val: anytype, parse_ok_val: bool) AnalysisResult {
    const errs = error_list.toOwnedSlice(allocator) catch &.{};
    return AnalysisResult{
        .arena = arena,
        .src = src_val,
        .tree = tree_val,
        .type_reg = type_reg_val,
        .global_scope = Scope.init(allocator, null),
        .generics = SharedGenericContext.init(allocator),
        .checker = null,
        .errors = errs,
        .parse_ok = parse_ok_val,
        .dep_files = &.{},
        .symbol_origins = std.StringHashMap(SymbolOrigin).init(allocator),
    };
}

/// Run the frontend pipeline on the given source text and collect diagnostics.
/// Single-file analysis (no import resolution).
pub fn analyze(parent_allocator: std.mem.Allocator, text: []const u8, filename: []const u8) ?AnalysisResult {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    const allocator = arena.allocator();

    // Reset module-level error collection
    error_allocator = allocator;
    error_list = .{};

    // Dupe text into arena
    const content = allocator.dupe(u8, text) catch {
        arena.deinit();
        return null;
    };
    const fname = allocator.dupe(u8, filename) catch {
        arena.deinit();
        return null;
    };

    var src = Source.init(allocator, fname, content);

    // Scanner + Parser
    var err_reporter = ErrorReporter.init(&src, collectError);
    var scan = Scanner.initWithErrors(&src, &err_reporter);
    var tree = Ast.init(allocator);
    var parser = Parser.init(allocator, &scan, &tree, &err_reporter);
    parser.parseFile() catch {};

    const parse_ok = !err_reporter.hasErrors();

    // Type checking (only if parse succeeded)
    var type_reg = TypeRegistry.init(allocator) catch {
        return makeEmptyResult(arena, allocator, src, tree, undefined, false);
    };
    var global_scope = Scope.init(allocator, null);
    var generics = SharedGenericContext.init(allocator);

    var checker: ?Checker = null;
    if (parse_ok) {
        const target = @import("../frontend/target.zig").Target.native();
        var c = Checker.init(allocator, &tree, &type_reg, &err_reporter, &global_scope, &global_scope, &generics, target);
        c.checkFile() catch {};
        checker = c;
    }

    const errs = error_list.toOwnedSlice(allocator) catch &.{};

    return AnalysisResult{
        .arena = arena,
        .src = src,
        .tree = tree,
        .type_reg = type_reg,
        .global_scope = global_scope,
        .generics = generics,
        .checker = checker,
        .errors = errs,
        .parse_ok = parse_ok,
        .dep_files = &.{},
        .symbol_origins = std.StringHashMap(SymbolOrigin).init(allocator),
    };
}

/// Run the frontend pipeline with cross-file import resolution.
/// Copies the driver.zig two-phase pattern: parse imports recursively, then
/// type-check all files with a shared scope.
pub fn analyzeWithImports(parent_allocator: std.mem.Allocator, text: []const u8, filename: []const u8) ?AnalysisResult {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    const allocator = arena.allocator();

    // Reset module-level error collection
    error_allocator = allocator;
    error_list = .{};

    // Dupe text into arena
    const content = allocator.dupe(u8, text) catch {
        arena.deinit();
        return null;
    };
    const fname = allocator.dupe(u8, filename) catch {
        arena.deinit();
        return null;
    };

    var src = Source.init(allocator, fname, content);

    // Scanner + Parser for main file
    var err_reporter = ErrorReporter.init(&src, collectError);
    var scan = Scanner.initWithErrors(&src, &err_reporter);
    var tree = Ast.init(allocator);
    var parser = Parser.init(allocator, &scan, &tree, &err_reporter);
    parser.parseFile() catch {};

    const parse_ok = !err_reporter.hasErrors();

    // Type checking setup
    var type_reg = TypeRegistry.init(allocator) catch {
        return makeEmptyResult(arena, allocator, src, tree, undefined, false);
    };
    var global_scope = Scope.init(allocator, null);
    var generics = SharedGenericContext.init(allocator);
    var symbol_origins = std.StringHashMap(SymbolOrigin).init(allocator);
    const target = @import("../frontend/target.zig").Target.native();

    // Phase 1: Parse imports recursively (dependencies before main file)
    var dep_list = std.ArrayListUnmanaged(DepFile){};
    var seen_files = std.StringHashMap(void).init(allocator);
    defer seen_files.deinit();

    // Mark the main file as seen
    const main_canonical = normalizePath(allocator, fname) catch fname;
    seen_files.put(main_canonical, {}) catch {};

    if (parse_ok) {
        // Resolve and parse imports
        const imports = tree.getImports(allocator) catch &.{};
        const file_dir = std.fs.path.dirname(fname) orelse ".";

        for (imports) |import_path| {
            const full_path = resolveImportPath(allocator, import_path, file_dir) catch continue;
            parseDepRecursive(allocator, full_path, &dep_list, &seen_files) catch {};
        }
    }

    var dep_files: []DepFile = dep_list.toOwnedSlice(allocator) catch allocator.alloc(DepFile, 0) catch &.{};

    // Phase 2: Type-check deps with shared scope, then main file
    // Check each dependency (errors are ignored — they're not the user's file)
    // Track known symbols to detect which symbols each dep adds.
    var known_symbols = std.StringHashMap(void).init(allocator);
    defer known_symbols.deinit();

    for (0..dep_files.len) |dep_idx| {
        // Snapshot current symbol names before checking this dep
        known_symbols.clearRetainingCapacity();
        {
            var iter = global_scope.symbols.iterator();
            while (iter.next()) |entry| {
                known_symbols.put(entry.key_ptr.*, {}) catch {};
            }
        }

        var dep_err_reporter = ErrorReporter.init(&dep_files[dep_idx].src, null); // discard dep errors
        var dep_checker = Checker.init(allocator, &dep_files[dep_idx].tree, &type_reg, &dep_err_reporter, &global_scope, &global_scope, &generics, target);
        dep_checker.checkFile() catch {};
        dep_checker.deinit();

        // Find newly added symbols (in global_scope but not in known_symbols)
        var scope_iter = global_scope.symbols.iterator();
        while (scope_iter.next()) |entry| {
            if (!known_symbols.contains(entry.key_ptr.*)) {
                symbol_origins.put(entry.key_ptr.*, .{ .dep_idx = dep_idx }) catch {};
            }
        }
    }

    // Check main file with the shared scope (errors collected for diagnostics)
    var checker: ?Checker = null;
    if (parse_ok) {
        var c = Checker.init(allocator, &tree, &type_reg, &err_reporter, &global_scope, &global_scope, &generics, target);
        c.checkFile() catch {};
        checker = c;
    }

    const errs = error_list.toOwnedSlice(allocator) catch &.{};

    return AnalysisResult{
        .arena = arena,
        .src = src,
        .tree = tree,
        .type_reg = type_reg,
        .global_scope = global_scope,
        .generics = generics,
        .checker = checker,
        .errors = errs,
        .parse_ok = parse_ok,
        .dep_files = dep_files,
        .symbol_origins = symbol_origins,
    };
}

/// Recursively parse a dependency file and its imports.
/// Dependencies are added before the files that import them (depth-first).
fn parseDepRecursive(
    allocator: std.mem.Allocator,
    path: []const u8,
    dep_list: *std.ArrayListUnmanaged(DepFile),
    seen_files: *std.StringHashMap(void),
) !void {
    const canonical = normalizePath(allocator, path) catch try allocator.dupe(u8, path);

    if (seen_files.contains(canonical)) return;
    try seen_files.put(canonical, {});

    // Read the file
    const dep_text = std.fs.cwd().readFileAlloc(allocator, canonical, 1024 * 1024) catch return;

    var dep_src = Source.init(allocator, canonical, dep_text);
    var dep_err = ErrorReporter.init(&dep_src, null); // discard parse errors for deps
    var dep_scan = Scanner.initWithErrors(&dep_src, &dep_err);
    var dep_tree = Ast.init(allocator);
    var dep_parser = Parser.init(allocator, &dep_scan, &dep_tree, &dep_err);
    dep_parser.parseFile() catch return;

    if (dep_err.hasErrors()) return;

    // Recurse into this dep's imports first (depth-first)
    const dep_imports = dep_tree.getImports(allocator) catch &.{};
    const dep_dir = std.fs.path.dirname(canonical) orelse ".";
    for (dep_imports) |import_path| {
        const full_path = resolveImportPath(allocator, import_path, dep_dir) catch continue;
        try parseDepRecursive(allocator, full_path, dep_list, seen_files);
    }

    // Build file:// URI for this dep
    const uri = std.fmt.allocPrint(allocator, "file://{s}", .{canonical}) catch canonical;

    try dep_list.append(allocator, .{
        .uri = uri,
        .path = canonical,
        .src = dep_src,
        .tree = dep_tree,
    });
}

/// Resolve an import path to a filesystem path.
/// "std/list" → <stdlib_dir>/list.cot
/// "foo.cot"  → <source_dir>/foo.cot
fn resolveImportPath(allocator: std.mem.Allocator, import_path: []const u8, source_dir: []const u8) ![]const u8 {
    if (std.mem.startsWith(u8, import_path, "std/")) {
        return resolveStdImport(allocator, import_path, source_dir);
    }
    return std.fs.path.join(allocator, &.{ source_dir, import_path });
}

/// Resolve stdlib imports: "std/list" -> "<stdlib_dir>/list.cot"
/// Discovery order (copied from driver.zig:319-342):
/// 1. COT_STDLIB env var (explicit override)
/// 2. Walk up from source file looking for stdlib/ directory
/// 3. CWD fallback (./stdlib/)
fn resolveStdImport(allocator: std.mem.Allocator, import_path: []const u8, source_dir: []const u8) ![]const u8 {
    const module = import_path["std/".len..];
    const filename = try std.fmt.allocPrint(allocator, "{s}.cot", .{module});
    defer allocator.free(filename);

    // Tier 1: COT_STDLIB env var
    if (std.posix.getenv("COT_STDLIB")) |stdlib_dir| {
        return std.fs.path.join(allocator, &.{ stdlib_dir, filename });
    }

    // Tier 2: Walk up from source directory looking for stdlib/
    var dir: []const u8 = source_dir;
    while (true) {
        const candidate = try std.fs.path.join(allocator, &.{ dir, "stdlib", filename });
        if (std.fs.cwd().access(candidate, .{})) |_| {
            return candidate;
        } else |_| {}
        allocator.free(candidate);
        dir = std.fs.path.dirname(dir) orelse break;
    }

    // Tier 3: CWD fallback
    return std.fs.path.join(allocator, &.{ "stdlib", filename });
}

/// Normalize a path to its canonical (real) form.
fn normalizePath(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return std.fs.cwd().realpathAlloc(allocator, path) catch try allocator.dupe(u8, path);
}
