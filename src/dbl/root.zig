//! Cot-DBL Frontend
//!
//! A complete DBL syntax frontend for the Cot language.
//! Provides a dedicated lexer and parser that transform DBL source code into
//! the same AST used by cot core, enabling compilation through
//! the standard IR → Bytecode → VM pipeline.
//!
//! ## Architecture
//!
//! ```
//! DBL Source (.dbl)
//!     ↓
//! DBL Lexer (cot-dbl)  →  DBL Tokens
//!     ↓
//! DBL Parser (cot-dbl) →  cot.ast.NodeStore (StmtIdx[])
//!     ↓
//! IR Lowerer (cot)     →  cot.ir.Module
//!     ↓
//! Bytecode Emitter     →  cot.bytecode.Module
//!     ↓
//! VM (cot-runtime)     →  Execution
//! ```
//!
//! ## Usage
//!
//! ```zig
//! const dbl = @import("cot_dbl");
//!
//! // Compile and run DBL source
//! try dbl.run(allocator, source);
//!
//! // Or get tokens/AST for inspection
//! var lexer = dbl.Lexer.init(source);
//! const tokens = try lexer.tokenize(allocator);
//!
//! var strings = cot.base.StringInterner.init(allocator);
//! defer strings.deinit();
//! var store = cot.ast.NodeStore.init(allocator, &strings);
//! defer store.deinit();
//!
//! var parser = dbl.Parser.init(allocator, tokens, &store, &strings);
//! const stmts = try parser.parse();
//! ```

const std = @import("std");
const cot = @import("cot");
const cot_runtime = @import("cot_runtime");

// DBL runtime extension (Map type, NSPC_* functions)
const dbl_ext = cot_runtime.dbl_ext;

// DBL-specific modules - complete frontend
pub const Lexer = @import("lexer.zig").Lexer;
pub const Parser = @import("parser.zig").Parser;
pub const ParseError = @import("parser.zig").ParseError;

// DBL-specific token types (separate from cot core)
pub const Token = @import("token.zig").Token;
pub const TokenType = @import("token.zig").TokenType;

// Re-export cot AST types for convenience (shared with cot core)
pub const ast = cot.ast;
pub const base = cot.base;

/// Extension metadata
pub const Extension = struct {
    pub const name = "cot-dbl";
    pub const version = "0.1.0";
    pub const description = "DBL syntax frontend for Cot";

    /// File extensions handled by this frontend
    pub const file_extensions = [_][]const u8{ ".dbl", ".dbo" };
};

/// Check if a file should use DBL mode
pub fn shouldUseDblMode(filename: []const u8) bool {
    for (Extension.file_extensions) |ext| {
        if (std.mem.endsWith(u8, filename, ext)) {
            return true;
        }
    }
    return false;
}

/// Compile and run DBL source code
pub fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    // Initialize extension registry
    cot.extension.initRegistry(allocator);
    defer cot.extension.deinitRegistry();

    // Compile to bytecode module
    var module = try compileToModule(allocator, source, "main");
    defer module.deinit();

    // Execute in VM
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    // Load DBL extension (Map type, NSPC_* functions)
    try vm.loadExtension(dbl_ext.dbl_extension);

    try vm.initChannels();
    try vm.execute(&module);
}

/// Compile and run DBL source code with registry already initialized
/// Use this when the caller manages the extension registry
pub fn runWithRegistry(allocator: std.mem.Allocator, source: []const u8) !void {
    return runWithRegistryAndPath(allocator, source, null);
}

/// Compile and run DBL source code with base path for imports
pub fn runWithRegistryAndPath(allocator: std.mem.Allocator, source: []const u8, base_path: ?[]const u8) !void {
    // Compile to bytecode module with path for import resolution
    var module = try compileToModuleWithPath(allocator, source, "main", base_path);
    defer module.deinit();

    // Execute in VM
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    // Load DBL extension (Map type, NSPC_* functions)
    try vm.loadExtension(dbl_ext.dbl_extension);

    try vm.initChannels();
    try vm.execute(&module);
}

/// Compile DBL source to bytecode
/// Supports optional base_path for resolving imports relative to the source file
pub fn compileToModule(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8) !cot.bytecode.Module {
    return compileToModuleWithPath(allocator, source, module_name, null);
}

/// Compile DBL source to bytecode with a base path for resolving imports
pub fn compileToModuleWithPath(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8, base_path: ?[]const u8) !cot.bytecode.Module {
    // Preprocess imports - combine all source files
    const combined_source = try preprocessImports(allocator, source, base_path);
    defer if (combined_source.ptr != source.ptr) allocator.free(combined_source);

    // Tokenize with DBL lexer
    var lex = Lexer.init(combined_source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse using NodeStore-based parser
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parse = Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();

    const top_level = parse.parse() catch |err| {
        // Log parser errors if any
        for (parse.errors.items) |e| {
            std.debug.print("Parse error at line {d}: {s}\n", .{ e.token.line, e.message });
        }
        return err;
    };
    defer allocator.free(top_level);

    // Check for parser errors
    if (parse.hasErrors()) {
        for (parse.errors.items) |e| {
            std.debug.print("Parse error at line {d}: {s}\n", .{ e.token.line, e.message });
        }
        return error.ParseError;
    }

    // Lower to IR directly using NodeStore
    const ir_module = cot.ir_lower.lower(allocator, &store, &strings, top_level, module_name) catch |err| {
        std.debug.print("IR lowering error: {}\n", .{err});
        return error.LowerError;
    };
    defer allocator.destroy(ir_module);
    defer ir_module.deinit();

    // Emit bytecode using register mode (fully register-based VM)
    var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
    defer emitter.deinit();
    return try emitter.emit(ir_module);
}

/// Preprocess DBL source to handle import statements
/// Scans for `import "path/to/file.dbl"` lines and inlines them
fn preprocessImports(allocator: std.mem.Allocator, source: []const u8, base_path: ?[]const u8) ![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .empty;
    errdefer result.deinit(allocator);

    var imported = std.StringHashMap(void).init(allocator);
    defer imported.deinit();

    try processSourceWithImports(allocator, source, base_path, &result, &imported);

    // If nothing was imported, return original source to avoid allocation
    if (imported.count() == 0) {
        result.deinit(allocator);
        return source;
    }

    return try result.toOwnedSlice(allocator);
}

fn processSourceWithImports(
    allocator: std.mem.Allocator,
    source: []const u8,
    base_path: ?[]const u8,
    result: *std.ArrayListUnmanaged(u8),
    imported: *std.StringHashMap(void),
) !void {
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Check for import statement: import "path" or import 'path'
        if (std.mem.startsWith(u8, trimmed, "import ")) {
            const rest = std.mem.trim(u8, trimmed[7..], " \t");
            if (rest.len > 2 and (rest[0] == '"' or rest[0] == '\'')) {
                const quote = rest[0];
                if (std.mem.indexOfScalar(u8, rest[1..], quote)) |end_pos| {
                    const import_path = rest[1 .. end_pos + 1];

                    // Prevent duplicate imports
                    const gop = try imported.getOrPut(import_path);
                    if (!gop.found_existing) {
                        // Resolve path relative to base_path
                        const full_path = try resolveImportPath(allocator, import_path, base_path);
                        defer allocator.free(full_path);

                        // Read and recursively process the imported file
                        if (std.fs.cwd().openFile(full_path, .{})) |file| {
                            defer file.close();
                            const content = file.readToEndAlloc(allocator, 1024 * 1024 * 10) catch continue;
                            defer allocator.free(content);

                            // Get the directory of the imported file for nested imports
                            const import_dir = std.fs.path.dirname(full_path);
                            try processSourceWithImports(allocator, content, import_dir, result, imported);
                            try result.append(allocator, '\n');
                        } else |_| {
                            std.debug.print("Warning: Could not import '{s}'\n", .{full_path});
                        }
                    }
                    continue; // Skip the import line itself
                }
            }
        }

        // Not an import - append the line
        try result.appendSlice(allocator, line);
        try result.append(allocator, '\n');
    }
}

fn resolveImportPath(allocator: std.mem.Allocator, import_path: []const u8, base_path: ?[]const u8) ![]const u8 {
    if (std.fs.path.isAbsolute(import_path)) {
        return try allocator.dupe(u8, import_path);
    }

    if (base_path) |bp| {
        // Resolve relative to base path
        return try std.fs.path.join(allocator, &.{ bp, import_path });
    }

    // No base path, use as-is (relative to cwd)
    return try allocator.dupe(u8, import_path);
}

// ============================================================================
// Tests
// ============================================================================

test "extension metadata" {
    try std.testing.expectEqualStrings("cot-dbl", Extension.name);
}

test "dbl mode detection" {
    try std.testing.expect(shouldUseDblMode("program.dbl"));
    try std.testing.expect(shouldUseDblMode("legacy.dbo"));
    try std.testing.expect(!shouldUseDblMode("modern.cot"));
}

test "dbl lexer produces cot tokens" {
    const source = "PROC myproc\nEND";
    var lex = Lexer.init(source);
    const tokens = try lex.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.kw_proc, tokens[0].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
    try std.testing.expectEqual(TokenType.kw_end, tokens[2].type);
}
