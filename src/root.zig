//! Cot - Commercial Oriented Transactions
//!
//! A modern language for business software.
//! Designed for migrating and modernizing legacy business applications.
//!
//! This library provides:
//! - Lexer: Tokenizes Cot (.cot) source code
//! - Parser: Builds an AST from tokens (using SoA NodeStore)
//! - AST: Abstract Syntax Tree types (SoA-based with StringInterner)
//! - IR: Intermediate Representation for multiple backends
//! - Runtime: Interpreter and execution environment
//! - CotDB: ISAM database engine
//! - Builtins: Built-in functions
//! - Bytecode: Bytecode compiler and VM
//! - Native: Native function registry
//!
//! For DBL syntax (.dbl files), use the cot-dbl frontend package.
//!
//! ## AST
//!
//! The AST uses Structure-of-Arrays (SoA) layout with StringInterner for
//! efficient string deduplication:
//!
//! - `ast.NodeStore` - Central SoA storage for all AST nodes
//! - `ast.StmtIdx`, `ast.ExprIdx`, `ast.TypeIdx` - Typed indices
//! - `ast.StringInterner`, `ast.StringId` - String deduplication

const std = @import("std");

// Cot Runtime - bytecode VM, native functions, ISAM backends
const cot_runtime = @import("cot_runtime");

// Base utilities
pub const base = @import("base/mod.zig");

// Core compiler modules
pub const lexer = @import("lexer/lexer.zig");
pub const token = @import("lexer/token.zig");
pub const parser = @import("parser/parser.zig");
// AST module - SoA-based AST
pub const ast = @import("ast/mod.zig");
pub const compiler = @import("compiler/compiler.zig");

// Runtime is provided by cot-runtime package

// Bytecode compiler and VM (from cot_runtime)
pub const bytecode = cot_runtime.bytecode;

// Compile-time evaluation (uses NodeStore-based AST)
pub const comptime_eval = @import("comptime/comptime.zig");
pub const comptime_value = @import("comptime/value.zig");
pub const comptime_builtins = @import("comptime/builtins.zig");

// Intermediate Representation
pub const ir = @import("ir/ir.zig");
pub const ir_lower = @import("ir/lower.zig");
pub const ir_optimize = @import("ir/optimize.zig");
pub const ir_printer = @import("ir/printer.zig");
pub const ir_emit_bytecode = @import("ir/emit_bytecode.zig");

// Semantic analysis types for LSP
pub const SemanticDiagnostic = ir_lower.SemanticDiagnostic;
pub const analyzeSemantics = ir_lower.analyzeSemantics;

// Build logging system
pub const log = @import("log.zig");

// Schema repository
pub const schema = @import("schema/schema.zig");

// Framework (project configuration, commands, workspace management)
pub const framework = @import("framework/mod.zig");

// Native function registry (from cot_runtime)
pub const native = cot_runtime.native;

// CotDB - ISAM database engine (from cotdb package)
pub const cotdb = @import("cotdb");
pub const isam = cotdb.isam;

// Debug utilities (from cot_runtime)
pub const debug = cot_runtime.debug;

// Testing utilities
pub const testing_utils = @import("testing/mod.zig");

// Code formatter (shared between CLI and LSP)
pub const formatter = @import("formatter/mod.zig");

// Crash handling (from cot_runtime)
pub const crash = cot_runtime.crash;

// Extension system (from cot_runtime)
pub const extension = cot_runtime.extension;

/// Library version
pub const version = "0.1.0";

// ANSI color codes for terminal output
const Color = struct {
    const reset = "\x1b[0m";
    const bold = "\x1b[1m";
    const red = "\x1b[31m";
    const blue = "\x1b[34m";
    const cyan = "\x1b[36m";
};

/// Format a parse error with source context for better developer UX
pub fn formatParseError(source: []const u8, line: usize, column: usize, message: []const u8) void {
    // Header: error at line:column
    std.debug.print("{s}{s}error{s}: {s}{s}{s}\n", .{
        Color.bold,
        Color.red,
        Color.reset,
        Color.bold,
        message,
        Color.reset,
    });

    // Find the source line
    var current_line: usize = 1;
    var line_start: usize = 0;
    var line_end: usize = 0;

    for (source, 0..) |c, i| {
        if (current_line == line) {
            line_start = i;
            // Find end of line
            line_end = i;
            while (line_end < source.len and source[line_end] != '\n') {
                line_end += 1;
            }
            break;
        }
        if (c == '\n') {
            current_line += 1;
        }
    }

    if (current_line == line and line_end > line_start) {
        const source_line = source[line_start..line_end];

        // Line number and source
        std.debug.print("{s}{d:>5} |{s} {s}\n", .{
            Color.blue,
            line,
            Color.reset,
            source_line,
        });

        // Pointer line
        std.debug.print("{s}      |{s} ", .{ Color.blue, Color.reset });

        // Spaces to column position
        var col: usize = 1;
        for (source_line) |c| {
            if (col >= column) break;
            if (c == '\t') {
                std.debug.print("    ", .{});
            } else {
                std.debug.print(" ", .{});
            }
            col += 1;
        }

        // Caret
        std.debug.print("{s}{s}^{s}\n", .{ Color.bold, Color.cyan, Color.reset });
    } else {
        // Fallback if we can't find the line
        std.debug.print("  --> line {d}, column {d}\n", .{ line, column });
    }

    std.debug.print("\n", .{});
}

/// Compile and execute Cot source code using bytecode/VM pipeline
pub fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    return runWithFile(allocator, source, null);
}

/// Run Cot source with optional source file path (for @file() builtin)
pub fn runWithFile(allocator: std.mem.Allocator, source: []const u8, source_file: ?[]const u8) !void {
    // Initialize extension registry
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Compile to bytecode module
    const module_name = if (source_file) |f| std.fs.path.stem(f) else "main";
    var module = try compileToModuleWithFile(allocator, source, module_name, source_file);
    defer module.deinit();

    // Execute in VM
    var vm = bytecode.VM.init(allocator);
    defer vm.deinit();

    try vm.execute(&module);
}

/// Compile Cot source to bytecode using the IR pipeline
pub fn compileToModule(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8) !bytecode.Module {
    return compileToModuleWithFile(allocator, source, module_name, null);
}

/// Compile Cot source to bytecode with optional source file path
pub fn compileToModuleWithFile(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8, source_file: ?[]const u8) !bytecode.Module {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse using NodeStore-based parser
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parse = parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();

    const top_level = parse.parse() catch {
        // Show collected errors with source context
        for (parse.errors.items) |parse_err| {
            formatParseError(source, parse_err.line, parse_err.column, parse_err.message);
        }
        return error.ParseError;
    };
    defer allocator.free(top_level);

    // Check for parser errors (multi-error collection)
    if (parse.hasErrors()) {
        for (parse.errors.items) |err| {
            formatParseError(source, err.line, err.column, err.message);
        }
        return error.ParseError;
    }

    // Lower to IR using detailed error reporting
    const lower_result = ir_lower.lowerWithDetails(allocator, &store, &strings, top_level, module_name, .{ .source_file = source_file });
    const ir_module = switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                std.debug.print("{d}:{d}: error: {s}\n", .{ detail.line, detail.column, detail.message });
                if (detail.context.len > 0) {
                    std.debug.print("  while {s}\n", .{detail.context});
                }
            } else {
                std.debug.print("IR lowering error: {}\n", .{e.kind});
            }
            return error.LowerError;
        },
    };
    defer allocator.destroy(ir_module);
    defer ir_module.deinit();

    // Emit bytecode from IR
    var emitter = ir_emit_bytecode.BytecodeEmitter.init(allocator);
    defer emitter.deinit();
    return try emitter.emit(ir_module);
}

/// Lower Cot source to IR (for debugging/inspection)
pub fn lowerToIR(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8) !*ir.Module {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse using NodeStore-based parser
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parse = parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();

    const top_level = parse.parse() catch {
        // Show collected errors with source context
        for (parse.errors.items) |parse_err| {
            formatParseError(source, parse_err.line, parse_err.column, parse_err.message);
        }
        return error.ParseError;
    };
    defer allocator.free(top_level);

    // Check for parser errors (multi-error collection)
    if (parse.hasErrors()) {
        for (parse.errors.items) |err| {
            formatParseError(source, err.line, err.column, err.message);
        }
        return error.ParseError;
    }

    // Lower to IR using detailed error reporting
    const lower_result = ir_lower.lowerWithDetails(allocator, &store, &strings, top_level, module_name, .{ .source_file = module_name });
    return switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                std.debug.print("{d}:{d}: error: {s}\n", .{ detail.line, detail.column, detail.message });
                if (detail.context.len > 0) {
                    std.debug.print("  while {s}\n", .{detail.context});
                }
            } else {
                std.debug.print("IR lowering error: {}\n", .{e.kind});
            }
            return e.kind;
        },
    };
}

test "library version" {
    try std.testing.expectEqualStrings("0.1.0", version);
}
