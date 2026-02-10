//! Frontend pipeline runner for the LSP.
//! Runs Source -> Scanner -> Parser -> Checker and collects diagnostics.

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
        self.tree.deinit();
        self.src.deinit();
        self.arena.deinit();
    }
};

/// Run the frontend pipeline on the given source text and collect diagnostics.
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
        const errs = error_list.toOwnedSlice(allocator) catch &.{};
        return AnalysisResult{
            .arena = arena,
            .src = src,
            .tree = tree,
            .type_reg = undefined,
            .global_scope = Scope.init(allocator, null),
            .generics = SharedGenericContext.init(allocator),
            .checker = null,
            .errors = errs,
            .parse_ok = false,
        };
    };
    var global_scope = Scope.init(allocator, null);
    var generics = SharedGenericContext.init(allocator);

    var checker: ?Checker = null;
    if (parse_ok) {
        const target = @import("../core/target.zig").Target.native();
        var c = Checker.init(allocator, &tree, &type_reg, &err_reporter, &global_scope, &generics, target);
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
    };
}
