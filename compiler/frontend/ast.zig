//! Abstract Syntax Tree node definitions.

const std = @import("std");
const source = @import("source.zig");
const token = @import("token.zig");

const Span = source.Span;
const Pos = source.Pos;
const Token = token.Token;

pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const NodeList = []const NodeIndex;

pub const File = struct {
    filename: []const u8,
    decls: []const NodeIndex,
    span: Span,
    safe_mode: bool = false,
};

// Declarations

pub const Decl = union(enum) {
    fn_decl: FnDecl,
    var_decl: VarDecl,
    struct_decl: StructDecl,
    enum_decl: EnumDecl,
    union_decl: UnionDecl,
    type_alias: TypeAlias,
    import_decl: ImportDecl,
    impl_block: ImplBlock,
    trait_decl: TraitDecl,
    impl_trait: ImplTraitBlock,
    error_set_decl: ErrorSetDecl,
    test_decl: TestDecl,
    bad_decl: BadDecl,

    pub fn span(self: Decl) Span {
        return switch (self) { inline else => |d| d.span };
    }
};

// Rust: where T: Ord clause. Go 1.18: [T comparable] inline constraint.
// Cot uses Rust's where clause: `fn sort(T)(l: *List(T)) void where T: Ord`
pub const FnDecl = struct {
    name: []const u8,
    type_params: []const []const u8 = &.{},
    type_param_bounds: []const ?[]const u8 = &.{}, // parallel with type_params, null = unbounded
    params: []const Field,
    return_type: NodeIndex,
    body: NodeIndex,
    is_extern: bool,
    span: Span,
};
pub const VarDecl = struct { name: []const u8, type_expr: NodeIndex, value: NodeIndex, is_const: bool, span: Span };
pub const StructDecl = struct { name: []const u8, type_params: []const []const u8 = &.{}, fields: []const Field, span: Span };
pub const ImplBlock = struct { type_name: []const u8, type_params: []const []const u8 = &.{}, methods: []const NodeIndex, span: Span };
pub const TraitDecl = struct { name: []const u8, methods: []const NodeIndex, span: Span };
pub const ImplTraitBlock = struct { trait_name: []const u8, target_type: []const u8, type_params: []const []const u8 = &.{}, methods: []const NodeIndex, span: Span };
pub const TestDecl = struct { name: []const u8, body: NodeIndex, span: Span };
pub const EnumDecl = struct { name: []const u8, backing_type: NodeIndex, variants: []const EnumVariant, span: Span };
pub const UnionDecl = struct { name: []const u8, variants: []const UnionVariant, span: Span };
pub const TypeAlias = struct { name: []const u8, target: NodeIndex, span: Span };
pub const ImportDecl = struct { path: []const u8, span: Span };
pub const ErrorSetDecl = struct { name: []const u8, variants: []const []const u8, span: Span };
pub const BadDecl = struct { span: Span };

pub const Field = struct { name: []const u8, type_expr: NodeIndex, default_value: NodeIndex, span: Span };
pub const EnumVariant = struct { name: []const u8, value: NodeIndex, span: Span };
pub const UnionVariant = struct { name: []const u8, type_expr: NodeIndex, span: Span };

// Expressions

pub const Expr = union(enum) {
    ident: Ident,
    literal: Literal,
    binary: Binary,
    unary: Unary,
    call: Call,
    index: Index,
    slice_expr: SliceExpr,
    field_access: FieldAccess,
    array_literal: ArrayLiteral,
    paren: Paren,
    if_expr: IfExpr,
    switch_expr: SwitchExpr,
    block_expr: BlockExpr,
    struct_init: StructInit,
    new_expr: NewExpr,
    builtin_call: BuiltinCall,
    string_interp: StringInterp,
    type_expr: TypeExpr,
    try_expr: TryExpr,
    catch_expr: CatchExpr,
    error_literal: ErrorLiteral,
    closure_expr: ClosureExpr,
    tuple_literal: TupleLiteral,
    comptime_block: ComptimeBlock,
    zero_init: ZeroInit,
    addr_of: AddrOf,
    deref: Deref,
    bad_expr: BadExpr,

    pub fn span(self: Expr) Span {
        return switch (self) { inline else => |e| e.span };
    }
};

pub const Ident = struct { name: []const u8, span: Span };
pub const Literal = struct { kind: LiteralKind, value: []const u8, span: Span };
pub const LiteralKind = enum { int, float, string, char, true_lit, false_lit, null_lit, undefined_lit };
pub const Binary = struct { op: Token, left: NodeIndex, right: NodeIndex, span: Span };
pub const Unary = struct { op: Token, operand: NodeIndex, span: Span };
pub const Call = struct { callee: NodeIndex, args: []const NodeIndex, span: Span };
pub const Index = struct { base: NodeIndex, idx: NodeIndex, span: Span };
pub const SliceExpr = struct { base: NodeIndex, start: NodeIndex, end: NodeIndex, span: Span };
pub const FieldAccess = struct { base: NodeIndex, field: []const u8, span: Span };
pub const ArrayLiteral = struct { elements: []const NodeIndex, span: Span };
pub const Paren = struct { inner: NodeIndex, span: Span };
pub const IfExpr = struct { condition: NodeIndex, then_branch: NodeIndex, else_branch: NodeIndex, capture: []const u8 = "", span: Span };
pub const SwitchExpr = struct { subject: NodeIndex, cases: []const SwitchCase, else_body: NodeIndex, span: Span };
// Zig: switch supports ranges (1...10), captures, inline else.
// Rust: match supports nested patterns, guards (if expr), ranges (1..=10).
// Cot: extends switch with guards + ranges. Wildcards handled as else at parse time.
pub const SwitchCase = struct {
    patterns: []const NodeIndex,
    capture: []const u8,
    guard: NodeIndex = null_node, // Rust: `pattern if guard_expr =>`
    is_range: bool = false, // patterns[0] = start, patterns[1] = end (inclusive)
    body: NodeIndex,
    span: Span,
};
pub const BlockExpr = struct { stmts: []const NodeIndex, expr: NodeIndex, span: Span };
pub const StructInit = struct { type_name: []const u8, type_args: []const NodeIndex = &.{}, fields: []const FieldInit, span: Span };
pub const FieldInit = struct { name: []const u8, value: NodeIndex, span: Span };
/// Heap allocation expression: new Type { field: value, ... } or new Type(args...)
/// Reference: Go's walkNew (walk/builtin.go:601-616)
pub const NewExpr = struct {
    type_name: []const u8,
    type_args: []const NodeIndex = &.{},
    fields: []const FieldInit,
    /// Constructor args for `new Type(args...)` sugar — calls init() method
    constructor_args: []const NodeIndex = &.{},
    /// True when `()` syntax used instead of `{}`
    is_constructor: bool = false,
    span: Span,
};
pub const BuiltinKind = enum {
    // Type intrinsics
    size_of,
    align_of,
    // Casts
    int_cast,
    ptr_cast,
    int_to_ptr,
    ptr_to_int,
    // String construction
    string,
    // Assertions
    assert,
    assert_eq,
    // Memory
    alloc,
    dealloc,
    realloc,
    memcpy,
    // WASI file I/O
    fd_write,
    fd_read,
    fd_close,
    fd_seek,
    fd_open,
    // String decomposition
    ptr_of,
    len_of,
    // Process control
    trap,
    exit,
    // Time/random
    time,
    random,
    // CLI args
    args_count,
    arg_len,
    arg_ptr,
    // Environment
    environ_count,
    environ_len,
    environ_ptr,
    // Comptime targets
    target_os,
    target_arch,
    target,
    // Compile-time
    compile_error,
    // Math (float unary)
    abs,
    ceil,
    floor,
    trunc,
    round,
    sqrt,
    // Math (float binary)
    fmin,
    fmax,
    // Networking (TCP sockets)
    net_socket,
    net_bind,
    net_listen,
    net_accept,
    net_connect,
    net_set_reuse_addr,

    const map = std.StaticStringMap(BuiltinKind).initComptime(.{
        .{ "sizeOf", .size_of },
        .{ "alignOf", .align_of },
        .{ "intCast", .int_cast },
        .{ "ptrCast", .ptr_cast },
        .{ "intToPtr", .int_to_ptr },
        .{ "ptrToInt", .ptr_to_int },
        .{ "string", .string },
        .{ "assert", .assert },
        .{ "assert_eq", .assert_eq },
        .{ "alloc", .alloc },
        .{ "dealloc", .dealloc },
        .{ "realloc", .realloc },
        .{ "memcpy", .memcpy },
        .{ "fd_write", .fd_write },
        .{ "fd_read", .fd_read },
        .{ "fd_close", .fd_close },
        .{ "fd_seek", .fd_seek },
        .{ "fd_open", .fd_open },
        .{ "ptrOf", .ptr_of },
        .{ "lenOf", .len_of },
        .{ "trap", .trap },
        .{ "exit", .exit },
        .{ "time", .time },
        .{ "random", .random },
        .{ "args_count", .args_count },
        .{ "arg_len", .arg_len },
        .{ "arg_ptr", .arg_ptr },
        .{ "environ_count", .environ_count },
        .{ "environ_len", .environ_len },
        .{ "environ_ptr", .environ_ptr },
        .{ "target_os", .target_os },
        .{ "target_arch", .target_arch },
        .{ "target", .target },
        .{ "compileError", .compile_error },
        .{ "abs", .abs },
        .{ "ceil", .ceil },
        .{ "floor", .floor },
        .{ "trunc", .trunc },
        .{ "round", .round },
        .{ "sqrt", .sqrt },
        .{ "fmin", .fmin },
        .{ "fmax", .fmax },
        .{ "net_socket", .net_socket },
        .{ "net_bind", .net_bind },
        .{ "net_listen", .net_listen },
        .{ "net_accept", .net_accept },
        .{ "net_connect", .net_connect },
        .{ "net_set_reuse_addr", .net_set_reuse_addr },
    });

    pub fn fromString(s: []const u8) ?BuiltinKind {
        return map.get(s);
    }

    pub fn sourceName(self: BuiltinKind) []const u8 {
        return switch (self) {
            .size_of => "sizeOf",
            .align_of => "alignOf",
            .int_cast => "intCast",
            .ptr_cast => "ptrCast",
            .int_to_ptr => "intToPtr",
            .ptr_to_int => "ptrToInt",
            .string => "string",
            .assert => "assert",
            .assert_eq => "assert_eq",
            .alloc => "alloc",
            .dealloc => "dealloc",
            .realloc => "realloc",
            .memcpy => "memcpy",
            .fd_write => "fd_write",
            .fd_read => "fd_read",
            .fd_close => "fd_close",
            .fd_seek => "fd_seek",
            .fd_open => "fd_open",
            .ptr_of => "ptrOf",
            .len_of => "lenOf",
            .trap => "trap",
            .exit => "exit",
            .time => "time",
            .random => "random",
            .args_count => "args_count",
            .arg_len => "arg_len",
            .arg_ptr => "arg_ptr",
            .environ_count => "environ_count",
            .environ_len => "environ_len",
            .environ_ptr => "environ_ptr",
            .target_os => "target_os",
            .target_arch => "target_arch",
            .target => "target",
            .compile_error => "compileError",
            .abs => "abs",
            .ceil => "ceil",
            .floor => "floor",
            .trunc => "trunc",
            .round => "round",
            .sqrt => "sqrt",
            .fmin => "fmin",
            .fmax => "fmax",
            .net_socket => "net_socket",
            .net_bind => "net_bind",
            .net_listen => "net_listen",
            .net_accept => "net_accept",
            .net_connect => "net_connect",
            .net_set_reuse_addr => "net_set_reuse_addr",
        };
    }
};
pub const BuiltinCall = struct { kind: BuiltinKind, type_arg: NodeIndex, args: [3]NodeIndex, span: Span };
pub const StringSegment = union(enum) { text: []const u8, expr: NodeIndex };
pub const StringInterp = struct { segments: []const StringSegment, span: Span };
pub const TypeExpr = struct { kind: TypeKind, span: Span };
pub const GenericInstance = struct { name: []const u8, type_args: []const NodeIndex };
pub const TypeKind = union(enum) {
    named: []const u8,
    pointer: NodeIndex,
    optional: NodeIndex,
    error_union: struct { error_set: NodeIndex = null_node, elem: NodeIndex },
    slice: NodeIndex,
    array: struct { size: NodeIndex, elem: NodeIndex },
    map: struct { key: NodeIndex, value: NodeIndex },
    list: NodeIndex,
    function: struct { params: []const NodeIndex, ret: NodeIndex },
    tuple: []const NodeIndex,
    generic_instance: GenericInstance,
};
pub const TryExpr = struct { operand: NodeIndex, span: Span };
pub const CatchExpr = struct { operand: NodeIndex, capture: []const u8, fallback: NodeIndex, span: Span };
pub const ErrorLiteral = struct { error_name: []const u8, span: Span };
pub const ClosureExpr = struct { params: []const Field, return_type: NodeIndex, body: NodeIndex, span: Span };
pub const TupleLiteral = struct { elements: []const NodeIndex, span: Span };
pub const ComptimeBlock = struct { body: NodeIndex, span: Span };
pub const ZeroInit = struct { span: Span };
pub const AddrOf = struct { operand: NodeIndex, span: Span };
pub const Deref = struct { operand: NodeIndex, span: Span };
pub const BadExpr = struct { span: Span };

// Statements

pub const Stmt = union(enum) {
    expr_stmt: ExprStmt,
    return_stmt: ReturnStmt,
    var_stmt: VarStmt,
    assign_stmt: AssignStmt,
    if_stmt: IfStmt,
    while_stmt: WhileStmt,
    for_stmt: ForStmt,
    block_stmt: BlockStmt,
    break_stmt: BreakStmt,
    continue_stmt: ContinueStmt,
    defer_stmt: DeferStmt,
    bad_stmt: BadStmt,

    pub fn span(self: Stmt) Span {
        return switch (self) { inline else => |s| s.span };
    }
};

pub const ExprStmt = struct { expr: NodeIndex, span: Span };
pub const ReturnStmt = struct { value: NodeIndex, span: Span };
pub const VarStmt = struct { name: []const u8, type_expr: NodeIndex, value: NodeIndex, is_const: bool, span: Span };
pub const AssignStmt = struct { target: NodeIndex, op: Token, value: NodeIndex, span: Span };
pub const IfStmt = struct { condition: NodeIndex, then_branch: NodeIndex, else_branch: NodeIndex, capture: []const u8 = "", span: Span };
pub const WhileStmt = struct { condition: NodeIndex, body: NodeIndex, label: ?[]const u8 = null, span: Span };
/// For loop statement supporting:
/// - `for item in collection { }` (value only)
/// - `for i, item in collection { }` (index and value)
/// - `for i in 0..10 { }` (numeric range)
pub const ForStmt = struct {
    binding: []const u8, // Primary binding (value or index for range)
    index_binding: ?[]const u8 = null, // Optional index when iterating with index
    iterable: NodeIndex, // Collection or null for range
    range_start: NodeIndex = null_node, // For `for i in start..end`
    range_end: NodeIndex = null_node, // For `for i in start..end`
    body: NodeIndex,
    label: ?[]const u8 = null, // Optional label for labeled break/continue (Zig: `outer: for ...`)
    span: Span,

    /// Check if this is a numeric range loop (for i in 0..10)
    pub fn isRange(self: ForStmt) bool {
        return self.range_start != null_node;
    }
};
pub const BlockStmt = struct { stmts: []const NodeIndex, span: Span };
pub const BreakStmt = struct { label: ?[]const u8 = null, span: Span };
pub const ContinueStmt = struct { label: ?[]const u8 = null, span: Span };
pub const DeferStmt = struct { expr: NodeIndex, is_errdefer: bool = false, span: Span };
pub const BadStmt = struct { span: Span };

// Unified Node

pub const Node = union(enum) {
    decl: Decl,
    expr: Expr,
    stmt: Stmt,

    pub fn span(self: Node) Span {
        return switch (self) {
            .decl => |d| d.span(),
            .expr => |e| e.span(),
            .stmt => |s| s.span(),
        };
    }

    pub fn asDecl(self: Node) ?Decl {
        return switch (self) { .decl => |d| d, else => null };
    }

    pub fn asExpr(self: Node) ?Expr {
        return switch (self) { .expr => |e| e, else => null };
    }

    pub fn asStmt(self: Node) ?Stmt {
        return switch (self) { .stmt => |s| s, else => null };
    }
};

// AST Storage

pub const Ast = struct {
    nodes: std.ArrayListUnmanaged(Node),
    allocator: std.mem.Allocator,
    file: ?File,

    pub fn init(allocator: std.mem.Allocator) Ast {
        return .{ .nodes = .{}, .allocator = allocator, .file = null };
    }

    pub fn deinit(self: *Ast) void {
        if (self.file) |file| if (file.decls.len > 0) self.allocator.free(file.decls);

        for (self.nodes.items) |node| {
            switch (node) {
                .decl => |decl| switch (decl) {
                    .fn_decl => |fn_d| {
                        if (fn_d.params.len > 0) self.allocator.free(fn_d.params);
                        if (fn_d.type_params.len > 0) self.allocator.free(fn_d.type_params);
                    },
                    .struct_decl => |s| {
                        if (s.fields.len > 0) self.allocator.free(s.fields);
                        if (s.type_params.len > 0) self.allocator.free(s.type_params);
                    },
                    .enum_decl => |e| if (e.variants.len > 0) self.allocator.free(e.variants),
                    .union_decl => |u| if (u.variants.len > 0) self.allocator.free(u.variants),
                    .error_set_decl => |e| if (e.variants.len > 0) self.allocator.free(e.variants),
                    .impl_block => |ib| {
                        if (ib.type_params.len > 0) self.allocator.free(ib.type_params);
                    },
                    .impl_trait => |it| {
                        if (it.type_params.len > 0) self.allocator.free(it.type_params);
                    },
                    else => {},
                },
                .expr => |expr| switch (expr) {
                    .type_expr => |t| switch (t.kind) {
                        .function => |f| if (f.params.len > 0) self.allocator.free(f.params),
                        .generic_instance => |gi| if (gi.type_args.len > 0) self.allocator.free(gi.type_args),
                        .tuple => |elems| if (elems.len > 0) self.allocator.free(elems),
                        else => {},
                    },
                    .tuple_literal => |tl| if (tl.elements.len > 0) self.allocator.free(tl.elements),
                    .closure_expr => |ce| if (ce.params.len > 0) self.allocator.free(ce.params),
                    .call => |c| if (c.args.len > 0) self.allocator.free(c.args),
                    .array_literal => |a| if (a.elements.len > 0) self.allocator.free(a.elements),
                    .block_expr => |b| if (b.stmts.len > 0) self.allocator.free(b.stmts),
                    .switch_expr => |s| {
                        for (s.cases) |case| if (case.patterns.len > 0) self.allocator.free(case.patterns);
                        if (s.cases.len > 0) self.allocator.free(s.cases);
                    },
                    .struct_init => |si| {
                        if (si.type_args.len > 0) self.allocator.free(si.type_args);
                        if (si.fields.len > 0) self.allocator.free(si.fields);
                    },
                    .new_expr => |ne| {
                        if (ne.type_args.len > 0) self.allocator.free(ne.type_args);
                        if (ne.fields.len > 0) self.allocator.free(ne.fields);
                    },
                    else => {},
                },
                .stmt => |stmt| switch (stmt) {
                    .block_stmt => |b| if (b.stmts.len > 0) self.allocator.free(b.stmts),
                    else => {},
                },
            }
        }
        self.nodes.deinit(self.allocator);
    }

    pub fn addNode(self: *Ast, node: Node) !NodeIndex {
        const idx: NodeIndex = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return idx;
    }

    pub fn addExpr(self: *Ast, expr: Expr) !NodeIndex {
        return self.addNode(.{ .expr = expr });
    }

    pub fn addStmt(self: *Ast, stmt: Stmt) !NodeIndex {
        return self.addNode(.{ .stmt = stmt });
    }

    pub fn addDecl(self: *Ast, decl: Decl) !NodeIndex {
        return self.addNode(.{ .decl = decl });
    }

    pub fn getNode(self: *const Ast, idx: NodeIndex) ?Node {
        if (idx == null_node or idx >= self.nodes.items.len) return null;
        return self.nodes.items[idx];
    }

    pub fn nodeCount(self: *const Ast) usize {
        return self.nodes.items.len;
    }

    pub fn getRootDecls(self: *const Ast) []const NodeIndex {
        return if (self.file) |file| file.decls else &.{};
    }

    pub fn getImports(self: *const Ast, allocator: std.mem.Allocator) ![]const []const u8 {
        var imports = std.ArrayListUnmanaged([]const u8){};
        errdefer imports.deinit(allocator);
        for (self.getRootDecls()) |decl_idx| {
            if (self.getNode(decl_idx)) |node| {
                if (node.asDecl()) |decl| {
                    if (decl == .import_decl) try imports.append(allocator, decl.import_decl.path);
                }
            }
        }
        return imports.toOwnedSlice(allocator);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "null_node is max value" {
    try std.testing.expectEqual(std.math.maxInt(u32), null_node);
}

test "Ast add and get nodes" {
    var tree = Ast.init(std.testing.allocator);
    defer tree.deinit();
    const expr = Expr{ .ident = .{ .name = "x", .span = Span.zero } };
    const idx = try tree.addExpr(expr);
    try std.testing.expectEqual(@as(NodeIndex, 0), idx);
    try std.testing.expectEqual(@as(usize, 1), tree.nodeCount());
    try std.testing.expectEqualStrings("x", tree.getNode(idx).?.asExpr().?.ident.name);
}

test "Ast null_node returns null" {
    var tree = Ast.init(std.testing.allocator);
    defer tree.deinit();
    try std.testing.expect(tree.getNode(null_node) == null);
}

test "Node span accessors" {
    const span = Span.init(Pos{ .offset = 5 }, Pos{ .offset = 10 });
    try std.testing.expectEqual(@as(u32, 5), (Node{ .decl = .{ .bad_decl = .{ .span = span } } }).span().start.offset);
    try std.testing.expectEqual(@as(u32, 5), (Node{ .expr = .{ .bad_expr = .{ .span = span } } }).span().start.offset);
    try std.testing.expectEqual(@as(u32, 5), (Node{ .stmt = .{ .bad_stmt = .{ .span = span } } }).span().start.offset);
}

test "Decl span accessor" {
    const span = Span.init(Pos{ .offset = 0 }, Pos{ .offset = 10 });
    const fn_decl = Decl{ .fn_decl = .{ .name = "main", .params = &.{}, .return_type = null_node, .body = null_node, .is_extern = false, .span = span } };
    try std.testing.expectEqual(@as(u32, 0), fn_decl.span().start.offset);
}

test "Expr span accessor" {
    const span = Span.init(Pos{ .offset = 0 }, Pos{ .offset = 5 });
    const ident = Expr{ .ident = .{ .name = "foo", .span = span } };
    try std.testing.expectEqual(@as(u32, 0), ident.span().start.offset);
}

test "Stmt span accessor" {
    const span = Span.init(Pos{ .offset = 0 }, Pos{ .offset = 10 });
    const return_stmt = Stmt{ .return_stmt = .{ .value = null_node, .span = span } };
    try std.testing.expectEqual(@as(u32, 0), return_stmt.span().start.offset);
}

test "LiteralKind enum" {
    try std.testing.expectEqual(LiteralKind.int, LiteralKind.int);
    try std.testing.expectEqual(LiteralKind.string, LiteralKind.string);
}

test "TypeKind union" {
    try std.testing.expectEqualStrings("int", (TypeKind{ .named = "int" }).named);
    try std.testing.expectEqual(@as(NodeIndex, 0), (TypeKind{ .pointer = 0 }).pointer);
}
