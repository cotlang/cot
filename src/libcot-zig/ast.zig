//! Abstract Syntax Tree definitions for Cot.
//!
//! The AST is a tree of nodes produced by the parser and consumed by the
//! checker and lowerer. Every node is one of three kinds:
//!
//!   Decl — declarations (fn, struct, enum, import, test, ...)
//!   Expr — expressions (ident, literal, binary, call, if, ...)
//!   Stmt — statements (var, assign, return, if, while, for, ...)
//!
//! Nodes are stored in a flat arena (`Ast.nodes`) and referenced by index
//! (`NodeIndex`). This avoids pointer chasing and makes the tree trivially
//! serializable. The sentinel value `null_node` represents "no node" (like
//! null for pointers).

const std = @import("std");
const source = @import("source.zig");
const token = @import("token.zig");

const Span = source.Span;
const Pos = source.Pos;
const Token = token.Token;

/// Index into the AST node arena. All cross-references between nodes use
/// this type rather than pointers. `null_node` = absent.
pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const NodeList = []const NodeIndex;


/// Top-level source file. Contains the filename, root declaration indices,
/// and whether @safe mode is enabled for this file.
pub const File = struct {
    filename: []const u8,
    decls: []const NodeIndex,
    span: Span,
    safe_mode: bool = false,
};


/// Declaration — top-level or nested definitions.
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
    unchecked_sendable: UncheckedSendable,
    test_decl: TestDecl,
    bench_decl: BenchDecl,
    bad_decl: BadDecl,

    pub fn span(self: Decl) Span {
        return switch (self) { inline else => |d| d.span };
    }
};

pub const FnDecl = struct {
    name: []const u8,
    type_params: []const []const u8 = &.{},
    /// Parallel with type_params — null entry means unbounded.
    type_param_bounds: []const ?[]const u8 = &.{},
    params: []const Field,
    return_type: NodeIndex,
    body: NodeIndex,
    is_extern: bool,
    is_export: bool = false,
    is_async: bool = false,
    is_static: bool = false,
    /// Monomorphize this generic function (one copy per concrete type combo).
    is_inlinable: bool = false,
    /// Actor method accessible without await from outside (must not access mutable state).
    is_nonisolated: bool = false,
    /// Function isolated to a global actor (e.g., @MainActor).
    global_actor: ?[]const u8 = null,
    doc_comment: []const u8 = "",
    span: Span,
};

pub const VarDecl = struct {
    name: []const u8,
    type_expr: NodeIndex,
    value: NodeIndex,
    is_const: bool,
    doc_comment: []const u8 = "",
    span: Span,
};

pub const StructLayout = enum { auto, @"packed", @"extern" };

pub const StructDecl = struct {
    name: []const u8,
    type_params: []const []const u8 = &.{},
    fields: []const Field,
    layout: StructLayout = .auto,
    nested_decls: []const NodeIndex = &.{},
    doc_comment: []const u8 = "",
    is_actor: bool = false,
    span: Span,
};

pub const ImplBlock = struct {
    type_name: []const u8,
    type_params: []const []const u8 = &.{},
    methods: []const NodeIndex,
    consts: []const NodeIndex = &.{},
    doc_comment: []const u8 = "",
    span: Span,
};

/// Associated type inside a trait: `type Iterator: AsyncIterator(Element)`
pub const AssocTypeDecl = struct {
    name: []const u8,
    bound: []const u8 = "",
    span: Span,
};

pub const TraitDecl = struct {
    name: []const u8,
    type_params: []const []const u8 = &.{},
    methods: []const NodeIndex,
    assoc_types: []const AssocTypeDecl = &.{},
    doc_comment: []const u8 = "",
    span: Span,
};

pub const ImplTraitBlock = struct {
    trait_name: []const u8,
    target_type: []const u8,
    type_params: []const []const u8 = &.{},
    methods: []const NodeIndex,
    assoc_types: []const AssocTypeDecl = &.{},
    doc_comment: []const u8 = "",
    span: Span,
};

pub const EnumDecl = struct {
    name: []const u8,
    backing_type: NodeIndex,
    variants: []const EnumVariant,
    nested_decls: []const NodeIndex = &.{},
    doc_comment: []const u8 = "",
    span: Span,
};

pub const UnionDecl = struct {
    name: []const u8,
    variants: []const UnionVariant,
    doc_comment: []const u8 = "",
    span: Span,
};

pub const TypeAlias = struct {
    name: []const u8,
    target: NodeIndex,
    is_distinct: bool = false,
    doc_comment: []const u8 = "",
    span: Span,
};

pub const ImportDecl = struct { path: []const u8, span: Span };
pub const ErrorSetDecl = struct { name: []const u8, variants: []const []const u8, doc_comment: []const u8 = "", span: Span };
pub const UncheckedSendable = struct { type_name: []const u8, span: Span };
pub const TestDecl = struct { name: []const u8, body: NodeIndex, span: Span };
pub const BenchDecl = struct { name: []const u8, body: NodeIndex, span: Span };
pub const BadDecl = struct { span: Span };

pub const Field = struct {
    name: []const u8,
    type_expr: NodeIndex,
    default_value: NodeIndex,
    doc_comment: []const u8 = "",
    /// Ownership transfer to callee (SE-0430 `sending` parameter).
    is_sending: bool = false,
    span: Span,
};

pub const EnumVariant = struct { name: []const u8, value: NodeIndex, span: Span };
pub const UnionVariant = struct { name: []const u8, type_expr: NodeIndex, span: Span };


/// Expression — anything that produces a value.
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
    await_expr: AwaitExpr,
    task_expr: TaskExpr,
    catch_expr: CatchExpr,
    orelse_expr: OrElseExpr,
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
pub const LiteralKind = enum { int, float, string, char, true_lit, false_lit, null_lit, undefined_lit, unreachable_lit };
pub const Binary = struct { op: Token, left: NodeIndex, right: NodeIndex, span: Span };
pub const Unary = struct { op: Token, operand: NodeIndex, span: Span };
pub const Call = struct { callee: NodeIndex, args: []const NodeIndex, span: Span };
pub const Index = struct { base: NodeIndex, idx: NodeIndex, span: Span };
pub const SliceExpr = struct { base: NodeIndex, start: NodeIndex, end: NodeIndex, span: Span };
pub const FieldAccess = struct { base: NodeIndex, field: []const u8, span: Span };
pub const ArrayLiteral = struct { elements: []const NodeIndex, span: Span };
pub const Paren = struct { inner: NodeIndex, span: Span };

pub const IfExpr = struct {
    condition: NodeIndex,
    then_branch: NodeIndex,
    else_branch: NodeIndex,
    capture: []const u8 = "",
    capture_is_ptr: bool = false,
    span: Span,
};

pub const SwitchExpr = struct {
    subject: NodeIndex,
    cases: []const SwitchCase,
    else_body: NodeIndex,
    span: Span,
};

pub const SwitchCase = struct {
    patterns: []const NodeIndex,
    capture: []const u8,
    capture_is_ptr: bool = false,
    /// Optional guard expression: `pattern if guard_expr =>`
    guard: NodeIndex = null_node,
    /// If true, patterns[0] = start, patterns[1] = end (inclusive range).
    is_range: bool = false,
    body: NodeIndex,
    span: Span,
};

pub const BlockExpr = struct { stmts: []const NodeIndex, expr: NodeIndex, label: ?[]const u8 = null, span: Span };

pub const StructInit = struct {
    type_name: []const u8,
    type_args: []const NodeIndex = &.{},
    fields: []const FieldInit,
    span: Span,
};

pub const FieldInit = struct { name: []const u8, value: NodeIndex, span: Span };

/// Heap allocation: `new Type { field: value }` or `new Type(args...)`.
pub const NewExpr = struct {
    type_name: []const u8,
    type_args: []const NodeIndex = &.{},
    fields: []const FieldInit,
    constructor_args: []const NodeIndex = &.{},
    is_constructor: bool = false,
    span: Span,
};

/// Compiler builtin function (@sizeOf, @intCast, @panic, etc.).
///
/// Source names are camelCase (@sizeOf, @intFromEnum). Enum tags are snake_case
/// (size_of, int_from_enum). The mapping is generated at comptime — 59 of 61
/// builtins follow mechanical snake_to_camel conversion. Only 2 exceptions:
///   type_of → "TypeOf" (capital T)
///   atomic_cas → "atomicCAS" (all-caps CAS)
pub const BuiltinKind = enum {
    // Type intrinsics
    size_of,
    align_of,
    enum_len,

    // Casts
    int_cast,
    float_cast,
    float_from_int,
    int_from_float,
    ptr_cast,
    int_to_ptr,
    ptr_to_int,

    // String / assertion
    string,
    assert,
    assert_eq,
    ptr_of,
    len_of,
    trap,

    // Target info
    target_os,
    target_arch,
    target,

    // Compile-time
    compile_error,
    embed_file,

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

    // Reflection
    has_field,
    type_of,
    field,
    type_name,
    enum_name,
    type_info,

    // Enum/error intrinsics
    int_from_enum,
    enum_from_int,
    tag_name,
    error_name,
    int_from_bool,

    // Cast intrinsics
    bit_cast,
    truncate,
    as,

    // Struct introspection
    offset_of,

    // Integer min/max
    min,
    max,

    // Pointer cast
    align_cast,
    const_cast,

    // ARC management
    arc_retain,
    arc_release,
    is_unique,

    // Panic
    panic,

    // Bit manipulation
    ctz,
    clz,
    pop_count,

    // Atomics
    atomic_load,
    atomic_store,
    atomic_add,
    atomic_cas,
    atomic_exchange,

    /// Get the user-facing source name (@sizeOf, @intCast, etc.).
    /// Most names are mechanical snake_to_camel; only 2 exceptions exist.
    pub fn sourceName(self: BuiltinKind) []const u8 {
        return switch (self) {
            // Exceptions — don't follow mechanical snake_to_camel
            .type_of => "TypeOf",
            .atomic_cas => "atomicCAS",
            // Everything else: use comptime-generated camelCase name
            inline else => |tag| comptime snakeToCamel(@tagName(tag)),
        };
    }

    /// Reverse lookup: source name string → enum.
    /// Built at comptime from @typeInfo + sourceName().
    pub const map = blk: {
        const fields = @typeInfo(BuiltinKind).@"enum".fields;
        var entries: [fields.len]struct { []const u8, BuiltinKind } = undefined;
        for (fields, 0..) |f, i| {
            const kind: BuiltinKind = @enumFromInt(f.value);
            entries[i] = .{ kind.sourceName(), kind };
        }
        break :blk std.StaticStringMap(BuiltinKind).initComptime(entries);
    };

    pub fn fromString(s: []const u8) ?BuiltinKind {
        return map.get(s);
    }
};

/// Convert snake_case to camelCase at comptime.
/// "size_of" → "sizeOf", "int_from_enum" → "intFromEnum"
fn snakeToCamel(comptime input: []const u8) *const [comptimeCamelLen(input)]u8 {
    // Build a fixed-size array at comptime and return a pointer to it.
    // The pointer-to-array coerces to []const u8 at usage sites, and unlike
    // a comptime var slice, it's a valid comptime-known value at runtime.
    @setEvalBranchQuota(10_000);
    comptime {
        const len = comptimeCamelLen(input);
        var buf: [len]u8 = undefined;
        var i: usize = 0;
        var capitalize_next = false;
        for (input) |c| {
            if (c == '_') {
                capitalize_next = true;
            } else if (capitalize_next) {
                buf[i] = std.ascii.toUpper(c);
                i += 1;
                capitalize_next = false;
            } else {
                buf[i] = c;
                i += 1;
            }
        }
        const final = buf;
        return &final;
    }
}

fn comptimeCamelLen(comptime input: []const u8) usize {
    var len: usize = 0;
    for (input) |c| {
        if (c != '_') len += 1;
    }
    return len;
}

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
    existential: NodeIndex,
};

pub const TryExpr = struct { operand: NodeIndex, span: Span };
pub const AwaitExpr = struct { operand: NodeIndex, span: Span };

/// Unstructured task creation: `Task { body }` or `Task.detached { body }`.
pub const TaskExpr = struct {
    body: NodeIndex,
    is_detached: bool = false,
    span: Span,
};

pub const CatchExpr = struct {
    operand: NodeIndex,
    capture: []const u8,
    capture_is_ptr: bool = false,
    fallback: NodeIndex,
    span: Span,
};

pub const OrElseExpr = struct {
    operand: NodeIndex,
    fallback: NodeIndex,
    fallback_kind: OrElseFallback,
    span: Span,
};

pub const OrElseFallback = enum { expr, return_void, return_val, break_val, continue_val };
pub const ErrorLiteral = struct { error_name: []const u8, span: Span };

pub const ClosureExpr = struct {
    params: []const Field,
    return_type: NodeIndex,
    body: NodeIndex,
    is_sendable: bool = false,
    is_async: bool = false,
    span: Span,
};

pub const TupleLiteral = struct { elements: []const NodeIndex, span: Span };
pub const ComptimeBlock = struct { body: NodeIndex, span: Span };
pub const ZeroInit = struct { span: Span };
pub const AddrOf = struct { operand: NodeIndex, span: Span };
pub const Deref = struct { operand: NodeIndex, span: Span };
pub const BadExpr = struct { span: Span };


/// Statement — things that don't produce values (or produce them as side effects).
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
    async_let: AsyncLet,
    destructure_stmt: DestructureStmt,
    bad_stmt: BadStmt,

    pub fn span(self: Stmt) Span {
        return switch (self) { inline else => |s| s.span };
    }
};

pub const ExprStmt = struct { expr: NodeIndex, span: Span };
pub const ReturnStmt = struct { value: NodeIndex, span: Span };

pub const VarStmt = struct {
    name: []const u8,
    type_expr: NodeIndex,
    value: NodeIndex,
    is_const: bool,
    is_weak: bool = false,
    is_unowned: bool = false,
    span: Span,
};

pub const DestructureBinding = struct { name: []const u8, type_expr: NodeIndex, span: Span };
pub const DestructureStmt = struct { bindings: []const DestructureBinding, value: NodeIndex, is_const: bool, span: Span };
pub const AssignStmt = struct { target: NodeIndex, op: Token, value: NodeIndex, span: Span };

pub const IfStmt = struct {
    condition: NodeIndex,
    then_branch: NodeIndex,
    else_branch: NodeIndex,
    capture: []const u8 = "",
    capture_is_ptr: bool = false,
    span: Span,
};

pub const WhileStmt = struct {
    condition: NodeIndex,
    body: NodeIndex,
    label: ?[]const u8 = null,
    capture: []const u8 = "",
    capture_is_ptr: bool = false,
    continue_expr: NodeIndex = null_node,
    span: Span,
};

/// For loop: value iteration, index+value iteration, or numeric range.
///
///   for item in collection { }       — value only
///   for i, item in collection { }    — index and value
///   for i in 0..10 { }               — numeric range
///   for await item in stream { }     — async iteration
pub const ForStmt = struct {
    binding: []const u8,
    index_binding: ?[]const u8 = null,
    iterable: NodeIndex,
    range_start: NodeIndex = null_node,
    range_end: NodeIndex = null_node,
    body: NodeIndex,
    label: ?[]const u8 = null,
    is_inline: bool = false,
    is_await: bool = false,
    span: Span,

    pub fn isRange(self: ForStmt) bool {
        return self.range_start != null_node;
    }
};

pub const BlockStmt = struct { stmts: []const NodeIndex, span: Span };
pub const BreakStmt = struct { label: ?[]const u8 = null, value: NodeIndex = null_node, span: Span };
pub const ContinueStmt = struct { label: ?[]const u8 = null, span: Span };
pub const DeferStmt = struct { expr: NodeIndex, is_errdefer: bool = false, span: Span };

/// Parallel binding: `async let x = expr` — creates structured child task.
pub const AsyncLet = struct { name: []const u8, value: NodeIndex, span: Span };
pub const BadStmt = struct { span: Span };


/// Top-level node: every AST node is a declaration, expression, or statement.
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


/// AST node arena. All nodes are stored in a flat array and referenced by
/// index. The parser builds the tree by appending nodes and storing indices
/// in parent nodes' fields.
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
                    .fn_decl => |f| {
                        if (f.params.len > 0) self.allocator.free(f.params);
                        if (f.type_params.len > 0) self.allocator.free(f.type_params);
                    },
                    .struct_decl => |s| {
                        if (s.fields.len > 0) self.allocator.free(s.fields);
                        if (s.type_params.len > 0) self.allocator.free(s.type_params);
                        if (s.nested_decls.len > 0) self.allocator.free(s.nested_decls);
                    },
                    .enum_decl => |e| {
                        if (e.variants.len > 0) self.allocator.free(e.variants);
                        if (e.nested_decls.len > 0) self.allocator.free(e.nested_decls);
                    },
                    .union_decl => |u| if (u.variants.len > 0) self.allocator.free(u.variants),
                    .error_set_decl => |e| if (e.variants.len > 0) self.allocator.free(e.variants),
                    .impl_block => |ib| {
                        if (ib.type_params.len > 0) self.allocator.free(ib.type_params);
                        if (ib.consts.len > 0) self.allocator.free(ib.consts);
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
                    .destructure_stmt => |d| if (d.bindings.len > 0) self.allocator.free(d.bindings),
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

    /// Check if the file uses async features (needed for auto-importing the executor).
    pub fn hasAsyncFunctions(self: *const Ast) bool {
        for (self.nodes.items) |node| {
            if (node.asDecl()) |decl| {
                if (decl == .fn_decl and decl.fn_decl.is_async) return true;
            }
            if (node.asExpr()) |expr| {
                if (expr == .closure_expr and expr.closure_expr.is_async) return true;
                if (expr == .await_expr) return true;
            }
        }
        return false;
    }
};


test "null_node is max value" {
    try std.testing.expectEqual(std.math.maxInt(u32), null_node);
}

test "add and get nodes" {
    var tree = Ast.init(std.testing.allocator);
    defer tree.deinit();
    const expr = Expr{ .ident = .{ .name = "x", .span = Span.zero } };
    const idx = try tree.addExpr(expr);
    try std.testing.expectEqual(@as(NodeIndex, 0), idx);
    try std.testing.expectEqual(@as(usize, 1), tree.nodeCount());
    try std.testing.expectEqualStrings("x", tree.getNode(idx).?.asExpr().?.ident.name);
}

test "null_node returns null" {
    var tree = Ast.init(std.testing.allocator);
    defer tree.deinit();
    try std.testing.expect(tree.getNode(null_node) == null);
}

test "span accessors" {
    const span = Span.init(Pos{ .offset = 5 }, Pos{ .offset = 10 });
    try std.testing.expectEqual(@as(u32, 5), (Node{ .decl = .{ .bad_decl = .{ .span = span } } }).span().start.offset);
    try std.testing.expectEqual(@as(u32, 5), (Node{ .expr = .{ .bad_expr = .{ .span = span } } }).span().start.offset);
    try std.testing.expectEqual(@as(u32, 5), (Node{ .stmt = .{ .bad_stmt = .{ .span = span } } }).span().start.offset);
}

test "builtin source names" {
    try std.testing.expectEqualStrings("sizeOf", BuiltinKind.size_of.sourceName());
    try std.testing.expectEqualStrings("intFromEnum", BuiltinKind.int_from_enum.sourceName());
    try std.testing.expectEqualStrings("assertEq", BuiltinKind.assert_eq.sourceName());
    // Exceptions
    try std.testing.expectEqualStrings("TypeOf", BuiltinKind.type_of.sourceName());
    try std.testing.expectEqualStrings("atomicCAS", BuiltinKind.atomic_cas.sourceName());
    // Single-word builtins
    try std.testing.expectEqualStrings("abs", BuiltinKind.abs.sourceName());
    try std.testing.expectEqualStrings("panic", BuiltinKind.panic.sourceName());
}

test "builtin lookup" {
    try std.testing.expectEqual(BuiltinKind.size_of, BuiltinKind.fromString("sizeOf").?);
    try std.testing.expectEqual(BuiltinKind.int_from_enum, BuiltinKind.fromString("intFromEnum").?);
    try std.testing.expectEqual(BuiltinKind.type_of, BuiltinKind.fromString("TypeOf").?);
    try std.testing.expectEqual(BuiltinKind.atomic_cas, BuiltinKind.fromString("atomicCAS").?);
    try std.testing.expect(BuiltinKind.fromString("notABuiltin") == null);
}

test "snakeToCamel" {
    try std.testing.expectEqualStrings("sizeOf", comptime snakeToCamel("size_of"));
    try std.testing.expectEqualStrings("intFromEnum", comptime snakeToCamel("int_from_enum"));
    try std.testing.expectEqualStrings("abs", comptime snakeToCamel("abs"));
    try std.testing.expectEqualStrings("assertEq", comptime snakeToCamel("assert_eq"));
}
