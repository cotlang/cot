//! Abstract Syntax Tree for Cot source code.
//!
//! Nodes are stored in a flat arena using a data-oriented design inspired by
//! Zig's own compiler AST. Every node is exactly 13 bytes (padded to 16):
//!
//!   tag:        u8   — identifies the syntax form (Tag enum)
//!   main_token: u32  — primary token index for this node
//!   data:       8B   — two u32 payload values (union)
//!
//! Variable-length data (parameter lists, field lists, etc.) is stored in a
//! separate `extra_data: []u32` sidecar array, referenced by index. This
//! eliminates per-node heap allocations entirely.
//!
//! Consumers access rich node data through `full.*` accessor functions that
//! unpack the compact representation into ergonomic structs on the stack.

const std = @import("std");
const source = @import("source.zig");
const tok = @import("token.zig");

const Span = source.Span;
const Pos = source.Pos;
const Token = tok.Token;

// ============================================================================
// Index types — type-safe wrappers around u32
// ============================================================================

/// Index into the node array.
pub const Index = enum(u32) {
    /// The root node is always at index 0.
    root = 0,
    _,

    pub fn toOptional(i: Index) OptionalIndex {
        const result: OptionalIndex = @enumFromInt(@intFromEnum(i));
        std.debug.assert(result != .none);
        return result;
    }
};

/// Index into the node array, or absent.
pub const OptionalIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(oi: OptionalIndex) ?Index {
        return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
    }

    pub fn fromOptional(oi: ?Index) OptionalIndex {
        return if (oi) |i| i.toOptional() else .none;
    }
};

/// Index into the token array.
pub const TokenIndex = u32;

/// Index into the token array, or absent.
pub const OptionalTokenIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(oti: OptionalTokenIndex) ?TokenIndex {
        return if (oti == .none) null else @intFromEnum(oti);
    }
};

/// Index into `extra_data`.
pub const ExtraIndex = enum(u32) {
    _,
};

/// A contiguous range in `extra_data`, representing a variable-length list.
pub const SubRange = struct {
    start: ExtraIndex,
    end: ExtraIndex,

    pub fn len(self: SubRange) u32 {
        return @intFromEnum(self.end) - @intFromEnum(self.start);
    }

    pub const empty: SubRange = .{
        .start = @enumFromInt(0),
        .end = @enumFromInt(0),
    };
};

// ============================================================================
// Node — the core fixed-size unit
// ============================================================================

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    comptime {
        std.debug.assert(@sizeOf(Tag) == 1);
        if (!std.debug.runtime_safety) {
            std.debug.assert(@sizeOf(Data) == 8);
        }
    }

    // ========================================================================
    // Tag — every Cot syntax form
    // ========================================================================

    pub const Tag = enum(u8) {
        // ----- Root -----

        /// Root node (always at index 0). data: extra_range of top-level decl indices.
        root,

        // ----- Declarations -----

        /// `fn name(params) RetType { body }` — data: extra(FnDecl) + body Index
        fn_decl,
        /// `extern fn name(params) RetType` — data: extra(FnDecl), no body
        fn_decl_extern,
        /// `var name: Type = value` or `const name = value` — data: extra(VarDecl)
        var_decl,
        /// `struct Name { fields }` — data: extra(StructDecl)
        struct_decl,
        /// `enum Name { variants }` — data: extra(EnumDecl)
        enum_decl,
        /// `union Name { variants }` — data: extra(UnionDecl)
        union_decl,
        /// `type Name = Target` — data: name token + target node
        type_alias,
        /// `type Name = distinct Target` — data: name token + target node
        type_alias_distinct,
        /// `import "path"` — data: path token
        import_decl,
        /// `impl Type { methods }` — data: extra(ImplBlock)
        impl_block,
        /// `trait Name { methods }` — data: extra(TraitDecl)
        trait_decl,
        /// `impl Trait for Type { methods }` — data: extra(ImplTrait)
        impl_trait,
        /// `error { Variant1, Variant2 }` — data: extra(ErrorSetDecl)
        error_set_decl,
        /// `@unchecked(Sendable) TypeName` — data: type_name token
        unchecked_sendable,
        /// `test "name" { body }` — data: name token + body node
        test_decl,
        /// `bench "name" { body }` — data: name token + body node
        bench_decl,

        // ----- Literals -----

        /// Identifier — data: unused (name derived from main_token)
        ident,
        /// Integer literal — data: unused (text from main_token)
        literal_int,
        /// Float literal — data: unused
        literal_float,
        /// String literal — data: unused
        literal_string,
        /// Character literal — data: unused
        literal_char,
        /// `true` — data: unused
        literal_true,
        /// `false` — data: unused
        literal_false,
        /// `null` — data: unused
        literal_null,
        /// `undefined` — data: unused
        literal_undefined,
        /// `unreachable` — data: unused
        literal_unreachable,
        /// `error.Name` — data: name token (the identifier after `error.`)
        error_literal,

        // ----- Binary operators -----
        // All: data = left node + right node

        binary_add,
        binary_sub,
        binary_mul,
        binary_div,
        binary_mod,
        binary_eq,
        binary_neq,
        binary_lt,
        binary_gt,
        binary_lte,
        binary_gte,
        binary_and,
        binary_or,
        binary_bit_and,
        binary_bit_or,
        binary_bit_xor,
        binary_shl,
        binary_shr,
        binary_concat,
        binary_pipe,

        // ----- Unary operators -----
        // All: data = operand node

        unary_neg,
        unary_not,
        unary_bit_not,
        unary_addr_of,
        unary_deref,
        unary_try,
        unary_await,
        unary_unwrap, // .?

        // ----- Calls -----

        /// `f()` — data: callee node (no args)
        call_zero,
        /// `f(a)` — data: callee node + arg node
        call_one,
        /// `f(a, b, ...)` — data: callee node + extra(SubRange) for args
        call,

        // ----- Access -----

        /// `a[i]` — data: base node + index node
        index,
        /// `a[start..end]` — data: base node + extra(SliceData)
        slice,
        /// `a.field` — data: base node + field name token
        field_access,

        // ----- Array/Tuple literals -----

        /// `[]` or `.{}` — data: unused
        array_literal_empty,
        /// `[x]` — data: element node
        array_literal_one,
        /// `[a, b, ...]` — data: extra_range of element nodes
        array_literal,
        /// `(a, b, ...)` tuple literal — data: extra_range of element nodes
        tuple_literal,

        // ----- Grouping -----

        /// `(expr)` — data: inner node
        paren,

        // ----- If -----

        /// `if cond then_branch` (no else) — data: condition node + then node
        if_simple,
        /// `if cond then_branch else else_branch` — data: condition node + extra(IfData)
        if_full,

        // ----- Switch -----

        /// `switch subject { cases }` — data: subject node + extra(SwitchData)
        switch_expr,

        // ----- Blocks -----

        /// Single-statement block — data: stmt node + result node
        block_one,
        /// Two-statement block — data: stmt1 node + stmt2 node
        block_two,
        /// Block with 3+ stmts — data: extra_range of stmt nodes + extra result OptionalIndex
        block,

        // ----- Struct/new init -----

        /// `Type { field: value }` (1 field) — data: extra(StructInitOne)
        struct_init_one,
        /// `Type { f1: v1, f2: v2, ... }` — data: extra(StructInit)
        struct_init,
        /// `new Type { fields }` or `new Type(args)` — data: extra(NewExpr)
        new_expr,

        // ----- Builtins -----

        /// `@builtin(args)` — data: extra(BuiltinCallData)
        builtin_call,

        // ----- String interpolation -----

        /// `"text ${expr} text"` — data: extra_range of segment data
        string_interp,

        // ----- Type expressions -----

        /// Named type: `int`, `MyStruct` — data: unused (name from main_token)
        type_named,
        /// `*T` — data: pointee node
        type_pointer,
        /// `?T` — data: child node
        type_optional,
        /// `!T` (no error set) — data: elem node
        type_error_union,
        /// `ErrorSet!T` — data: error_set node + elem node
        type_error_union_set,
        /// `[]T` — data: elem node
        type_slice,
        /// `[N]T` — data: size node + elem node
        type_array,
        /// `Map(K, V)` — data: key node + value node
        type_map,
        /// `List(T)` — data: elem node
        type_list,
        /// `fn(A, B) -> R` — data: extra(FnType)
        type_function,
        /// `(A, B, C)` tuple type — data: extra_range of element type nodes
        type_tuple,
        /// `Name(T1, T2)` generic instance — data: extra(GenericInstance)
        type_generic,
        /// `any Trait` existential — data: trait node
        type_existential,

        // ----- Error handling expressions -----

        /// `x catch |e| fallback` — data: operand node + extra(CatchData)
        catch_expr,
        /// `x orelse fallback` — data: operand node + extra(OrElseData)
        orelse_expr,

        // ----- Closures -----

        /// `fn(params) -> T { body }` closure — data: extra(ClosureData)
        closure_expr,

        // ----- Misc expressions -----

        /// `comptime { body }` — data: body node
        comptime_block,
        /// `Task { body }` or `Task.detached { body }` — data: extra(TaskData)
        task_expr,
        /// `.{}` zero-init — data: unused
        zero_init,

        // ----- Statements -----

        /// Expression statement `expr;` — data: expr node
        expr_stmt,
        /// `return value` — data: value node
        return_expr,
        /// `return` (void) — data: unused
        return_void,

        /// `var name: Type = value` local — data: extra(LocalVarData)
        var_local,
        /// `const name: Type = value` local — data: extra(LocalVarData)
        const_local,

        /// `target = value` — data: target node + value node
        assign,
        /// `target += value` — data: target node + value node
        assign_add,
        assign_sub,
        assign_mul,
        assign_div,
        assign_mod,
        assign_bit_and,
        assign_bit_or,
        assign_bit_xor,

        /// `if cond { then }` statement (no else) — data: condition node + then node
        if_stmt_simple,
        /// `if cond { then } else { else }` — data: condition node + extra(IfData)
        if_stmt,

        /// `while cond { body }` — data: condition node + extra(WhileData)
        while_stmt,

        /// `for binding in iterable { body }` — data: extra(ForData)
        for_stmt,

        /// Block statement with 3+ stmts — data: extra_range of stmt nodes
        block_stmt,

        /// `break` — data: unused
        break_plain,
        /// `break :label value` — data: extra(BreakData)
        break_expr,
        /// `continue` — data: unused
        continue_plain,
        /// `continue :label` — data: label token
        continue_labeled,

        /// `defer expr` — data: expr node
        defer_stmt,
        /// `errdefer expr` — data: expr node
        errdefer_stmt,

        /// `async let name = value` — data: name token + value node
        async_let,

        /// `let (a, b) = value` — data: extra(DestructureData)
        destructure,

        /// Error recovery sentinel.
        bad_node,
    };

    // ========================================================================
    // Data — two u32 payload
    // ========================================================================

    pub const Data = union {
        none: void,
        node: Index,
        opt_node: OptionalIndex,
        token: TokenIndex,
        node_and_node: struct { Index, Index },
        node_and_opt_node: struct { Index, OptionalIndex },
        node_and_extra: struct { Index, ExtraIndex },
        node_and_token: struct { Index, TokenIndex },
        extra_and_node: struct { ExtraIndex, Index },
        extra_and_opt_node: struct { ExtraIndex, OptionalIndex },
        token_and_node: struct { TokenIndex, Index },
        token_and_token: struct { TokenIndex, TokenIndex },
        extra_range: SubRange,
    };
};

// ============================================================================
// Extra data structs — packed into extra_data: []u32
// ============================================================================

pub const FnDecl = struct {
    name_token: TokenIndex,
    type_params: SubRange,
    param_bounds: SubRange,
    params: SubRange,
    return_type: OptionalIndex,
    flags: FnFlags,
    doc_comment: OptionalTokenIndex,
    global_actor: OptionalTokenIndex,
};

pub const FnFlags = packed struct(u32) {
    is_export: bool = false,
    is_async: bool = false,
    is_static: bool = false,
    is_inlinable: bool = false,
    is_nonisolated: bool = false,
    _padding: u27 = 0,
};

pub const VarDecl = struct {
    name_token: TokenIndex,
    type_expr: OptionalIndex,
    value: OptionalIndex,
    doc_comment: OptionalTokenIndex,
    flags: VarFlags,
};

pub const VarFlags = packed struct(u32) {
    is_const: bool = false,
    _padding: u31 = 0,
};

pub const StructDecl = struct {
    name_token: TokenIndex,
    type_params: SubRange,
    fields: SubRange,
    nested_decls: SubRange,
    layout: u32, // StructLayout enum value
    flags: StructFlags,
    doc_comment: OptionalTokenIndex,
};

pub const StructLayout = enum(u32) { auto = 0, @"packed" = 1, @"extern" = 2 };

pub const StructFlags = packed struct(u32) {
    is_actor: bool = false,
    _padding: u31 = 0,
};

pub const EnumDecl = struct {
    name_token: TokenIndex,
    backing_type: OptionalIndex,
    variants: SubRange,
    nested_decls: SubRange,
    doc_comment: OptionalTokenIndex,
};

pub const UnionDecl = struct {
    name_token: TokenIndex,
    variants: SubRange,
    doc_comment: OptionalTokenIndex,
};

pub const ImplBlock = struct {
    type_name_token: TokenIndex,
    type_params: SubRange,
    methods: SubRange,
    consts: SubRange,
    doc_comment: OptionalTokenIndex,
};

pub const TraitDecl = struct {
    name_token: TokenIndex,
    type_params: SubRange,
    methods: SubRange,
    assoc_types: SubRange,
    doc_comment: OptionalTokenIndex,
};

pub const ImplTrait = struct {
    trait_name_token: TokenIndex,
    target_type_token: TokenIndex,
    type_params: SubRange,
    methods: SubRange,
    assoc_types: SubRange,
    doc_comment: OptionalTokenIndex,
};

pub const ErrorSetDecl = struct {
    name_token: TokenIndex,
    variants: SubRange,
    doc_comment: OptionalTokenIndex,
};

pub const Field = struct {
    name_token: TokenIndex,
    type_expr: Index,
    default_value: OptionalIndex,
    flags: FieldFlags,
};

pub const FieldFlags = packed struct(u32) {
    is_sending: bool = false,
    _padding: u31 = 0,
};

pub const EnumVariant = struct {
    name_token: TokenIndex,
    value: OptionalIndex,
};

pub const UnionVariant = struct {
    name_token: TokenIndex,
    type_expr: Index,
};

pub const AssocType = struct {
    name_token: TokenIndex,
    bound_token: OptionalTokenIndex,
};

/// Extra data for `if_full` and `if_stmt`.
pub const IfData = struct {
    then_node: Index,
    else_node: OptionalIndex,
    capture_token: OptionalTokenIndex,
    flags: IfFlags,
};

pub const IfFlags = packed struct(u32) {
    capture_is_ptr: bool = false,
    _padding: u31 = 0,
};

/// Extra data for `switch_expr`.
pub const SwitchData = struct {
    cases: SubRange,
    else_body: OptionalIndex,
};

pub const SwitchCaseData = struct {
    patterns: SubRange,
    guard: OptionalIndex,
    capture_token: OptionalTokenIndex,
    body: Index,
    flags: SwitchCaseFlags,
};

pub const SwitchCaseFlags = packed struct(u32) {
    is_range: bool = false,
    capture_is_ptr: bool = false,
    _padding: u30 = 0,
};

/// Extra data for `slice`.
pub const SliceData = struct {
    start: OptionalIndex,
    end: OptionalIndex,
};

/// Extra data for `struct_init_one`.
pub const StructInitOne = struct {
    type_name_token: OptionalTokenIndex,
    field_name_token: TokenIndex,
    field_value: Index,
};

/// Extra data for `struct_init`.
pub const StructInit = struct {
    type_name_token: OptionalTokenIndex,
    type_args: SubRange,
    fields: SubRange, // pairs of (name_token, value_node) in extra_data
};

/// Extra data for `new_expr`.
pub const NewExprData = struct {
    type_name_token: TokenIndex,
    type_args: SubRange,
    fields: SubRange,
    constructor_args: SubRange,
    flags: NewFlags,
};

pub const NewFlags = packed struct(u32) {
    is_constructor: bool = false,
    _padding: u31 = 0,
};

pub const BuiltinCallData = struct {
    kind: u32, // BuiltinKind enum value
    type_arg: OptionalIndex,
    arg0: OptionalIndex,
    arg1: OptionalIndex,
    arg2: OptionalIndex,
};

pub const FnType = struct {
    params: SubRange,
    ret: OptionalIndex,
};

pub const GenericInstance = struct {
    name_token: TokenIndex,
    type_args: SubRange,
};

pub const CatchData = struct {
    capture_token: OptionalTokenIndex,
    fallback: Index,
    flags: CatchFlags,
};

pub const CatchFlags = packed struct(u32) {
    capture_is_ptr: bool = false,
    _padding: u31 = 0,
};

pub const OrElseData = struct {
    fallback: Index,
    fallback_kind: u32, // OrElseFallback enum value
};

pub const OrElseFallback = enum(u32) {
    expr = 0,
    return_void = 1,
    return_val = 2,
    break_val = 3,
    continue_val = 4,
};

pub const ClosureData = struct {
    params: SubRange,
    return_type: OptionalIndex,
    body: Index,
    flags: ClosureFlags,
};

pub const ClosureFlags = packed struct(u32) {
    is_sendable: bool = false,
    is_async: bool = false,
    _padding: u30 = 0,
};

pub const TaskData = struct {
    body: Index,
    flags: TaskFlags,
};

pub const TaskFlags = packed struct(u32) {
    is_detached: bool = false,
    _padding: u31 = 0,
};

pub const LocalVarData = struct {
    name_token: TokenIndex,
    type_expr: OptionalIndex,
    value: OptionalIndex,
    flags: LocalVarFlags,
};

pub const LocalVarFlags = packed struct(u32) {
    is_weak: bool = false,
    is_unowned: bool = false,
    _padding: u30 = 0,
};

pub const WhileData = struct {
    body: Index,
    label_token: OptionalTokenIndex,
    capture_token: OptionalTokenIndex,
    continue_expr: OptionalIndex,
    flags: WhileFlags,
};

pub const WhileFlags = packed struct(u32) {
    capture_is_ptr: bool = false,
    _padding: u31 = 0,
};

pub const ForData = struct {
    binding_token: TokenIndex,
    index_binding_token: OptionalTokenIndex,
    iterable: OptionalIndex,
    range_start: OptionalIndex,
    range_end: OptionalIndex,
    body: Index,
    label_token: OptionalTokenIndex,
    flags: ForFlags,
};

pub const ForFlags = packed struct(u32) {
    is_inline: bool = false,
    is_await: bool = false,
    _padding: u30 = 0,
};

pub const BreakData = struct {
    label_token: OptionalTokenIndex,
    value: OptionalIndex,
};

pub const DestructureData = struct {
    bindings: SubRange, // alternating (name_token, type_expr OptionalIndex) pairs
    value: Index,
    flags: DestructureFlags,
};

pub const DestructureFlags = packed struct(u32) {
    is_const: bool = false,
    _padding: u31 = 0,
};

/// String interpolation segment in extra_data.
/// Tag word: 0 = text segment (next word = token index),
///           1 = expr segment (next word = node index).
pub const SEGMENT_TEXT: u32 = 0;
pub const SEGMENT_EXPR: u32 = 1;

// ============================================================================
// BuiltinKind — compiler intrinsics
// ============================================================================

/// Compiler builtin functions (@sizeOf, @intCast, @panic, etc.).
///
/// Source names are camelCase (@sizeOf, @intFromEnum). Enum tags are snake_case
/// (size_of, int_from_enum). The mapping is generated at comptime — 59 of 61
/// builtins follow mechanical snake_to_camel conversion. Only 2 exceptions:
///   type_of -> "TypeOf" (capital T)
///   atomic_cas -> "atomicCAS" (all-caps CAS)
pub const BuiltinKind = enum(u32) {
    size_of,
    align_of,
    enum_len,
    int_cast,
    float_cast,
    float_from_int,
    int_from_float,
    ptr_cast,
    int_to_ptr,
    ptr_to_int,
    string,
    assert,
    assert_eq,
    ptr_of,
    len_of,
    trap,
    target_os,
    target_arch,
    target,
    compile_error,
    embed_file,
    abs,
    ceil,
    floor,
    trunc,
    round,
    sqrt,
    fmin,
    fmax,
    has_field,
    type_of,
    field,
    type_name,
    enum_name,
    type_info,
    int_from_enum,
    enum_from_int,
    tag_name,
    error_name,
    int_from_bool,
    bit_cast,
    truncate,
    as,
    offset_of,
    min,
    max,
    align_cast,
    const_cast,
    arc_retain,
    arc_release,
    is_unique,
    panic,
    ctz,
    clz,
    pop_count,
    atomic_load,
    atomic_store,
    atomic_add,
    atomic_cas,
    atomic_exchange,

    /// Get the user-facing source name (@sizeOf, @intCast, etc.).
    pub fn sourceName(self: BuiltinKind) []const u8 {
        return switch (self) {
            .type_of => "TypeOf",
            .atomic_cas => "atomicCAS",
            inline else => |tag| comptime snakeToCamel(@tagName(tag)),
        };
    }

    /// Reverse lookup: source name string -> enum.
    pub const map = blk: {
        @setEvalBranchQuota(10_000);
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

fn snakeToCamel(comptime input: []const u8) *const [comptimeCamelLen(input)]u8 {
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

// ============================================================================
// Ast — the complete parse result
// ============================================================================

pub const Ast = struct {
    /// Reference to externally-owned source text.
    source: [:0]const u8,
    tokens: TokenList.Slice,
    nodes: NodeList.Slice,
    extra_data: []const u32,
    errors: []const Error,
    safe_mode: bool,

    pub const TokenList = std.MultiArrayList(struct {
        tag: Token,
        start: u32, // byte offset into source
    });

    pub const NodeList = std.MultiArrayList(Node);

    pub const Error = struct {
        token: TokenIndex,
        tag: ErrorTag,
        token_is_prev: bool = false,
    };

    pub const ErrorTag = enum {
        expected_semicolon,
        expected_comma,
        expected_identifier,
        expected_expression,
        expected_type,
        expected_block,
        expected_rparen,
        expected_rbrace,
        expected_rbracket,
        unexpected_token,
        max_nesting_exceeded,
    };

    pub fn deinit(self: *Ast, gpa: std.mem.Allocator) void {
        self.tokens.deinit(gpa);
        self.nodes.deinit(gpa);
        gpa.free(self.extra_data);
        gpa.free(self.errors);
        self.* = undefined;
    }

    // --- Token accessors ---

    pub fn tokenTag(self: *const Ast, ti: TokenIndex) Token {
        return self.tokens.items(.tag)[ti];
    }

    pub fn tokenStart(self: *const Ast, ti: TokenIndex) u32 {
        return self.tokens.items(.start)[ti];
    }

    /// Get the source text for a token.
    pub fn tokenSlice(self: *const Ast, ti: TokenIndex) []const u8 {
        const start = self.tokenStart(ti);
        // For most tokens, we can determine the end by scanning forward.
        // Keywords and operators have known lengths from the tag.
        const tag = self.tokenTag(ti);
        if (tag.string().len > 0 and tag.isKeyword()) {
            return self.source[start .. start + tag.string().len];
        }
        // For identifiers, numbers, strings: scan to next token start
        if (ti + 1 < self.tokens.len) {
            // Walk backward from next token to skip whitespace
            var end = self.tokenStart(ti + 1);
            while (end > start and (self.source[end - 1] == ' ' or self.source[end - 1] == '\n' or self.source[end - 1] == '\t' or self.source[end - 1] == '\r')) {
                end -= 1;
            }
            return self.source[start..end];
        }
        return self.source[start..];
    }

    // --- Node accessors ---

    pub fn nodeTag(self: *const Ast, node: Index) Node.Tag {
        return self.nodes.items(.tag)[@intFromEnum(node)];
    }

    pub fn nodeMainToken(self: *const Ast, node: Index) TokenIndex {
        return self.nodes.items(.main_token)[@intFromEnum(node)];
    }

    pub fn nodeData(self: *const Ast, node: Index) Node.Data {
        return self.nodes.items(.data)[@intFromEnum(node)];
    }

    // --- Extra data unpacker ---

    /// Read a struct from extra_data starting at `index`.
    /// Uses comptime reflection to read consecutive u32 fields.
    /// SubRange fields consume 2 u32 slots (start + end); all others consume 1.
    pub fn extraData(self: *const Ast, index: ExtraIndex, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        var offset: usize = @intFromEnum(index);
        inline for (fields) |f| {
            if (f.type == SubRange) {
                @field(result, f.name) = .{
                    .start = @enumFromInt(self.extra_data[offset]),
                    .end = @enumFromInt(self.extra_data[offset + 1]),
                };
                offset += 2;
            } else {
                const raw = self.extra_data[offset];
                @field(result, f.name) = switch (f.type) {
                    Index => @enumFromInt(raw),
                    OptionalIndex => @enumFromInt(raw),
                    OptionalTokenIndex => @enumFromInt(raw),
                    ExtraIndex => @enumFromInt(raw),
                    u32 => raw,
                    FnFlags, StructFlags, FieldFlags, IfFlags, SwitchCaseFlags,
                    VarFlags, LocalVarFlags, WhileFlags, ForFlags, ClosureFlags,
                    TaskFlags, CatchFlags, NewFlags, DestructureFlags,
                    => @bitCast(raw),
                    else => @compileError("unexpected extra_data field type: " ++ @typeName(f.type)),
                };
                offset += 1;
            }
        }
        return result;
    }

    /// Get a slice of node indices from a SubRange.
    pub fn extraSlice(self: *const Ast, range: SubRange) []const u32 {
        return self.extra_data[@intFromEnum(range.start)..@intFromEnum(range.end)];
    }

    /// Get a slice of node Indices from a SubRange.
    pub fn extraNodes(self: *const Ast, range: SubRange) []const Index {
        const raw = self.extraSlice(range);
        return @ptrCast(raw);
    }

    /// Compute the source span for any node.
    pub fn nodeSpan(self: *const Ast, node: Index) Span {
        const main = self.nodeMainToken(node);
        const start_offset = self.tokenStart(main);
        // For a simple implementation, use main_token position.
        // Full firstToken/lastToken traversal would be more precise
        // but requires per-tag logic (TODO: implement like Zig).
        return Span.init(
            Pos{ .offset = start_offset },
            Pos{ .offset = start_offset + @as(u32, @intCast(self.tokenSlice(main).len)) },
        );
    }

    /// Check if the file uses async features.
    pub fn hasAsyncFunctions(self: *const Ast) bool {
        const tags = self.nodes.items(.tag);
        for (tags) |tag| {
            switch (tag) {
                .fn_decl => {
                    // Check FnFlags in extra data
                    // TODO: implement when parser wires this up
                },
                .unary_await, .closure_expr => return true,
                else => {},
            }
        }
        return false;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "index type safety" {
    const idx: Index = @enumFromInt(42);
    const opt = idx.toOptional();
    try std.testing.expectEqual(idx, opt.unwrap().?);
    try std.testing.expect(OptionalIndex.none.unwrap() == null);
}

test "SubRange length" {
    const range = SubRange{
        .start = @enumFromInt(5),
        .end = @enumFromInt(10),
    };
    try std.testing.expectEqual(@as(u32, 5), range.len());
}

test "tag fits in one byte" {
    try std.testing.expectEqual(@as(usize, 1), @sizeOf(Node.Tag));
}

test "builtin source names" {
    try std.testing.expectEqualStrings("sizeOf", BuiltinKind.size_of.sourceName());
    try std.testing.expectEqualStrings("intFromEnum", BuiltinKind.int_from_enum.sourceName());
    try std.testing.expectEqualStrings("TypeOf", BuiltinKind.type_of.sourceName());
    try std.testing.expectEqualStrings("atomicCAS", BuiltinKind.atomic_cas.sourceName());
    try std.testing.expectEqualStrings("abs", BuiltinKind.abs.sourceName());
}

test "builtin lookup" {
    try std.testing.expectEqual(BuiltinKind.size_of, BuiltinKind.fromString("sizeOf").?);
    try std.testing.expectEqual(BuiltinKind.type_of, BuiltinKind.fromString("TypeOf").?);
    try std.testing.expectEqual(BuiltinKind.atomic_cas, BuiltinKind.fromString("atomicCAS").?);
    try std.testing.expect(BuiltinKind.fromString("notABuiltin") == null);
}

test "snakeToCamel" {
    try std.testing.expectEqualStrings("sizeOf", comptime snakeToCamel("size_of"));
    try std.testing.expectEqualStrings("intFromEnum", comptime snakeToCamel("int_from_enum"));
    try std.testing.expectEqualStrings("abs", comptime snakeToCamel("abs"));
}

test "extraData round-trip with SubRange" {
    // Simulate packing an EnumDecl into extra_data
    const extra = [_]u32{
        42, // name_token
        @intFromEnum(OptionalIndex.none), // backing_type
        5, 10, // variants SubRange (start=5, end=10)
        20, 25, // nested_decls SubRange (start=20, end=25)
        @intFromEnum(OptionalTokenIndex.none), // doc_comment
    };
    // Create a mock Ast just for the extraData method
    const ast_val: Ast = .{
        .source = "test",
        .tokens = undefined,
        .nodes = undefined,
        .extra_data = &extra,
        .errors = &.{},
        .safe_mode = false,
    };
    const result = ast_val.extraData(@enumFromInt(0), EnumDecl);
    try std.testing.expectEqual(@as(TokenIndex, 42), result.name_token);
    try std.testing.expect(result.backing_type.unwrap() == null);
    try std.testing.expectEqual(@as(u32, 5), result.variants.len());
    try std.testing.expectEqual(@as(u32, 5), result.nested_decls.len());
    try std.testing.expect(result.doc_comment.unwrap() == null);
}

test "extraData with packed flags" {
    const flags = FnFlags{ .is_async = true, .is_static = true };
    const extra = [_]u32{
        0, // name_token
        0, 0, // type_params SubRange
        0, 0, // param_bounds SubRange
        0, 0, // params SubRange
        @intFromEnum(OptionalIndex.none), // return_type
        @bitCast(flags), // flags
        @intFromEnum(OptionalTokenIndex.none), // doc_comment
        @intFromEnum(OptionalTokenIndex.none), // global_actor
    };
    const ast_val: Ast = .{
        .source = "test",
        .tokens = undefined,
        .nodes = undefined,
        .extra_data = &extra,
        .errors = &.{},
        .safe_mode = false,
    };
    const result = ast_val.extraData(@enumFromInt(0), FnDecl);
    try std.testing.expect(result.flags.is_async);
    try std.testing.expect(result.flags.is_static);
    try std.testing.expect(!result.flags.is_export);
    try std.testing.expect(!result.flags.is_inlinable);
}
