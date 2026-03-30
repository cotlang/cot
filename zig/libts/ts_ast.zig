//! TypeScript/JavaScript AST node definitions.
//!
//! These are intermediate nodes produced by the libts parser before
//! transformation to Cot AST nodes. The transform layer converts:
//!   TS AST → Cot AST (ast.zig nodes)
//!
//! Design: Array-based with NodeIndex (same pattern as Cot AST).
//! Nodes are stored in a flat ArrayList, referenced by u32 index.

const std = @import("std");
const source = @import("source.zig");

const Span = source.Span;
const Pos = source.Pos;

pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(u32);

/// A list of node indices, allocated from the AST allocator.
pub const NodeList = []const NodeIndex;

// ============================================================================
// Top-level node: tagged union of decl/expr/stmt/type
// ============================================================================

pub const Node = union(enum) {
    decl: Decl,
    expr: Expr,
    stmt: Stmt,
    type_node: TypeNode,
    class_member: ClassMember,
    pattern: Pattern,
};

// ============================================================================
// Declarations
// ============================================================================

pub const Decl = union(enum) {
    function: FunctionDecl,
    variable: VarDecl,
    class: ClassDecl,
    interface: InterfaceDecl,
    type_alias: TypeAliasDecl,
    enum_decl: EnumDecl,
    import_decl: ImportDecl,
    export_decl: ExportDecl,
    namespace: NamespaceDecl,

    pub const FunctionDecl = struct {
        name: ?[]const u8,
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex = null_node,
        body: NodeIndex = null_node, // block statement
        is_async: bool = false,
        is_generator: bool = false,
        is_export: bool = false,
        is_default: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,
    };

    pub const VarDecl = struct {
        kind: VarKind,
        declarators: []const Declarator = &.{},
        is_export: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,

        pub const VarKind = enum { var_kw, let, const_kw };
    };

    pub const Declarator = struct {
        binding: NodeIndex, // ident expr or destructuring pattern
        type_ann: NodeIndex = null_node,
        init: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const ClassDecl = struct {
        name: ?[]const u8,
        type_params: NodeList = &.{},
        extends: NodeIndex = null_node, // expression with optional type args
        implements: NodeList = &.{}, // type references
        members: []const NodeIndex = &.{}, // ClassMember nodes
        is_abstract: bool = false,
        is_export: bool = false,
        is_default: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,
    };

    pub const InterfaceDecl = struct {
        name: []const u8,
        type_params: NodeList = &.{},
        extends: NodeList = &.{}, // type references
        members: []const InterfaceMember = &.{},
        is_export: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,
    };

    pub const InterfaceMember = struct {
        kind: MemberKind,
        name: ?[]const u8,
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex = null_node,
        type_ann: NodeIndex = null_node,
        is_optional: bool = false,
        is_readonly: bool = false,
        span: Span = Span.zero,

        pub const MemberKind = enum {
            property,
            method,
            call_signature,
            construct_signature,
            index_signature,
        };
    };

    pub const TypeAliasDecl = struct {
        name: []const u8,
        type_params: NodeList = &.{},
        type_node: NodeIndex,
        is_export: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,
    };

    pub const EnumDecl = struct {
        name: []const u8,
        members: []const EnumMember = &.{},
        is_const: bool = false,
        is_export: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,
    };

    pub const EnumMember = struct {
        name: []const u8,
        init: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const ImportDecl = struct {
        kind: ImportKind,
        module_specifier: []const u8,
        default_binding: ?[]const u8 = null,
        namespace_binding: ?[]const u8 = null,
        named_bindings: []const ImportBinding = &.{},
        is_type_only: bool = false,
        span: Span = Span.zero,

        pub const ImportKind = enum {
            value, // import { x } from 'mod'
            default, // import x from 'mod'
            namespace, // import * as x from 'mod'
            side_effect, // import 'mod'
        };
    };

    pub const ImportBinding = struct {
        name: []const u8,
        alias: ?[]const u8 = null,
        is_type: bool = false,
    };

    pub const ExportDecl = struct {
        kind: ExportKind,
        declaration: NodeIndex = null_node, // export const x = ...
        module_specifier: ?[]const u8 = null,
        named_bindings: []const ExportBinding = &.{},
        default_expr: NodeIndex = null_node, // export default expr
        is_type_only: bool = false,
        span: Span = Span.zero,

        pub const ExportKind = enum {
            declaration, // export function/class/const/etc
            default, // export default expr
            named, // export { x, y }
            all, // export * from 'mod'
            all_as, // export * as ns from 'mod'
        };
    };

    pub const ExportBinding = struct {
        name: []const u8,
        alias: ?[]const u8 = null,
        is_type: bool = false,
    };

    pub const NamespaceDecl = struct {
        name: []const u8,
        body: NodeIndex, // block or another namespace
        is_export: bool = false,
        is_declare: bool = false,
        span: Span = Span.zero,
    };
};

// ============================================================================
// Parameters (shared by functions, methods, constructors)
// ============================================================================

pub const Param = struct {
    binding: NodeIndex, // ident or destructuring pattern
    type_ann: NodeIndex = null_node,
    default_value: NodeIndex = null_node,
    is_rest: bool = false,
    is_optional: bool = false,
    is_readonly: bool = false,
    accessibility: Accessibility = .none,
    span: Span = Span.zero,
};

pub const Accessibility = enum { none, public, protected, private };

// ============================================================================
// Class Members
// ============================================================================

pub const ClassMember = union(enum) {
    property: PropertyDecl,
    method: MethodDecl,
    constructor: ConstructorDecl,
    getter: GetterSetterDecl,
    setter: GetterSetterDecl,
    index_signature: IndexSignatureDecl,
    static_block: StaticBlockDecl,

    pub const PropertyDecl = struct {
        name: NodeIndex, // expr (ident, string, computed)
        type_ann: NodeIndex = null_node,
        init: NodeIndex = null_node,
        is_static: bool = false,
        is_readonly: bool = false,
        is_optional: bool = false,
        is_abstract: bool = false,
        is_declare: bool = false,
        is_override: bool = false,
        accessibility: Accessibility = .none,
        span: Span = Span.zero,
    };

    pub const MethodDecl = struct {
        name: NodeIndex, // expr (ident, string, computed)
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex = null_node,
        body: NodeIndex = null_node, // block statement
        is_static: bool = false,
        is_async: bool = false,
        is_generator: bool = false,
        is_abstract: bool = false,
        is_optional: bool = false,
        is_override: bool = false,
        accessibility: Accessibility = .none,
        span: Span = Span.zero,
    };

    pub const ConstructorDecl = struct {
        params: []const Param = &.{},
        body: NodeIndex = null_node,
        accessibility: Accessibility = .none,
        span: Span = Span.zero,
    };

    pub const GetterSetterDecl = struct {
        name: NodeIndex,
        params: []const Param = &.{}, // setter has 1 param
        return_type: NodeIndex = null_node,
        body: NodeIndex = null_node,
        is_static: bool = false,
        is_abstract: bool = false,
        is_override: bool = false,
        accessibility: Accessibility = .none,
        span: Span = Span.zero,
    };

    pub const IndexSignatureDecl = struct {
        params: []const Param = &.{},
        type_ann: NodeIndex,
        is_static: bool = false,
        is_readonly: bool = false,
        span: Span = Span.zero,
    };

    pub const StaticBlockDecl = struct {
        body: NodeIndex, // block
        span: Span = Span.zero,
    };
};

// ============================================================================
// Expressions
// ============================================================================

pub const Expr = union(enum) {
    ident: Ident,
    literal: Literal,
    template: TemplateLiteral,
    binary: BinaryExpr,
    unary: UnaryExpr,
    update: UpdateExpr,
    assign: AssignExpr,
    conditional: ConditionalExpr,
    call: CallExpr,
    new_expr: NewExpr,
    member: MemberExpr,
    computed_member: ComputedMemberExpr,
    optional_member: OptionalMemberExpr,
    optional_call: OptionalCallExpr,
    array_literal: ArrayLiteral,
    object_literal: ObjectLiteral,
    arrow_function: ArrowFunction,
    function_expr: FunctionExpr,
    class_expr: ClassExpr,
    spread: SpreadExpr,
    yield_expr: YieldExpr,
    await_expr: AwaitExpr,
    paren: ParenExpr,
    sequence: SequenceExpr,
    as_expr: AsExpr,
    satisfies_expr: SatisfiesExpr,
    non_null: NonNullExpr,
    type_assertion: TypeAssertionExpr,
    typeof_expr: TypeofExpr,
    void_expr: VoidExpr,
    delete_expr: DeleteExpr,
    tagged_template: TaggedTemplateExpr,
    this_expr: ThisExpr,
    super_expr: SuperExpr,

    pub const Ident = struct {
        name: []const u8,
        span: Span = Span.zero,
    };

    pub const Literal = struct {
        kind: LiteralKind,
        value: []const u8, // raw text
        span: Span = Span.zero,

        pub const LiteralKind = enum {
            string,
            number,
            bigint,
            boolean,
            null_lit,
            undefined,
            regex,
        };
    };

    pub const TemplateLiteral = struct {
        quasis: []const []const u8, // static string parts
        expressions: NodeList, // dynamic expression parts
        span: Span = Span.zero,
    };

    pub const BinaryExpr = struct {
        op: BinaryOp,
        left: NodeIndex,
        right: NodeIndex,
        span: Span = Span.zero,
    };

    pub const BinaryOp = enum {
        add, sub, mul, div, rem, power,
        eql, neq, strict_eql, strict_neq,
        lt, lte, gt, gte,
        shl, shr, unsigned_shr,
        bitand, bitor, bitxor,
        land, lor,
        nullish_coalesce,
        instanceof, in_op,
    };

    pub const UnaryExpr = struct {
        op: UnaryOp,
        operand: NodeIndex,
        span: Span = Span.zero,

        pub const UnaryOp = enum {
            neg, pos, lnot, bitnot, typeof_op, void_op, delete_op,
        };
    };

    pub const UpdateExpr = struct {
        op: UpdateOp,
        operand: NodeIndex,
        prefix: bool,
        span: Span = Span.zero,

        pub const UpdateOp = enum { increment, decrement };
    };

    pub const AssignExpr = struct {
        op: AssignOp,
        left: NodeIndex,
        right: NodeIndex,
        span: Span = Span.zero,

        pub const AssignOp = enum {
            assign,
            add_assign, sub_assign, mul_assign, div_assign, rem_assign, power_assign,
            shl_assign, shr_assign, unsigned_shr_assign,
            bitand_assign, bitor_assign, bitxor_assign,
            land_assign, lor_assign, nullish_assign,
        };
    };

    pub const ConditionalExpr = struct {
        condition: NodeIndex,
        consequent: NodeIndex,
        alternate: NodeIndex,
        span: Span = Span.zero,
    };

    pub const CallExpr = struct {
        callee: NodeIndex,
        type_args: NodeList = &.{},
        args: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const NewExpr = struct {
        callee: NodeIndex,
        type_args: NodeList = &.{},
        args: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const MemberExpr = struct {
        object: NodeIndex,
        property: []const u8,
        span: Span = Span.zero,
    };

    pub const ComputedMemberExpr = struct {
        object: NodeIndex,
        property: NodeIndex, // expression
        span: Span = Span.zero,
    };

    pub const OptionalMemberExpr = struct {
        object: NodeIndex,
        property: []const u8,
        span: Span = Span.zero,
    };

    pub const OptionalCallExpr = struct {
        callee: NodeIndex,
        type_args: NodeList = &.{},
        args: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const ArrayLiteral = struct {
        elements: NodeList = &.{}, // may contain spread exprs
        span: Span = Span.zero,
    };

    pub const ObjectLiteral = struct {
        properties: []const ObjectProperty = &.{},
        span: Span = Span.zero,
    };

    pub const ObjectProperty = struct {
        kind: PropertyKind,
        key: NodeIndex, // ident, string lit, or computed
        value: NodeIndex = null_node,
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex = null_node,
        body: NodeIndex = null_node,
        is_computed: bool = false,
        is_shorthand: bool = false, // { x } ≡ { x: x }
        is_async: bool = false,
        is_generator: bool = false,
        span: Span = Span.zero,

        pub const PropertyKind = enum { init, get, set, method, spread };
    };

    pub const ArrowFunction = struct {
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex = null_node,
        body: NodeIndex, // block stmt or single expr
        is_async: bool = false,
        span: Span = Span.zero,
    };

    pub const FunctionExpr = struct {
        name: ?[]const u8 = null,
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex = null_node,
        body: NodeIndex = null_node,
        is_async: bool = false,
        is_generator: bool = false,
        span: Span = Span.zero,
    };

    pub const ClassExpr = struct {
        name: ?[]const u8 = null,
        type_params: NodeList = &.{},
        extends: NodeIndex = null_node,
        implements: NodeList = &.{},
        members: []const NodeIndex = &.{},
        span: Span = Span.zero,
    };

    pub const SpreadExpr = struct {
        operand: NodeIndex,
        span: Span = Span.zero,
    };

    pub const YieldExpr = struct {
        argument: NodeIndex = null_node,
        delegate: bool = false, // yield*
        span: Span = Span.zero,
    };

    pub const AwaitExpr = struct {
        argument: NodeIndex,
        span: Span = Span.zero,
    };

    pub const ParenExpr = struct {
        inner: NodeIndex,
        span: Span = Span.zero,
    };

    pub const SequenceExpr = struct {
        exprs: NodeList, // comma-separated: (a, b, c)
        span: Span = Span.zero,
    };

    pub const AsExpr = struct {
        expr: NodeIndex,
        type_node: NodeIndex,
        span: Span = Span.zero,
    };

    pub const SatisfiesExpr = struct {
        expr: NodeIndex,
        type_node: NodeIndex,
        span: Span = Span.zero,
    };

    pub const NonNullExpr = struct {
        expr: NodeIndex,
        span: Span = Span.zero,
    };

    pub const TypeAssertionExpr = struct {
        type_node: NodeIndex,
        expr: NodeIndex,
        span: Span = Span.zero,
    };

    pub const TypeofExpr = struct {
        operand: NodeIndex,
        span: Span = Span.zero,
    };

    pub const VoidExpr = struct {
        operand: NodeIndex,
        span: Span = Span.zero,
    };

    pub const DeleteExpr = struct {
        operand: NodeIndex,
        span: Span = Span.zero,
    };

    pub const TaggedTemplateExpr = struct {
        tag: NodeIndex,
        template: NodeIndex,
        type_args: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const ThisExpr = struct {
        span: Span = Span.zero,
    };

    pub const SuperExpr = struct {
        span: Span = Span.zero,
    };
};

// ============================================================================
// Statements
// ============================================================================

pub const Stmt = union(enum) {
    expr_stmt: ExprStmt,
    block: BlockStmt,
    return_stmt: ReturnStmt,
    if_stmt: IfStmt,
    while_stmt: WhileStmt,
    do_while: DoWhileStmt,
    for_stmt: ForStmt,
    for_in: ForInStmt,
    for_of: ForOfStmt,
    switch_stmt: SwitchStmt,
    throw_stmt: ThrowStmt,
    try_stmt: TryStmt,
    break_stmt: BreakStmt,
    continue_stmt: ContinueStmt,
    labeled: LabeledStmt,
    var_stmt: VarStmt,
    empty: EmptyStmt,
    debugger: DebuggerStmt,
    with_stmt: WithStmt,

    pub const ExprStmt = struct {
        expr: NodeIndex,
        span: Span = Span.zero,
    };

    pub const BlockStmt = struct {
        stmts: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const ReturnStmt = struct {
        value: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const IfStmt = struct {
        condition: NodeIndex,
        consequent: NodeIndex,
        alternate: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const WhileStmt = struct {
        condition: NodeIndex,
        body: NodeIndex,
        span: Span = Span.zero,
    };

    pub const DoWhileStmt = struct {
        body: NodeIndex,
        condition: NodeIndex,
        span: Span = Span.zero,
    };

    pub const ForStmt = struct {
        init: NodeIndex = null_node, // var decl or expr
        condition: NodeIndex = null_node,
        update: NodeIndex = null_node,
        body: NodeIndex,
        span: Span = Span.zero,
    };

    pub const ForInStmt = struct {
        left: NodeIndex, // var decl or expr
        right: NodeIndex,
        body: NodeIndex,
        span: Span = Span.zero,
    };

    pub const ForOfStmt = struct {
        left: NodeIndex, // var decl or expr
        right: NodeIndex,
        body: NodeIndex,
        is_await: bool = false,
        span: Span = Span.zero,
    };

    pub const SwitchStmt = struct {
        discriminant: NodeIndex,
        cases: []const SwitchCase = &.{},
        span: Span = Span.zero,
    };

    pub const SwitchCase = struct {
        @"test": NodeIndex = null_node, // null_node = default case
        consequent: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const ThrowStmt = struct {
        argument: NodeIndex,
        span: Span = Span.zero,
    };

    pub const TryStmt = struct {
        block: NodeIndex,
        handler: ?CatchClause = null,
        finalizer: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const CatchClause = struct {
        param: NodeIndex = null_node, // binding pattern or ident
        type_ann: NodeIndex = null_node, // TS: catch (e: Error)
        body: NodeIndex,
        span: Span = Span.zero,
    };

    pub const BreakStmt = struct {
        label: ?[]const u8 = null,
        span: Span = Span.zero,
    };

    pub const ContinueStmt = struct {
        label: ?[]const u8 = null,
        span: Span = Span.zero,
    };

    pub const LabeledStmt = struct {
        label: []const u8,
        body: NodeIndex,
        span: Span = Span.zero,
    };

    pub const VarStmt = struct {
        decl: NodeIndex, // points to Decl.variable
        span: Span = Span.zero,
    };

    pub const EmptyStmt = struct {
        span: Span = Span.zero,
    };

    pub const DebuggerStmt = struct {
        span: Span = Span.zero,
    };

    pub const WithStmt = struct {
        object: NodeIndex,
        body: NodeIndex,
        span: Span = Span.zero,
    };
};

// ============================================================================
// Type Nodes (TS type annotations)
// ============================================================================

pub const TypeNode = union(enum) {
    keyword: KeywordType,
    reference: TypeReference,
    array: ArrayType,
    tuple: TupleType,
    union_type: UnionType,
    intersection: IntersectionType,
    function_type: FunctionType,
    object_type: ObjectType,
    conditional: ConditionalType,
    mapped: MappedType,
    indexed_access: IndexedAccessType,
    literal: LiteralType,
    type_operator: TypeOperator,
    parenthesized: ParenthesizedType,
    infer_type: InferType,
    typeof_type: TypeofType,
    template_literal: TemplateLiteralType,
    type_predicate: TypePredicate,

    pub const KeywordType = struct {
        kind: KeywordKind,
        span: Span = Span.zero,

        pub const KeywordKind = enum {
            number, string, boolean, void_kw, null_kw, undefined,
            any, unknown, never, object, symbol, bigint,
        };
    };

    pub const TypeReference = struct {
        name: []const u8,
        type_args: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const ArrayType = struct {
        element: NodeIndex,
        span: Span = Span.zero,
    };

    pub const TupleType = struct {
        elements: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const UnionType = struct {
        types: NodeList,
        span: Span = Span.zero,
    };

    pub const IntersectionType = struct {
        types: NodeList,
        span: Span = Span.zero,
    };

    pub const FunctionType = struct {
        type_params: NodeList = &.{},
        params: []const Param = &.{},
        return_type: NodeIndex,
        span: Span = Span.zero,
    };

    pub const ObjectType = struct {
        members: []const Decl.InterfaceMember = &.{},
        span: Span = Span.zero,
    };

    pub const ConditionalType = struct {
        check_type: NodeIndex,
        extends_type: NodeIndex,
        true_type: NodeIndex,
        false_type: NodeIndex,
        span: Span = Span.zero,
    };

    pub const MappedType = struct {
        type_param: []const u8, // K
        constraint: NodeIndex, // keyof T
        name_type: NodeIndex = null_node, // as NewKey
        value_type: NodeIndex,
        is_readonly: Modifier = .none,
        is_optional: Modifier = .none,
        span: Span = Span.zero,

        pub const Modifier = enum { none, add, remove };
    };

    pub const IndexedAccessType = struct {
        object_type: NodeIndex,
        index_type: NodeIndex,
        span: Span = Span.zero,
    };

    pub const LiteralType = struct {
        value: NodeIndex, // literal expression
        span: Span = Span.zero,
    };

    pub const TypeOperator = struct {
        op: TypeOp,
        operand: NodeIndex,
        span: Span = Span.zero,

        pub const TypeOp = enum { keyof, unique, readonly };
    };

    pub const ParenthesizedType = struct {
        inner: NodeIndex,
        span: Span = Span.zero,
    };

    pub const InferType = struct {
        name: []const u8,
        constraint: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const TypeofType = struct {
        expr: NodeIndex,
        span: Span = Span.zero,
    };

    pub const TemplateLiteralType = struct {
        quasis: []const []const u8 = &.{},
        types: NodeList = &.{},
        span: Span = Span.zero,
    };

    pub const TypePredicate = struct {
        param_name: []const u8,
        type_node: NodeIndex,
        is_asserts: bool = false,
        span: Span = Span.zero,
    };
};

// ============================================================================
// Destructuring Patterns
// ============================================================================

pub const Pattern = union(enum) {
    object: ObjectPattern,
    array: ArrayPattern,
    rest: RestPattern,
    assign: AssignPattern,

    pub const ObjectPattern = struct {
        properties: []const ObjectPatternProp = &.{},
        type_ann: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const ObjectPatternProp = struct {
        key: NodeIndex, // ident or computed
        value: NodeIndex = null_node, // binding pattern (null = shorthand)
        default_value: NodeIndex = null_node,
        is_computed: bool = false,
        is_rest: bool = false,
        span: Span = Span.zero,
    };

    pub const ArrayPattern = struct {
        elements: NodeList = &.{}, // may contain null_node for holes
        type_ann: NodeIndex = null_node,
        span: Span = Span.zero,
    };

    pub const RestPattern = struct {
        argument: NodeIndex,
        span: Span = Span.zero,
    };

    pub const AssignPattern = struct {
        left: NodeIndex, // ident or pattern
        right: NodeIndex, // default value
        span: Span = Span.zero,
    };
};

// ============================================================================
// Type Parameter
// ============================================================================

pub const TypeParam = struct {
    name: []const u8,
    constraint: NodeIndex = null_node, // extends clause
    default: NodeIndex = null_node,
    span: Span = Span.zero,
};

// ============================================================================
// AST Container
// ============================================================================

pub const Ast = struct {
    nodes: std.ArrayListUnmanaged(Node),
    allocator: std.mem.Allocator,
    source_file: ?SourceFile = null,

    pub const SourceFile = struct {
        filename: []const u8,
        statements: NodeList,
        span: Span,
    };

    pub fn init(allocator: std.mem.Allocator) Ast {
        return .{
            .nodes = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Ast) void {
        self.nodes.deinit(self.allocator);
    }

    pub fn addNode(self: *Ast, node: Node) !NodeIndex {
        const idx: NodeIndex = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return idx;
    }

    pub fn getNode(self: *const Ast, idx: NodeIndex) ?*const Node {
        if (idx == null_node) return null;
        if (idx >= self.nodes.items.len) return null;
        return &self.nodes.items[idx];
    }

    pub fn addDecl(self: *Ast, decl: Decl) !NodeIndex {
        return self.addNode(.{ .decl = decl });
    }

    pub fn addExpr(self: *Ast, expr: Expr) !NodeIndex {
        return self.addNode(.{ .expr = expr });
    }

    pub fn addStmt(self: *Ast, stmt: Stmt) !NodeIndex {
        return self.addNode(.{ .stmt = stmt });
    }

    pub fn addType(self: *Ast, type_node: TypeNode) !NodeIndex {
        return self.addNode(.{ .type_node = type_node });
    }

    pub fn addClassMember(self: *Ast, member: ClassMember) !NodeIndex {
        return self.addNode(.{ .class_member = member });
    }

    pub fn addPattern(self: *Ast, pattern: Pattern) !NodeIndex {
        return self.addNode(.{ .pattern = pattern });
    }

    /// Allocate a slice of NodeIndex from this AST's allocator.
    pub fn allocNodeList(self: *Ast, items: []const NodeIndex) !NodeList {
        const duped = try self.allocator.alloc(NodeIndex, items.len);
        @memcpy(duped, items);
        return duped;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ast basic node creation" {
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    const ident_idx = try ast.addExpr(.{ .ident = .{ .name = "x" } });
    try std.testing.expectEqual(@as(NodeIndex, 0), ident_idx);

    const node = ast.getNode(ident_idx);
    try std.testing.expect(node != null);

    try std.testing.expect(ast.getNode(null_node) == null);
}

test "ast function decl" {
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    const body_idx = try ast.addStmt(.{ .block = .{} });
    const fn_idx = try ast.addDecl(.{ .function = .{
        .name = "hello",
        .body = body_idx,
    } });
    try std.testing.expectEqual(@as(NodeIndex, 1), fn_idx);

    const node = ast.getNode(fn_idx).?;
    switch (node.*) {
        .decl => |d| switch (d) {
            .function => |f| {
                try std.testing.expectEqualStrings("hello", f.name.?);
                try std.testing.expectEqual(body_idx, f.body);
            },
            else => return error.UnexpectedNode,
        },
        else => return error.UnexpectedNode,
    }
}

test "ast class decl with members" {
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    const name_idx = try ast.addExpr(.{ .ident = .{ .name = "count" } });
    const prop = try ast.addClassMember(.{ .property = .{
        .name = name_idx,
        .is_static = true,
    } });

    const members = try ast.allocator.alloc(NodeIndex, 1);
    defer ast.allocator.free(members);
    members[0] = prop;

    const class_idx = try ast.addDecl(.{ .class = .{
        .name = "Counter",
        .members = members,
    } });

    const node = ast.getNode(class_idx).?;
    switch (node.*) {
        .decl => |d| switch (d) {
            .class => |c| {
                try std.testing.expectEqualStrings("Counter", c.name.?);
                try std.testing.expectEqual(@as(usize, 1), c.members.len);
            },
            else => return error.UnexpectedNode,
        },
        else => return error.UnexpectedNode,
    }
}

test "ast type nodes" {
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    const num_type = try ast.addType(.{ .keyword = .{ .kind = .number } });
    const str_type = try ast.addType(.{ .keyword = .{ .kind = .string } });

    const union_types = try ast.allocator.alloc(NodeIndex, 2);
    defer ast.allocator.free(union_types);
    union_types[0] = num_type;
    union_types[1] = str_type;

    const union_idx = try ast.addType(.{ .union_type = .{ .types = union_types } });
    const node = ast.getNode(union_idx).?;
    switch (node.*) {
        .type_node => |t| switch (t) {
            .union_type => |u| try std.testing.expectEqual(@as(usize, 2), u.types.len),
            else => return error.UnexpectedNode,
        },
        else => return error.UnexpectedNode,
    }
}
