//! Abstract Syntax Tree types for Cot
//!
//! Represents the parsed structure of a Cot (.cot) program.

const std = @import("std");
const Token = @import("../lexer/token.zig").Token;

/// Source location for debugging
pub const SourceLoc = struct {
    line: u32,
    column: u32,

    pub const none: SourceLoc = .{ .line = 0, .column = 0 };
};

/// A complete Cot program
pub const Program = struct {
    statements: []Statement,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .statements = &[_]Statement{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements) |*stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.statements);
    }
};

/// Statement types in Cot
pub const Statement = union(enum) {
    // Data division
    record: RecordDef,
    structure: StructureDef,
    group: GroupDef,
    field: FieldDef,
    literal: LiteralDef,
    common: CommonDef,

    // Procedure division
    proc: ProcDef,
    assignment: Assignment,
    if_stmt: IfStatement,
    case_stmt: CaseStatement,
    loop: LoopStatement,
    call: CallStatement,
    xcall: XCallStatement,
    return_stmt: ReturnStatement,
    goto_stmt: GotoStatement,
    label: LabelDef,

    // I/O
    open_stmt: OpenStatement,
    close_stmt: CloseStatement,
    read_stmt: ReadStatement,
    write_stmt: WriteStatement,
    store_stmt: StoreStatement,
    delete_stmt: DeleteStatement,

    // Data manipulation
    clear_stmt: ClearStatement,
    init_stmt: InitStatement,
    incr_stmt: IncrStatement,

    // OOP
    class: ClassDef,
    method: MethodDef,
    namespace: NamespaceDef,

    // Function/Subroutine definitions
    function_def: FunctionDef,
    subroutine_def: SubroutineDef,

    // Import statement
    import_stmt: ImportStatement,

    // Compile-time
    const_decl: ConstDecl,
    comptime_if: CompTimeIf,

    // Error handling
    onerror_stmt: OnerrorStatement,
    offerror_stmt: void, // No data needed

    // Expression statement
    expression: Expression,

    // Block
    block: Block,

    // Loop control
    break_stmt: SourceLoc,
    continue_stmt: SourceLoc,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .block => |*b| b.deinit(allocator),
            .if_stmt => |*i| i.deinit(allocator),
            .expression => |*e| e.deinit(allocator),
            .assignment => |*a| {
                var target = a.target;
                var value = a.value;
                target.deinit(allocator);
                value.deinit(allocator);
            },
            .record => |r| allocator.free(r.fields),
            .group => |g| allocator.free(g.fields),
            .xcall => |x| {
                for (x.arguments) |*arg| {
                    var a = arg.*;
                    a.deinit(allocator);
                }
                allocator.free(x.arguments);
            },
            .read_stmt => |r| {
                if (r.qualifiers.len > 0) allocator.free(r.qualifiers);
                if (r.error_list.len > 0) allocator.free(r.error_list);
            },
            .write_stmt => |w| if (w.qualifiers.len > 0) allocator.free(w.qualifiers),
            .open_stmt => |o| if (o.qualifiers.len > 0) allocator.free(o.qualifiers),
            .store_stmt => |s| if (s.qualifiers.len > 0) allocator.free(s.qualifiers),
            .function_def => |f| {
                allocator.free(f.parameters);
                for (f.body) |*stmt| stmt.deinit(allocator);
                allocator.free(f.body);
            },
            .subroutine_def => |s| {
                allocator.free(s.parameters);
                for (s.body) |*stmt| stmt.deinit(allocator);
                allocator.free(s.body);
            },
            .clear_stmt => |*c| {
                var target = c.target;
                target.deinit(allocator);
            },
            .incr_stmt => |*i| {
                var target = i.target;
                target.deinit(allocator);
                if (i.amount) |*amt| {
                    var a = amt.*;
                    a.deinit(allocator);
                }
            },
            .loop => |l| {
                l.body.deinit(allocator);
                allocator.destroy(l.body);
            },
            .const_decl => |*c| {
                c.value.deinit(allocator);
            },
            .comptime_if => |*ct| {
                ct.deinit(allocator);
            },
            else => {},
        }
    }
};

/// A block of statements
pub const Block = struct {
    statements: []Statement,

    pub fn deinit(self: *Block, allocator: std.mem.Allocator) void {
        for (self.statements) |*stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.statements);
    }
};

/// Record definition
pub const RecordDef = struct {
    name: ?[]const u8,
    fields: []FieldDef,
};

/// Structure definition (data layout template, no memory allocation)
/// Unlike RECORD, STRUCTURE just defines a type that can be used elsewhere
pub const StructureDef = struct {
    name: []const u8, // Structure name is required
    fields: []FieldDef,
};

/// Group definition (nested fields)
pub const GroupDef = struct {
    name: []const u8,
    fields: []FieldDef,
    overlay_target: ?[]const u8, // For overlays
};

/// Field definition
pub const FieldDef = struct {
    name: []const u8,
    data_type: DataType,
    initial_value: ?Expression,
    array_dims: ?[]usize,
};

/// Cot Data types
pub const DataType = union(enum) {
    alpha: AlphaType,
    decimal: DecimalType,
    implied_decimal: ImpliedDecimalType,
    integer: IntegerType,
    packed_decimal: PackedType,
    string: void,
    handle: void,
    structure_ref: []const u8, // @structname
    class_ref: []const u8, // @classname
};

pub const AlphaType = struct {
    size: ?usize, // null for a*, computed from initial value
};

pub const DecimalType = struct {
    size: ?usize, // null for d*, computed from initial value
};

pub const ImpliedDecimalType = struct {
    total_digits: usize,
    precision: usize,
};

pub const IntegerType = enum {
    i1,
    i2,
    i4,
    i8,
};

pub const PackedType = struct {
    size: usize,
    precision: ?usize,
};

/// Literal definition
pub const LiteralDef = struct {
    name: []const u8,
    data_type: DataType,
    value: Expression,
};

/// Common block definition
pub const CommonDef = struct {
    name: []const u8,
    fields: []FieldDef,
};

/// Procedure division marker
pub const ProcDef = struct {};

/// Assignment statement
pub const Assignment = struct {
    target: Expression,
    value: Expression,
    loc: SourceLoc = SourceLoc.none,
};

/// If statement
pub const IfStatement = struct {
    condition: *Expression,
    then_branch: *Statement,
    else_branch: ?*Statement,
    loc: SourceLoc = SourceLoc.none,

    pub fn deinit(self: *IfStatement, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
        self.then_branch.deinit(allocator);
        allocator.destroy(self.then_branch);
        if (self.else_branch) |eb| {
            eb.deinit(allocator);
            allocator.destroy(eb);
        }
    }
};

/// Compile-time constant declaration
/// Syntax: const NAME = value  or  .define NAME = value
pub const ConstDecl = struct {
    name: []const u8,
    value: Expression,
    is_public: bool = false,
    loc: SourceLoc = SourceLoc.none,
};

/// Compile-time conditional
/// Syntax: comptime if expr ... end  or  .if expr ... .end
pub const CompTimeIf = struct {
    condition: *Expression,
    then_body: []Statement,
    else_body: ?[]Statement,
    loc: SourceLoc = SourceLoc.none,

    pub fn deinit(self: *CompTimeIf, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
        for (self.then_body) |*stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.then_body);
        if (self.else_body) |else_stmts| {
            for (else_stmts) |*stmt| {
                var s = stmt.*;
                s.deinit(allocator);
            }
            allocator.free(else_stmts);
        }
    }
};

/// Compile-time builtin call
/// Syntax: @name(args...)  e.g. @defined(X), @os(), @sizeof(type)
pub const CompTimeBuiltin = struct {
    name: []const u8, // "defined", "os", "sizeof", etc.
    arguments: []Expression,
    loc: SourceLoc = SourceLoc.none,
};

/// Case statement
pub const CaseStatement = struct {
    selector: Expression,
    cases: []CaseBranch,
    default_branch: ?*Statement,
};

pub const CaseBranch = struct {
    values: []Expression,
    body: *Statement,
};

/// Loop statement (for, while, do-until, etc.)
pub const LoopStatement = struct {
    loop_type: LoopType,
    condition: ?Expression,
    init_expr: ?Expression,
    update_expr: ?Expression,
    body: *Statement,
    loc: SourceLoc = SourceLoc.none,
};

pub const LoopType = enum {
    do_forever,
    do_until,
    while_loop,
    for_from_thru,
    for_do,
    foreach,
};

/// CALL statement (internal subroutine)
pub const CallStatement = struct {
    label: []const u8,
    loc: SourceLoc = SourceLoc.none,
};

/// XCALL statement (external subroutine)
pub const XCallStatement = struct {
    routine_name: []const u8,
    arguments: []Expression,
    loc: SourceLoc = SourceLoc.none,
};

/// Return statements
pub const ReturnStatement = struct {
    return_type: ReturnType,
    value: ?Expression,
    loc: SourceLoc = SourceLoc.none,
};

pub const ReturnType = enum {
    xreturn,
    freturn,
    mreturn,
    simple_return,
};

/// GOTO statement
pub const GotoStatement = struct {
    label: []const u8,
    loc: SourceLoc = SourceLoc.none,
};

/// Label definition
pub const LabelDef = struct {
    name: []const u8,
};

/// ONERROR statement - sets up error handler
pub const OnerrorStatement = struct {
    label: []const u8, // Label to jump to on error
};

// I/O Statements
pub const OpenStatement = struct {
    channel: Expression,
    mode: []const u8,
    filename: Expression,
    qualifiers: []Qualifier,
    loc: SourceLoc = SourceLoc.none,
};

pub const CloseStatement = struct {
    channel: Expression,
    loc: SourceLoc = SourceLoc.none,
};

/// Error handler for I/O statements [[eof=label, err=handler]]
pub const ErrorHandler = struct {
    error_type: []const u8, // "eof", "err", "locked", etc.
    label: []const u8, // Jump target
};

pub const ReadStatement = struct {
    channel: Expression,
    record: Expression,
    key: ?Expression,
    qualifiers: []Qualifier,
    error_list: []ErrorHandler,
    loc: SourceLoc = SourceLoc.none,
};

pub const WriteStatement = struct {
    channel: Expression,
    record: Expression,
    key: ?Expression,
    qualifiers: []Qualifier,
    loc: SourceLoc = SourceLoc.none,
};

pub const StoreStatement = struct {
    channel: Expression,
    record: Expression,
    qualifiers: []Qualifier,
    loc: SourceLoc = SourceLoc.none,
};

pub const DeleteStatement = struct {
    channel: Expression,
    loc: SourceLoc = SourceLoc.none,
};

pub const Qualifier = struct {
    name: []const u8,
    value: ?Expression,
};

// Data manipulation statements
pub const ClearStatement = struct {
    target: Expression,
};

pub const InitStatement = struct {
    target: Expression,
};

pub const IncrStatement = struct {
    target: Expression,
    amount: ?Expression,
};

// OOP
pub const ClassDef = struct {
    name: []const u8,
    extends: ?[]const u8,
    implements: [][]const u8,
    members: []ClassMember,
};

pub const ClassMember = union(enum) {
    field: FieldDef,
    method: MethodDef,
    property: PropertyDef,
};

pub const MethodDef = struct {
    name: []const u8,
    access: AccessModifier,
    is_static: bool,
    return_type: ?DataType,
    parameters: []ParameterDef,
    body: []Statement,
};

pub const PropertyDef = struct {
    name: []const u8,
    access: AccessModifier,
    data_type: DataType,
    getter: ?[]Statement,
    setter: ?[]Statement,
};

pub const ParamDirection = enum {
    in, // Input only (read) - copy-in
    out, // Output only (write) - copy-out
    inout, // Both input and output - copy-in/copy-out (default for subroutines/functions)
};

pub const ParameterDef = struct {
    name: []const u8,
    data_type: DataType,
    direction: ParamDirection = .inout, // Default to INOUT for DBL compatibility
};

pub const AccessModifier = enum {
    public,
    private,
    protected,
    internal,
};

pub const NamespaceDef = struct {
    name: []const u8,
    members: []Statement,
};

/// Import statement for namespace imports
/// Syntax: import System
pub const ImportStatement = struct {
    namespace: []const u8, // e.g., "System"
};

/// Function definition (returns a value)
/// Syntax: function name, type [, export_name]
///         parameters...
///         endfunction
pub const FunctionDef = struct {
    name: []const u8,
    return_type: DataType,
    parameters: []ParameterDef,
    body: []Statement,
    export_name: ?[]const u8, // For .NET interop: .export "cot_function_name"
    is_static: bool,

    pub const Linkage = enum {
        internal, // Default, not exported
        export_c, // Export with C ABI for .NET P/Invoke
    };
};

/// Subroutine definition (no return value)
/// Syntax: subroutine name [, export_name]
///         parameters...
///         endsubroutine
pub const SubroutineDef = struct {
    name: []const u8,
    return_type: ?DataType, // Optional return type (unification with functions)
    parameters: []ParameterDef,
    body: []Statement,
    export_name: ?[]const u8, // For .NET interop
    is_external: bool, // EXTERNAL subroutine (declaration only)
};

/// Expression types
pub const Expression = union(enum) {
    // Literals
    integer: i64,
    decimal: []const u8, // Store as string to preserve precision
    string: []const u8,
    identifier: []const u8,
    null_literal: void,

    // Compound
    binary: *BinaryExpr,
    unary: *UnaryExpr,
    call: *CallExpr,
    builtin_call: *CallExpr, // %string(), %trim(), etc.
    comptime_builtin: *CompTimeBuiltin, // @defined(), @os(), etc.
    index: *IndexExpr,
    member: *MemberExpr,
    range: *RangeExpr,
    ambiguous: *AmbiguousCallOrRange, // Deferred resolution for name(arg1, arg2)

    // Special
    grouping: *Expression,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
                allocator.destroy(b);
            },
            .unary => |u| {
                u.operand.deinit(allocator);
                allocator.destroy(u);
            },
            .grouping => |g| {
                g.deinit(allocator);
                allocator.destroy(g);
            },
            .call, .builtin_call => |c| {
                for (c.arguments) |*arg| {
                    arg.deinit(allocator);
                }
                allocator.free(c.arguments);
                allocator.destroy(c);
            },
            .comptime_builtin => |ct| {
                for (ct.arguments) |*arg| {
                    arg.deinit(allocator);
                }
                allocator.free(ct.arguments);
                allocator.destroy(ct);
            },
            .index => |i| {
                var base = i.base;
                base.deinit(allocator);
                for (i.indices) |*idx| {
                    idx.deinit(allocator);
                }
                allocator.free(i.indices);
                allocator.destroy(i);
            },
            .member => |m| {
                var obj = m.object;
                obj.deinit(allocator);
                allocator.destroy(m);
            },
            .range => |r| {
                var base = r.base;
                var start = r.start;
                var end = r.end_or_length;
                base.deinit(allocator);
                start.deinit(allocator);
                end.deinit(allocator);
                allocator.destroy(r);
            },
            .ambiguous => |a| {
                var base = a.base;
                var first = a.first_arg;
                var second = a.second_arg;
                base.deinit(allocator);
                first.deinit(allocator);
                second.deinit(allocator);
                allocator.destroy(a);
            },
            else => {},
        }
    }
};

pub const BinaryExpr = struct {
    left: Expression,
    operator: BinaryOp,
    right: Expression,
};

pub const BinaryOp = enum {
    // Arithmetic
    add,
    subtract,
    multiply,
    divide,
    int_divide,
    modulo,
    power,

    // Comparison
    equal,
    not_equal,
    less_than,
    less_equal,
    greater_than,
    greater_equal,

    // Logical
    logical_and,
    logical_or,
    logical_xor,

    // String
    concat,
};

pub const UnaryExpr = struct {
    operator: UnaryOp,
    operand: Expression,
};

pub const UnaryOp = enum {
    negate,
    logical_not,
    bitwise_not,
};

pub const CallExpr = struct {
    callee: []const u8,
    arguments: []Expression,
    is_function: bool, // %function vs subroutine
    object: ?[]const u8 = null, // For method calls like Console.WriteLine, this is "Console"
};

pub const IndexExpr = struct {
    base: Expression,
    indices: []Expression,
};

pub const MemberExpr = struct {
    object: Expression,
    member: []const u8,
};

pub const RangeExpr = struct {
    base: Expression,
    start: Expression,
    end_or_length: Expression,
    is_length: bool, // true for (start:length) colon syntax, false for (start,end) comma syntax
};

/// Ambiguous expression that could be either a function call or a range (substring)
/// This is used for comma syntax like `name(arg1, arg2)` which could be:
/// - A function call: name(argument1, argument2)
/// - A substring range: name(start_position, end_position)
/// Resolution is deferred to semantic analysis phase where we can check the symbol table
pub const AmbiguousCallOrRange = struct {
    base: Expression, // The identifier or member expression being called/subscripted
    first_arg: Expression, // First argument (arg1 or start position)
    second_arg: Expression, // Second argument (arg2 or end position)
};

test "ast basic structure" {
    const allocator = std.testing.allocator;
    var program = Program.init(allocator);
    defer program.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), program.statements.len);
}
