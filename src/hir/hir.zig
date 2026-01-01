//! High-level Intermediate Representation (HIR)
//!
//! HIR is positioned between the AST and MIR (the current IR):
//!   Source -> AST -> HIR -> MIR -> Bytecode
//!
//! HIR provides:
//! 1. **Explicit Types**: Every expression has a resolved type
//! 2. **Desugared Control Flow**: for/while unified to loop
//! 3. **Desugared Operators**: += becomes x = x + 1
//! 4. **Resolved Names**: Variable references linked to declarations
//!
//! This makes MIR lowering simpler - it doesn't need to handle:
//! - Type inference
//! - Name resolution
//! - Control flow desugaring

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// Types
// ============================================================================

/// HIR Type - fully resolved types (no type inference needed)
pub const Type = union(enum) {
    /// Void type (for functions with no return)
    void: void,

    /// Boolean type
    bool: void,

    /// Signed integers
    i8: void,
    i16: void,
    i32: void,
    i64: void,

    /// Unsigned integers
    u8: void,
    u16: void,
    u32: void,
    u64: void,

    /// Floating point
    f32: void,
    f64: void,

    /// String (dynamic length)
    string: void,

    /// Fixed-length string (for DBL compatibility)
    string_fixed: u32,

    /// Decimal type (for financial calculations)
    decimal: struct {
        precision: u32,
        scale: u8,
    },

    /// Pointer to another type
    ptr: *const Type,

    /// Optional type (nullable)
    optional: *const Type,

    /// Array type
    array: struct {
        element: *const Type,
        length: u32,
    },

    /// Struct type (by name reference)
    @"struct": []const u8,

    /// Function type
    function: struct {
        params: []const Type,
        return_type: *const Type,
    },

    /// Check if this is a numeric type
    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .decimal => true,
            else => false,
        };
    }

    /// Check if this is a string type
    pub fn isString(self: Type) bool {
        return switch (self) {
            .string, .string_fixed => true,
            else => false,
        };
    }
};

// ============================================================================
// Expressions
// ============================================================================

/// Typed expression - every expression has an explicit type
pub const Expr = struct {
    kind: ExprKind,
    ty: Type,
    loc: SourceLoc,
};

/// Expression kinds
pub const ExprKind = union(enum) {
    // Literals
    int_literal: i64,
    float_literal: f64,
    string_literal: []const u8,
    bool_literal: bool,
    null_literal: void,

    // Variable reference (resolved to declaration)
    variable: struct {
        name: []const u8,
        decl: ?*const VarDecl, // Link to declaration (null for globals/externals)
    },

    // Binary operation
    binary: struct {
        op: BinaryOp,
        lhs: *const Expr,
        rhs: *const Expr,
    },

    // Unary operation
    unary: struct {
        op: UnaryOp,
        operand: *const Expr,
    },

    // Function/method call
    call: struct {
        callee: []const u8,
        args: []const *const Expr,
        is_method: bool,
        receiver: ?*const Expr, // For method calls
    },

    // Member access (struct.field)
    member: struct {
        object: *const Expr,
        field: []const u8,
        field_index: u32, // Resolved field index
    },

    // Array/string indexing
    index: struct {
        object: *const Expr,
        index: *const Expr,
    },

    // Type cast
    cast: struct {
        operand: *const Expr,
        target_type: Type,
    },

    // Address-of operator
    addr_of: *const Expr,

    // Dereference operator
    deref: *const Expr,
};

/// Binary operators
pub const BinaryOp = enum {
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,

    // Comparison
    eq,
    ne,
    lt,
    le,
    gt,
    ge,

    // Logical
    @"and",
    @"or",

    // Bitwise
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
};

/// Unary operators
pub const UnaryOp = enum {
    neg,
    not,
    bit_not,
};

// ============================================================================
// Statements
// ============================================================================

/// Statement kinds (desugared)
pub const Stmt = union(enum) {
    // Variable declaration
    var_decl: VarDecl,

    // Assignment (target = value)
    assign: struct {
        target: *const Expr,
        value: *const Expr,
    },

    // Expression statement (for side effects)
    expr_stmt: *const Expr,

    // Block of statements
    block: []const *const Stmt,

    // Unified loop (for/while desugared to this)
    loop: struct {
        /// Loop variable (optional, for 'for' loops)
        loop_var: ?*const VarDecl,
        /// Condition to continue (null for infinite loop)
        condition: ?*const Expr,
        /// Body statements
        body: []const *const Stmt,
        /// Increment expression (optional, for 'for' loops)
        increment: ?*const Expr,
    },

    // Conditional
    if_stmt: struct {
        condition: *const Expr,
        then_body: []const *const Stmt,
        else_body: ?[]const *const Stmt,
    },

    // Return
    return_stmt: ?*const Expr,

    // Break (with optional value for block expressions)
    break_stmt: ?*const Expr,

    // Continue
    continue_stmt: void,

    // I/O operations
    io_open: IoOpen,
    io_close: IoClose,
    io_read: IoRead,
    io_write: IoWrite,

    // Try/catch
    try_stmt: struct {
        body: []const *const Stmt,
        catch_var: ?[]const u8,
        catch_body: []const *const Stmt,
    },

    // Throw
    throw_stmt: *const Expr,
};

/// Variable declaration
pub const VarDecl = struct {
    name: []const u8,
    ty: Type,
    init: ?*const Expr,
    is_mutable: bool,
};

// ============================================================================
// I/O Statements
// ============================================================================

pub const IoOpen = struct {
    channel: *const Expr,
    filename: *const Expr,
    mode: OpenMode,

    pub const OpenMode = enum { input, output, update, append, indexed };
};

pub const IoClose = struct {
    channel: *const Expr,
};

pub const IoRead = struct {
    channel: *const Expr,
    target: *const Expr, // Variable to read into
    key: ?*const Expr,
};

pub const IoWrite = struct {
    channel: *const Expr,
    value: *const Expr,
    is_insert: bool, // true for store, false for write/update
};

// ============================================================================
// Functions and Modules
// ============================================================================

/// Function definition
pub const Function = struct {
    name: []const u8,
    params: []const Param,
    return_type: Type,
    body: []const *const Stmt,
    is_public: bool,

    pub const Param = struct {
        name: []const u8,
        ty: Type,
    };
};

/// Structure definition
pub const StructDef = struct {
    name: []const u8,
    fields: []const Field,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        offset: u32, // Byte offset within struct
    };
};

/// HIR Module - the top-level compilation unit
pub const Module = struct {
    name: []const u8,
    structs: std.ArrayListUnmanaged(StructDef),
    functions: std.ArrayListUnmanaged(Function),
    globals: std.ArrayListUnmanaged(VarDecl),
    allocator: Allocator,

    pub fn init(allocator: Allocator, name: []const u8) Module {
        return .{
            .name = name,
            .structs = .{},
            .functions = .{},
            .globals = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Module) void {
        self.structs.deinit(self.allocator);
        self.functions.deinit(self.allocator);
        self.globals.deinit(self.allocator);
    }
};

// ============================================================================
// Source Location
// ============================================================================

pub const SourceLoc = struct {
    line: u32 = 0,
    column: u32 = 0,
    file: ?[]const u8 = null,

    pub const zero = SourceLoc{};
};

// ============================================================================
// Tests
// ============================================================================

test "Type.isNumeric" {
    try std.testing.expect(Type.i32.isNumeric());
    try std.testing.expect(Type.f64.isNumeric());
    try std.testing.expect(!Type.string.isNumeric());
    try std.testing.expect(!Type.bool.isNumeric());
}

test "Type.isString" {
    try std.testing.expect(Type.string.isString());
    try std.testing.expect((Type{ .string_fixed = 30 }).isString());
    try std.testing.expect(!Type.i32.isString());
}
