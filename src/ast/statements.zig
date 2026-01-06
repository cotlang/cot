//! AST Statement Types
//!
//! Modern statement types only - no DBL-isms.
//! DBL syntax is mapped to these modern concepts by the cot-dbl frontend.

const std = @import("std");

/// Statement type tag for SoA storage
pub const StatementTag = enum(u8) {
    // ============================================
    // Declarations
    // ============================================

    /// Function definition: fn name(params) -> return_type { body }
    fn_def,

    /// Structure definition: struct Name { fields }
    struct_def,

    /// Union definition: union { fields } - fields share memory (overlays)
    union_def,

    /// Field view definition: view name = start..end - spans multiple fields
    field_view,

    /// Enumeration definition: enum Name { variants }
    enum_def,

    /// Constant declaration: const name = value
    const_decl,

    /// Variable declaration: let name = value, let mut name = value
    let_decl,

    /// Type alias: type Name = OtherType
    type_alias,

    /// Trait definition: trait Name<T> { fn method(self) -> T; }
    trait_def,

    /// Implementation block: impl Trait for Type { fn method(self) { ... } }
    impl_block,

    // ============================================
    // Control Flow
    // ============================================

    /// Conditional: if condition { then } else { else }
    if_stmt,

    /// Pattern matching: match expr { patterns }
    match_stmt,

    /// For loop: for item in iterable { body }
    for_stmt,

    /// While loop: while condition { body }
    while_stmt,

    /// Infinite loop: loop { body }
    loop_stmt,

    // ============================================
    // Jump Statements
    // ============================================

    /// Return from function: return value
    return_stmt,

    /// Break out of loop: break
    break_stmt,

    /// Continue to next iteration: continue
    continue_stmt,

    // ============================================
    // Error Handling
    // ============================================

    /// Try expression/block: try expr catch |err| { handler }
    try_stmt,

    /// Throw error: throw error_value
    throw_stmt,

    /// Defer statement: defer expr (executed at scope exit)
    defer_stmt,

    // ============================================
    // Expressions and Blocks
    // ============================================

    /// Assignment statement: target = value
    assignment,

    /// Expression statement (for side effects)
    expression,

    /// Block of statements: { statements }
    block,

    /// Record block: like block but doesn't introduce a new scope (DBL record)
    record_block,

    /// Import statement: import "module"
    import_stmt,

    // ============================================
    // I/O Operations (ISAM support)
    // ============================================

    /// Open channel: open(channel, path, mode)
    io_open,

    /// Close channel: close(channel)
    io_close,

    /// Read from channel: read(channel, record)
    io_read,

    /// Write to channel: write(channel, record)
    io_write,

    /// Store to ISAM: store(channel, record)
    io_store,

    /// Delete from ISAM: delete(channel)
    io_delete,

    // ============================================
    // Compile-Time
    // ============================================

    /// Compile-time conditional: comptime if condition { }
    comptime_if,

    /// Compile-time block: comptime { }
    comptime_block,

    // ============================================
    // Testing
    // ============================================

    /// Test definition: test "name" { body }
    test_def,

    /// Check if this is a declaration statement
    pub fn isDeclaration(self: StatementTag) bool {
        return switch (self) {
            .fn_def, .struct_def, .union_def, .field_view, .enum_def, .const_decl, .let_decl, .type_alias, .test_def => true,
            else => false,
        };
    }

    /// Check if this is a test definition
    pub fn isTest(self: StatementTag) bool {
        return self == .test_def;
    }

    /// Check if this is a control flow statement
    pub fn isControlFlow(self: StatementTag) bool {
        return switch (self) {
            .if_stmt, .match_stmt, .for_stmt, .while_stmt, .loop_stmt => true,
            else => false,
        };
    }

    /// Check if this is a jump statement
    pub fn isJump(self: StatementTag) bool {
        return switch (self) {
            .return_stmt, .break_stmt, .continue_stmt => true,
            else => false,
        };
    }

    /// Check if this is an I/O statement
    pub fn isIO(self: StatementTag) bool {
        return switch (self) {
            .io_open, .io_close, .io_read, .io_write, .io_store, .io_delete => true,
            else => false,
        };
    }

    /// Check if this is a compile-time statement
    pub fn isComptime(self: StatementTag) bool {
        return switch (self) {
            .comptime_if, .comptime_block => true,
            else => false,
        };
    }
};

// ============================================================
// Tests
// ============================================================

test "StatementTag categories" {
    try std.testing.expect(StatementTag.fn_def.isDeclaration());
    try std.testing.expect(!StatementTag.fn_def.isControlFlow());

    try std.testing.expect(StatementTag.if_stmt.isControlFlow());
    try std.testing.expect(!StatementTag.if_stmt.isDeclaration());

    try std.testing.expect(StatementTag.return_stmt.isJump());
    try std.testing.expect(StatementTag.io_read.isIO());
    try std.testing.expect(StatementTag.comptime_if.isComptime());
}
