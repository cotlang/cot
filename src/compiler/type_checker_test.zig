//! Type Checker Tests
//!
//! Comprehensive tests for type checking and type compatibility rules.

const std = @import("std");
const ir = @import("../ir/ir.zig");
const type_rules = @import("type_rules.zig");
const type_checker = @import("type_checker.zig");
const DiagnosticCollector = @import("diagnostic_collector.zig").DiagnosticCollector;

const Type = ir.Type;
const Compatibility = type_rules.Compatibility;

// ============================================================================
// Type Compatibility: Same Types
// ============================================================================

test "type_rules: same type is compatible" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i32, .i32));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .i64));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.bool, .bool));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.string, .string));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.f32, .f32));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.f64, .f64));
}

test "type_rules: void is never assignable" {
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.void, .i32));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.void, .string));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.void, .bool));
}

// ============================================================================
// Type Compatibility: Integer Widening
// ============================================================================

test "type_rules: i32 accepts smaller signed integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i32, .i8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i32, .i16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i32, .i32));
}

test "type_rules: i64 accepts all smaller integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .i8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .i16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .i32));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .i64));
}

test "type_rules: i32 accepts smaller unsigned integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i32, .u8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i32, .u16));
}

test "type_rules: i64 accepts smaller unsigned integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .u8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .u16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i64, .u32));
}

// ============================================================================
// Type Compatibility: Integer Narrowing (Lossy)
// ============================================================================

test "type_rules: i8 from larger signed is incompatible" {
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i8, .i16));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i8, .i32));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i8, .i64));
}

test "type_rules: i16 from i32/i64 is incompatible" {
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i16, .i32));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i16, .i64));
}

test "type_rules: u32 from signed is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.u32, .i8));
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.u32, .i16));
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.u32, .i32));
}

test "type_rules: i32 from u32 is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.i32, .u32));
}

test "type_rules: i64 from u64 is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.i64, .u64));
}

// ============================================================================
// Type Compatibility: Unsigned Integer Widening
// ============================================================================

test "type_rules: u32 accepts smaller unsigned integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u32, .u8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u32, .u16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u32, .u32));
}

test "type_rules: u64 accepts all smaller unsigned integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u64, .u8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u64, .u16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u64, .u32));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.u64, .u64));
}

// ============================================================================
// Type Compatibility: Boolean
// ============================================================================

test "type_rules: bool only accepts bool" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.bool, .bool));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.bool, .i32));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.bool, .string));
}

test "type_rules: integers don't accept bool" {
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i32, .bool));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.i64, .bool));
}

// ============================================================================
// Type Compatibility: String
// ============================================================================

test "type_rules: string accepts string" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.string, .string));
}

test "type_rules: string doesn't accept non-strings" {
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.string, .i32));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.string, .bool));
    try std.testing.expectEqual(Compatibility.incompatible, type_rules.isAssignable(.string, .f64));
}

// ============================================================================
// Type Compatibility: Floating Point
// ============================================================================

test "type_rules: f64 accepts f32" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.f64, .f32));
}

test "type_rules: f32 from f64 is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.f32, .f64));
}

test "type_rules: f64 accepts small integers implicitly" {
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f64, .i8));
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f64, .i16));
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f64, .i32));
}

test "type_rules: f32 accepts small integers implicitly" {
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f32, .i8));
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f32, .i16));
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f32, .u8));
    try std.testing.expectEqual(Compatibility.implicit_conversion, type_rules.isAssignable(.f32, .u16));
}

test "type_rules: f64 from i64 is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.f64, .i64));
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.f64, .u64));
}

test "type_rules: f32 from i32 is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.f32, .i32));
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.f32, .u32));
}

// ============================================================================
// Type Compatibility: Pointer-Sized Integers
// ============================================================================

test "type_rules: isize accepts smaller signed integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.isize, .i8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.isize, .i16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.isize, .i32));
}

test "type_rules: usize accepts smaller unsigned integers" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.usize, .u8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.usize, .u16));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.usize, .u32));
}

test "type_rules: usize from signed is lossy" {
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.usize, .i32));
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.usize, .i64));
}

// ============================================================================
// Type Helper Functions
// ============================================================================

test "type_rules: isNumeric for integers" {
    try std.testing.expect(type_rules.isNumeric(.i8));
    try std.testing.expect(type_rules.isNumeric(.i16));
    try std.testing.expect(type_rules.isNumeric(.i32));
    try std.testing.expect(type_rules.isNumeric(.i64));
    try std.testing.expect(type_rules.isNumeric(.u8));
    try std.testing.expect(type_rules.isNumeric(.u16));
    try std.testing.expect(type_rules.isNumeric(.u32));
    try std.testing.expect(type_rules.isNumeric(.u64));
}

test "type_rules: isNumeric for floats" {
    try std.testing.expect(type_rules.isNumeric(.f32));
    try std.testing.expect(type_rules.isNumeric(.f64));
}

test "type_rules: isNumeric for pointer-sized" {
    try std.testing.expect(type_rules.isNumeric(.isize));
    try std.testing.expect(type_rules.isNumeric(.usize));
}

test "type_rules: isNumeric returns false for non-numerics" {
    try std.testing.expect(!type_rules.isNumeric(.void));
    try std.testing.expect(!type_rules.isNumeric(.bool));
    try std.testing.expect(!type_rules.isNumeric(.string));
}

test "type_rules: isInteger for integers" {
    try std.testing.expect(type_rules.isInteger(.i8));
    try std.testing.expect(type_rules.isInteger(.i16));
    try std.testing.expect(type_rules.isInteger(.i32));
    try std.testing.expect(type_rules.isInteger(.i64));
    try std.testing.expect(type_rules.isInteger(.u8));
    try std.testing.expect(type_rules.isInteger(.u16));
    try std.testing.expect(type_rules.isInteger(.u32));
    try std.testing.expect(type_rules.isInteger(.u64));
}

test "type_rules: isInteger returns false for floats" {
    try std.testing.expect(!type_rules.isInteger(.f32));
    try std.testing.expect(!type_rules.isInteger(.f64));
}

test "type_rules: isFloat for floats" {
    try std.testing.expect(type_rules.isFloat(.f32));
    try std.testing.expect(type_rules.isFloat(.f64));
}

test "type_rules: isFloat returns false for integers" {
    try std.testing.expect(!type_rules.isFloat(.i32));
    try std.testing.expect(!type_rules.isFloat(.i64));
}

test "type_rules: isSigned for signed integers" {
    try std.testing.expect(type_rules.isSigned(.i8));
    try std.testing.expect(type_rules.isSigned(.i16));
    try std.testing.expect(type_rules.isSigned(.i32));
    try std.testing.expect(type_rules.isSigned(.i64));
    try std.testing.expect(type_rules.isSigned(.isize));
}

test "type_rules: isSigned returns false for unsigned" {
    try std.testing.expect(!type_rules.isSigned(.u8));
    try std.testing.expect(!type_rules.isSigned(.u16));
    try std.testing.expect(!type_rules.isSigned(.u32));
    try std.testing.expect(!type_rules.isSigned(.u64));
    try std.testing.expect(!type_rules.isSigned(.usize));
}

// ============================================================================
// Binary Operation Type Checking
// ============================================================================

test "type_rules: arithmetic ops with same types" {
    const result = type_rules.checkBinaryOp(.arithmetic, .i32, .i32);
    try std.testing.expect(result.ok);
    try std.testing.expectEqual(Type.i32, result.result_type);
}

test "type_rules: arithmetic ops with compatible integers" {
    // i32 + i16 should work, result is wider type
    const result = type_rules.checkBinaryOp(.arithmetic, .i32, .i16);
    try std.testing.expect(result.ok);
    try std.testing.expectEqual(Type.i32, result.result_type);
}

test "type_rules: arithmetic ops with floats" {
    const result = type_rules.checkBinaryOp(.arithmetic, .f64, .f64);
    try std.testing.expect(result.ok);
    try std.testing.expectEqual(Type.f64, result.result_type);
}

test "type_rules: arithmetic ops with mixed float/int" {
    // f64 + i32 should work (integer promoted to float)
    const result = type_rules.checkBinaryOp(.arithmetic, .f64, .i32);
    try std.testing.expect(result.ok);
}

test "type_rules: arithmetic ops with incompatible types fails" {
    const result = type_rules.checkBinaryOp(.arithmetic, .i32, .string);
    try std.testing.expect(!result.ok);
}

test "type_rules: arithmetic with bool fails" {
    const result = type_rules.checkBinaryOp(.arithmetic, .i32, .bool);
    try std.testing.expect(!result.ok);
}

test "type_rules: comparison ops with same types" {
    const result = type_rules.checkBinaryOp(.comparison, .i32, .i32);
    try std.testing.expect(result.ok);
    try std.testing.expectEqual(Type.bool, result.result_type);
}

test "type_rules: comparison ops with compatible integers" {
    const result = type_rules.checkBinaryOp(.comparison, .i64, .i16);
    try std.testing.expect(result.ok);
    try std.testing.expectEqual(Type.bool, result.result_type);
}

test "type_rules: comparison ops with floats" {
    const result = type_rules.checkBinaryOp(.comparison, .f64, .f64);
    try std.testing.expect(result.ok);
}

test "type_rules: string concat requires strings" {
    const result = type_rules.checkBinaryOp(.string_concat, .string, .string);
    try std.testing.expect(result.ok);
}

test "type_rules: string concat with non-string fails" {
    const result = type_rules.checkBinaryOp(.string_concat, .string, .i32);
    try std.testing.expect(!result.ok);
}

// ============================================================================
// Type Formatter
// ============================================================================

test "type_rules: formatType for basic types" {
    var buf: [64]u8 = undefined;

    try std.testing.expectEqualStrings("i32", type_rules.formatType(.i32, &buf));
    try std.testing.expectEqualStrings("i64", type_rules.formatType(.i64, &buf));
    try std.testing.expectEqualStrings("f64", type_rules.formatType(.f64, &buf));
    try std.testing.expectEqualStrings("bool", type_rules.formatType(.bool, &buf));
    try std.testing.expectEqualStrings("string", type_rules.formatType(.string, &buf));
    try std.testing.expectEqualStrings("void", type_rules.formatType(.void, &buf));
}

// ============================================================================
// Type Checker Integration
// ============================================================================

test "type_checker: empty module has no errors" {
    const allocator = std.testing.allocator;
    var collector = DiagnosticCollector.init(allocator);
    defer collector.deinit();

    var module = ir.Module.init(allocator);
    defer module.deinit();

    var checker = type_checker.init(allocator, &collector, &module, "test.cot");
    checker.check();

    try std.testing.expect(!checker.hasErrors());
    try std.testing.expectEqual(@as(usize, 0), collector.errors.items.len);
}

// ============================================================================
// Edge Cases
// ============================================================================

test "type_rules: signed/unsigned same size is lossy" {
    // u8 to i8 may overflow on values > 127
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.i8, .u8));
    try std.testing.expectEqual(Compatibility.lossy_conversion, type_rules.isAssignable(.u8, .i8));
}

test "type_rules: i16 accepts i8 and u8" {
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i16, .i8));
    try std.testing.expectEqual(Compatibility.compatible, type_rules.isAssignable(.i16, .u8));
}

test "type_rules: comparison with bools" {
    // Boolean comparison should work
    const result = type_rules.checkBinaryOp(.comparison, .bool, .bool);
    try std.testing.expect(result.ok);
}
