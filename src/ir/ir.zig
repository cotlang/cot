//! Cot IR - Intermediate Representation
//!
//! This module defines the IR used between the AST and code generation backends.
//! The IR is designed to:
//! - Support multiple backends (bytecode VM, native via Cranelift, Zig transpiler)
//! - Enable Cot functions to be exported with C ABI for .NET interop
//! - Preserve type information for accurate code generation
//! - Be inspectable for debugging and optimization
//!
//! Instruction names are aligned with Cranelift IR for easy JIT translation:
//! - Arithmetic: iadd, isub, imul, sdiv, udiv, srem, urem, ineg
//! - Bitwise: band, bor, bxor, bnot, ishl, sshr, ushr
//! - Comparison: icmp with IntCC condition codes
//! - Control: jump, brif, br_table, return_
//! - Constants: iconst, f32const, f64const

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// Types
// ============================================================================

/// Cot type representation in IR
pub const Type = union(enum) {
    /// Void type (for functions with no return)
    void: void,

    /// Boolean type
    bool: void,

    /// Signed integer types
    i8: void,
    i16: void,
    i32: void,
    i64: void,

    /// Unsigned integer types
    u8: void,
    u16: void,
    u32: void,
    u64: void,

    /// Pointer-sized integers
    isize: void,
    usize: void,

    /// Floating point types
    f32: void,
    f64: void,

    /// String type (dynamic length)
    string: void,

    /// Decimal type (for precise financial calculations)
    decimal: DecimalType,

    /// Pointer to another type
    ptr: *const Type,

    /// Optional type (nullable)
    optional: *const Type,

    /// Array of a type
    array: struct {
        element: *const Type,
        length: u32,
    },

    /// Slice (dynamic array)
    slice: *const Type,

    /// Struct type
    @"struct": *const StructType,

    /// Union type (fields share memory - overlays)
    @"union": *const UnionType,

    /// Function type
    function: *const FunctionType,

    /// Map type (ordered key-value store)
    map: *const MapType,

    pub const DecimalType = struct {
        /// Total precision in digits
        precision: u32,
        /// Number of decimal places (scale)
        scale: u8,
    };

    /// Get the size of this type in bytes
    pub fn sizeInBytes(self: Type) u32 {
        return switch (self) {
            .void => 0,
            .bool => 1,
            .i8, .u8 => 1,
            .i16, .u16 => 2,
            .i32, .u32, .f32 => 4,
            .i64, .u64, .f64 => 8,
            .isize, .usize => @sizeOf(usize),
            .string => 16, // Pointer + length
            .decimal => |d| @max(8, (d.precision + 1) / 2), // At least 8 bytes
            .ptr, .optional => 8, // 64-bit pointers
            .array => |a| a.element.sizeInBytes() * a.length,
            .slice => 16, // Pointer + length
            .@"struct" => |s| s.size,
            .@"union" => |u| u.size,
            .function => 8, // Function pointer
            .map => 8, // Map pointer
        };
    }

    /// Get alignment requirement for this type
    pub fn alignment(self: Type) u32 {
        return switch (self) {
            .void => 1,
            .bool => 1,
            .i8, .u8 => 1,
            .i16, .u16 => 2,
            .i32, .u32, .f32 => 4,
            .i64, .u64, .f64 => 8,
            .isize, .usize => @alignOf(usize),
            .string, .slice => 8,
            .decimal => 8,
            .ptr, .optional => 8,
            .array => |a| a.element.alignment(),
            .@"struct" => |s| s.alignment,
            .@"union" => |u| u.alignment,
            .function => 8,
            .map => 8,
        };
    }

    /// Get the C ABI type name for this type
    pub fn cTypeName(self: Type) []const u8 {
        return switch (self) {
            .void => "void",
            .bool => "bool",
            .i8 => "int8_t",
            .i16 => "int16_t",
            .i32 => "int32_t",
            .i64 => "int64_t",
            .u8 => "uint8_t",
            .u16 => "uint16_t",
            .u32 => "uint32_t",
            .u64 => "uint64_t",
            .isize => "intptr_t",
            .usize => "uintptr_t",
            .f32 => "float",
            .f64 => "double",
            .string => "cot_string_t*",
            .decimal => "cot_decimal_t",
            .ptr, .optional => "void*",
            .array, .slice => "void*",
            .@"struct" => "void*",
            .@"union" => "void*",
            .function => "void*",
            .map => "void*",
        };
    }

    /// Check if this type is numeric
    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .isize => true,
            .u8, .u16, .u32, .u64, .usize => true,
            .f32, .f64, .decimal => true,
            else => false,
        };
    }

    /// Check if this type is an integer
    pub fn isInteger(self: Type) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .isize => true,
            .u8, .u16, .u32, .u64, .usize => true,
            else => false,
        };
    }

    /// Check if this type is signed
    pub fn isSigned(self: Type) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .isize, .f32, .f64, .decimal => true,
            else => false,
        };
    }
};

/// Struct (structure) type definition
pub const StructType = struct {
    name: []const u8,
    fields: []const Field,
    size: u32,
    alignment: u32,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        offset: u32,
    };
};

/// Union type definition (fields share memory - all at offset 0)
pub const UnionType = struct {
    name: []const u8,
    variants: []const Variant,
    size: u32, // max of all variant sizes
    alignment: u32, // max of all variant alignments

    pub const Variant = struct {
        name: []const u8,
        ty: Type,
    };
};

/// Function type definition
pub const FunctionType = struct {
    params: []const Param,
    return_type: Type,
    is_variadic: bool,

    pub const Param = struct {
        name: []const u8,
        ty: Type,
        is_ref: bool, // by value (false) or by reference (true)
        default_value: ?Value = null, // optional default value for optional params
    };
};

/// Map type definition (ordered key-value store)
pub const MapType = struct {
    key_type: *const Type,
    value_type: *const Type,
};

// ============================================================================
// Values and Instructions
// ============================================================================

/// An SSA value reference
pub const Value = struct {
    id: u32,
    ty: Type,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("%{d}", .{self.id});
    }

    // ========== Type Validation Methods ==========

    /// Check if this value has a numeric type (suitable for arithmetic)
    pub fn isNumeric(self: Value) bool {
        return switch (self.ty) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .decimal => true,
            else => false,
        };
    }

    /// Check if this value has a string type
    pub fn isString(self: Value) bool {
        return switch (self.ty) {
            .string => true,
            // Check if it's a [N]u8 array (fixed-length string)
            .array => |a| a.element.* == .u8,
            else => false,
        };
    }

    /// Check if this value has a boolean type
    pub fn isBool(self: Value) bool {
        return self.ty == .bool;
    }

    /// Check if this value has a pointer type
    pub fn isPointer(self: Value) bool {
        return self.ty == .ptr;
    }

    /// Check if this value has an optional type
    pub fn isOptional(self: Value) bool {
        return self.ty == .optional;
    }

    /// Check if this value has an integer type (signed or unsigned)
    pub fn isInteger(self: Value) bool {
        return switch (self.ty) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => true,
            else => false,
        };
    }

    /// Check if this value has a floating point type
    pub fn isFloat(self: Value) bool {
        return switch (self.ty) {
            .f32, .f64 => true,
            else => false,
        };
    }

    /// Get the pointee type if this is a pointer, otherwise return the type itself
    pub fn pointeeType(self: Value) Type {
        return if (self.ty == .ptr) self.ty.ptr.* else self.ty;
    }

    /// Check if two values have compatible types for arithmetic
    pub fn compatibleForArithmetic(self: Value, other: Value) bool {
        // Both numeric, or both strings (for concatenation)
        return (self.isNumeric() and other.isNumeric()) or
            (self.isString() and other.isString());
    }

    /// Check if two values have compatible types for comparison
    pub fn compatibleForComparison(self: Value, other: Value) bool {
        // Same category: numeric vs numeric, string vs string, bool vs bool
        return (self.isNumeric() and other.isNumeric()) or
            (self.isString() and other.isString()) or
            (self.isBool() and other.isBool()) or
            (self.isPointer() and other.isPointer()) or
            (self.isOptional() or other.isOptional());
    }
};

/// Source location for error reporting
pub const SourceLoc = struct {
    line: u32,
    column: u32,
};

/// Integer comparison condition codes (matches Cranelift IntCC)
pub const IntCC = enum {
    eq, // Equal
    ne, // Not equal
    slt, // Signed less than
    sge, // Signed greater or equal
    sgt, // Signed greater than
    sle, // Signed less or equal
    ult, // Unsigned less than
    uge, // Unsigned greater or equal
    ugt, // Unsigned greater than
    ule, // Unsigned less or equal
};

/// IR Instructions (names aligned with Cranelift IR)
pub const Instruction = union(enum) {
    // ====== Memory operations ======
    alloca: Alloca,
    load: Load,
    store: Store,
    field_ptr: FieldPtr,

    // ====== Arithmetic (Cranelift names) ======
    iadd: BinaryOp, // Integer add
    isub: BinaryOp, // Integer subtract
    imul: BinaryOp, // Integer multiply
    sdiv: BinaryOp, // Signed divide
    udiv: BinaryOp, // Unsigned divide
    srem: BinaryOp, // Signed remainder
    urem: BinaryOp, // Unsigned remainder
    ineg: UnaryOp, // Integer negate

    // ====== Rounding operations (DBL legacy: # and ##) ======
    round: RoundOp, // True rounding: value ## places
    trunc: RoundOp, // Truncating round: value # places

    // ====== Bitwise operations (Cranelift names) ======
    band: BinaryOp, // Bitwise AND
    bor: BinaryOp, // Bitwise OR
    bxor: BinaryOp, // Bitwise XOR
    bnot: UnaryOp, // Bitwise NOT
    ishl: BinaryOp, // Integer shift left
    sshr: BinaryOp, // Signed shift right (arithmetic)
    ushr: BinaryOp, // Unsigned shift right (logical)

    // ====== Comparison (Cranelift style) ======
    icmp: IcmpOp, // Integer compare with condition code

    // ====== Logical (short-circuit) ======
    log_and: BinaryOp,
    log_or: BinaryOp,
    log_not: UnaryOp,

    // ====== String operations (Cot-specific) ======
    str_concat: BinaryOp,
    str_compare: BinaryOp,
    str_copy: struct { dest: Value, src: Value },
    str_slice: StrSlice,
    str_slice_store: StrSliceStore,
    str_len: UnaryOp,

    // ====== Control flow (Cranelift names) ======
    jump: Branch, // Unconditional jump (was: br)
    brif: CondBranch, // Branch if (was: cond_br)
    br_table: Switch, // Branch table (was: switch_br)
    return_: ?Value, // Return (was: ret)

    // ====== Function calls ======
    call: Call,
    call_indirect: CallIndirect, // Indirect call through function pointer

    // ====== Exception handling (Cot-specific) ======
    trap: TrapCode, // Trap/abort (Cranelift name)
    try_begin: TryBegin,
    try_end: void,
    catch_begin: CatchBegin,
    throw: Throw,

    // ====== Type conversions (Cranelift names) ======
    bitcast: Cast, // Reinterpret bits (was: cast)
    fcvt_from_sint: UnaryOp, // Convert signed int to float (was: int_to_float)
    fcvt_from_uint: UnaryOp, // Convert unsigned int to float
    fcvt_to_sint: UnaryOp, // Convert float to signed int (was: float_to_int)
    fcvt_to_uint: UnaryOp, // Convert float to unsigned int
    sextend: IntExtend, // Sign extend (was: int_extend with signed=true)
    uextend: IntExtend, // Zero extend (was: int_extend with signed=false)
    ireduce: UnaryOp, // Reduce integer size (was: int_truncate)
    format_decimal: FormatDecimal, // Format integer as zero-padded decimal string (DBL compatibility)
    parse_decimal: ParseDecimal, // Parse string to decimal with validation (DBL compatibility)

    // ====== Constants (Cranelift names) ======
    iconst: struct { ty: Type, value: i64, result: Value }, // Integer constant
    f32const: struct { value: f32, result: Value }, // 32-bit float constant
    f64const: struct { value: f64, result: Value }, // 64-bit float constant
    const_string: struct { value: []const u8, result: Value }, // Cot-specific
    const_null: struct { ty: Type, result: Value }, // Null constant

    // ====== Optional operations (Cot-specific) ======
    wrap_optional: UnaryOp,
    unwrap_optional: UnaryOp,
    is_null: UnaryOp,

    // ====== I/O operations (Cot-specific, ISAM file access) ======
    io_open: IoOpen,
    io_close: struct { channel: Value },
    io_read: IoRead,
    io_write: IoWrite,
    io_delete: struct { channel: Value },
    io_unlock: struct { channel: Value },

    // ====== Struct buffer operations (Cot-specific) ======
    load_struct_buf: struct { base_name: []const u8, struct_name: []const u8, result: Value },
    store_struct_buf: struct { base_name: []const u8, struct_name: []const u8, value: Value },

    // ====== Array operations ======
    array_load: ArrayOp,
    array_store: ArrayStore,
    array_len: UnaryOp,

    // ====== Map operations ======
    map_new: MapNew,
    map_set: MapSet,
    map_get: MapGet,
    map_delete: MapDelete,
    map_has: MapHas,
    map_len: MapLen,
    map_clear: MapClear,
    map_keys: MapKeys,
    map_values: MapValues,

    // ====== Debug information ======
    debug_line: struct { line: u32, column: u32 },

    // Allocate instruction
    pub const Alloca = struct {
        ty: Type,
        name: []const u8,
        result: Value,
    };

    // Load from memory
    pub const Load = struct {
        ptr: Value,
        result: Value,
    };

    // Store to memory
    pub const Store = struct {
        ptr: Value,
        value: Value,
        loc: ?SourceLoc = null,
    };

    // Get pointer to struct field
    pub const FieldPtr = struct {
        struct_ptr: Value,
        field_index: u32,
        result: Value,
    };

    // Binary operation
    pub const BinaryOp = struct {
        lhs: Value,
        rhs: Value,
        result: Value,
        loc: ?SourceLoc = null,
    };

    // Rounding operation (DBL legacy: # and ##)
    pub const RoundOp = struct {
        value: Value, // Value to round
        places: Value, // Number of decimal places
        result: Value, // Rounded result
        loc: ?SourceLoc = null,
    };

    // Unary operation
    pub const UnaryOp = struct {
        operand: Value,
        result: Value,
    };

    // Unconditional branch
    pub const Branch = struct {
        target: *Block,
    };

    // Conditional branch
    pub const CondBranch = struct {
        condition: Value,
        then_block: *Block,
        else_block: *Block,
    };

    // Switch/match branch
    pub const Switch = struct {
        value: Value,
        cases: []const Case,
        default: *Block,

        pub const Case = struct {
            value: i64,
            target: *Block,
        };
    };

    // Function call
    pub const Call = struct {
        callee: []const u8,
        args: []const Value,
        result: ?Value,
        method_receiver: ?Value = null, // For method calls: obj.method()
        loc: ?SourceLoc = null,
    };

    // Try block start
    pub const TryBegin = struct {
        catch_block: *Block,
    };

    // Catch block start
    pub const CatchBegin = struct {
        error_type: ?[]const u8, // null for catch-all
        error_value: ?Value, // Local to bind error to
    };

    // Throw exception
    pub const Throw = struct {
        value: Value,
        loc: ?SourceLoc = null,
    };

    // Type cast
    pub const Cast = struct {
        operand: Value,
        target_type: Type,
        result: Value,
    };

    // Format integer as zero-padded decimal string (DBL compatibility)
    pub const FormatDecimal = struct {
        value: Value, // Integer value to format
        width: u32, // Target width (zero-pad to this many digits)
        result: Value, // Result string
    };

    // Parse string to decimal with validation (DBL compatibility)
    // Raises "bad digit" error if string contains non-numeric characters
    pub const ParseDecimal = struct {
        value: Value, // String value to parse
        result: Value, // Result integer
    };

    // Integer sign/zero extend
    pub const IntExtend = struct {
        operand: Value,
        target_type: Type,
        signed: bool,
        result: Value,
    };

    // Integer comparison (Cranelift style with condition code)
    pub const IcmpOp = struct {
        cond: IntCC,
        lhs: Value,
        rhs: Value,
        result: Value,
        loc: ?SourceLoc = null,
    };

    // Indirect call through function pointer (Cranelift style)
    pub const CallIndirect = struct {
        sig: *const FunctionType,
        callee: Value, // Function pointer value
        args: []const Value,
        result: ?Value,
        loc: ?SourceLoc = null,
    };

    // Trap codes for runtime errors (Cranelift style)
    pub const TrapCode = enum {
        stack_overflow,
        heap_out_of_bounds,
        integer_overflow,
        integer_division_by_zero,
        bad_conversion_to_integer,
        unreachable_code,
    };

    /// Array load - load element from array (0-indexed)
    pub const ArrayOp = struct {
        array_ptr: Value, // Pointer to array base
        index: Value, // Index expression (0-based)
        result: Value, // Result value
        loc: ?SourceLoc = null,
    };

    /// Array store - store element to array (0-indexed)
    pub const ArrayStore = struct {
        array_ptr: Value, // Pointer to array base
        index: Value, // Index expression (0-based)
        value: Value, // Value to store
        loc: ?SourceLoc = null,
    };

    /// Map new - create a new map
    pub const MapNew = struct {
        flags: u8, // bit 0 = case_sensitive, bit 1 = preserve_spaces
        result: Value, // Result map value
        loc: ?SourceLoc = null,
    };

    /// Map set - set key-value pair
    pub const MapSet = struct {
        map: Value, // Map to modify
        key: Value, // Key value
        value: Value, // Value to store
        loc: ?SourceLoc = null,
    };

    /// Map get - get value by key
    pub const MapGet = struct {
        map: Value, // Map to read
        key: Value, // Key to look up
        result: Value, // Result value
        loc: ?SourceLoc = null,
    };

    /// Map delete - delete by key
    pub const MapDelete = struct {
        map: Value, // Map to modify
        key: Value, // Key to delete
        loc: ?SourceLoc = null,
    };

    /// Map has - check if key exists
    pub const MapHas = struct {
        map: Value, // Map to check
        key: Value, // Key to look up
        result: Value, // Boolean result
        loc: ?SourceLoc = null,
    };

    /// Map len - get number of entries
    pub const MapLen = struct {
        map: Value, // Map to query
        result: Value, // Integer result
        loc: ?SourceLoc = null,
    };

    /// Map clear - remove all entries
    pub const MapClear = struct {
        map: Value, // Map to clear
        loc: ?SourceLoc = null,
    };

    /// Map keys - get array of keys
    pub const MapKeys = struct {
        map: Value, // Map to query
        result: Value, // Array of keys
        loc: ?SourceLoc = null,
    };

    /// Map values - get array of values
    pub const MapValues = struct {
        map: Value, // Map to query
        result: Value, // Array of values
        loc: ?SourceLoc = null,
    };

    /// String slice - extract portion of string (0-indexed)
    pub const StrSlice = struct {
        source: Value, // Source string
        start: Value, // Start index (0-based)
        length_or_end: Value, // Length or end index depending on is_length
        is_length: bool = false, // If true, length_or_end is length; if false, end index (exclusive)
        result: Value, // Result value
        loc: ?SourceLoc = null,
    };

    /// String slice store - modify portion of string (0-indexed)
    pub const StrSliceStore = struct {
        target: Value, // Target string value
        target_ptr: Value, // Target variable (for store after modification)
        start: Value, // Start index (0-based)
        length_or_end: Value, // Length or end index depending on is_length
        is_length: bool = false, // If true, length_or_end is length; if false, end index (exclusive)
        value: Value, // Value to store
        loc: ?SourceLoc = null,
    };

    // I/O operations for ISAM file access
    pub const IoOpen = struct {
        channel: Value,
        mode: OpenMode,
        filename: Value,

        pub const OpenMode = enum {
            input, // Read-only
            output, // Write-only, truncate
            update, // Read-write
            append, // Write-only, append
            indexed, // ISAM indexed access
        };
    };

    pub const IoRead = struct {
        channel: Value,
        buffer: Value,
        key: ?Value,
        qualifiers: IoQualifiers,
        struct_name: ?[]const u8 = null, // For automatic struct unpacking
        base_name: ?[]const u8 = null, // Variable name for struct unpacking (e.g., "cust")
    };

    pub const IoWrite = struct {
        channel: Value,
        buffer: Value,
        is_insert: bool = false, // true for insert, false for update
    };

    pub const IoQualifiers = struct {
        match_mode: MatchMode = .exact,
        lock_mode: LockMode = .no_lock,
        key_index: u8 = 0,

        pub const MatchMode = enum { exact, greater_equal, greater, partial };
        pub const LockMode = enum { no_lock, shared, exclusive };
    };

    // ========== Instruction Category System ==========

    /// Categories of IR instructions for analysis and optimization
    pub const Category = enum {
        memory, // alloca, load, store, field_ptr, load_struct_buf, store_struct_buf
        arithmetic, // iadd, isub, imul, sdiv, udiv, srem, urem, ineg
        bitwise, // band, bor, bxor, bnot, ishl, sshr, ushr
        comparison, // icmp with IntCC condition codes
        logical, // log_and, log_or, log_not
        string, // str_concat, str_compare, str_copy, str_slice, str_slice_store, str_len
        control, // jump, brif, br_table, return_, call, call_indirect, trap
        exception, // try_begin, try_end, catch_begin, throw
        conversion, // bitcast, fcvt_from_sint, fcvt_from_uint, fcvt_to_sint, fcvt_to_uint, sextend, uextend, ireduce
        constant, // iconst, f32const, f64const, const_string, const_null
        optional, // wrap_optional, unwrap_optional, is_null
        io, // io_open, io_close, io_read, io_write, io_delete, io_unlock
        array, // array_load, array_store, array_len
        map, // map_new, map_set, map_get, map_delete, map_has, map_len, map_clear, map_keys, map_values
        debug, // debug_line
    };

    /// Get the category of this instruction
    pub fn category(self: Instruction) Category {
        return switch (self) {
            .alloca, .load, .store, .field_ptr, .load_struct_buf, .store_struct_buf => .memory,
            .iadd, .isub, .imul, .sdiv, .udiv, .srem, .urem, .ineg, .round, .trunc => .arithmetic,
            .band, .bor, .bxor, .bnot, .ishl, .sshr, .ushr => .bitwise,
            .icmp => .comparison,
            .log_and, .log_or, .log_not => .logical,
            .str_concat, .str_compare, .str_copy, .str_slice, .str_slice_store, .str_len => .string,
            .jump, .brif, .br_table, .return_, .call, .call_indirect, .trap => .control,
            .try_begin, .try_end, .catch_begin, .throw => .exception,
            .bitcast, .fcvt_from_sint, .fcvt_from_uint, .fcvt_to_sint, .fcvt_to_uint, .sextend, .uextend, .ireduce => .conversion,
            .iconst, .f32const, .f64const, .const_string, .const_null => .constant,
            .wrap_optional, .unwrap_optional, .is_null => .optional,
            .io_open, .io_close, .io_read, .io_write, .io_delete, .io_unlock => .io,
            .array_load, .array_store, .array_len => .array,
            .map_new, .map_set, .map_get, .map_delete, .map_has, .map_len, .map_clear, .map_keys, .map_values => .map,
            .debug_line => .debug,
        };
    }

    /// Check if this instruction is a memory operation
    pub fn isMemory(self: Instruction) bool {
        return self.category() == .memory;
    }

    /// Check if this instruction is an arithmetic operation
    pub fn isArithmetic(self: Instruction) bool {
        return self.category() == .arithmetic;
    }

    /// Check if this instruction is a comparison operation
    pub fn isComparison(self: Instruction) bool {
        return self.category() == .comparison;
    }

    /// Check if this instruction is a control flow operation
    pub fn isControlFlow(self: Instruction) bool {
        return self.category() == .control;
    }

    /// Check if this instruction is an I/O operation
    pub fn isIo(self: Instruction) bool {
        return self.category() == .io;
    }

    /// Check if this instruction is a terminator (ends a basic block)
    pub fn isTerminator(self: Instruction) bool {
        return switch (self) {
            .jump, .brif, .br_table, .return_, .throw, .trap => true,
            else => false,
        };
    }

    /// Check if this instruction produces a value (has a result)
    pub fn producesValue(self: Instruction) bool {
        return self.getResult() != null;
    }

    /// Get the result value if this instruction produces one
    pub fn getResult(self: Instruction) ?Value {
        return switch (self) {
            .alloca => |a| a.result,
            .load => |l| l.result,
            .field_ptr => |f| f.result,
            .iadd, .isub, .imul, .sdiv, .udiv, .srem, .urem => |op| op.result,
            .round, .trunc => |op| op.result,
            .band, .bor, .bxor, .ishl, .sshr, .ushr => |op| op.result,
            .icmp => |op| op.result,
            .log_and, .log_or => |op| op.result,
            .ineg, .log_not, .bnot => |op| op.result,
            .str_concat, .str_compare => |op| op.result,
            .str_len, .array_len => |op| op.result,
            .str_slice => |s| s.result,
            .iconst => |c| c.result,
            .f32const => |c| c.result,
            .f64const => |c| c.result,
            .const_string => |c| c.result,
            .const_null => |c| c.result,
            .call => |c| c.result,
            .call_indirect => |c| c.result,
            .load_struct_buf => |l| l.result,
            .array_load => |a| a.result,
            .bitcast => |c| c.result,
            .fcvt_from_sint, .fcvt_from_uint, .fcvt_to_sint, .fcvt_to_uint, .ireduce => |op| op.result,
            .sextend, .uextend => |e| e.result,
            .wrap_optional, .unwrap_optional, .is_null => |op| op.result,
            .format_decimal => |f| f.result,
            .parse_decimal => |p| p.result,
            // Map operations with results
            .map_new => |m| m.result,
            .map_get => |m| m.result,
            .map_has => |m| m.result,
            .map_len => |m| m.result,
            .map_keys => |m| m.result,
            .map_values => |m| m.result,
            // Instructions that don't produce values
            .store, .jump, .brif, .br_table, .return_, .trap, .io_open, .io_close, .io_read,
            .io_write, .io_delete, .io_unlock, .store_struct_buf, .array_store,
            .debug_line, .try_begin, .try_end, .catch_begin, .throw, .str_slice_store, .str_copy,
            .map_set, .map_delete, .map_clear => null,
        };
    }

    /// Check if this instruction has side effects (I/O, memory stores, exceptions)
    pub fn hasSideEffects(self: Instruction) bool {
        return switch (self) {
            .store, .io_open, .io_close, .io_read, .io_write, .io_delete, .io_unlock,
            .store_struct_buf, .array_store, .throw, .trap, .call, .call_indirect, .str_copy, .str_slice_store,
            .map_set, .map_delete, .map_clear => true,
            else => false,
        };
    }

    /// Check if this instruction is a map operation
    pub fn isMap(self: Instruction) bool {
        return self.category() == .map;
    }
};

// ============================================================================
// Blocks and Functions
// ============================================================================

/// A basic block in the control flow graph
pub const Block = struct {
    /// Block label (for debugging/printing)
    label: []const u8,

    /// Instructions in this block
    instructions: std.ArrayListUnmanaged(Instruction),

    /// Predecessor blocks
    predecessors: std.ArrayListUnmanaged(*Block),

    /// Successor blocks (determined by terminator)
    successors: std.ArrayListUnmanaged(*Block),

    /// Owning function
    parent: *Function,

    /// Allocator for this block
    allocator: Allocator,

    pub fn init(allocator: Allocator, label: []const u8, parent: *Function) Block {
        return .{
            .label = label,
            .instructions = .{},
            .predecessors = .{},
            .successors = .{},
            .parent = parent,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Block) void {
        // Free heap-allocated slices inside instructions
        for (self.instructions.items) |inst| {
            switch (inst) {
                .call => |c| {
                    if (c.args.len > 0) {
                        self.allocator.free(c.args);
                    }
                },
                .call_indirect => |c| {
                    if (c.args.len > 0) {
                        self.allocator.free(c.args);
                    }
                },
                .br_table => |s| {
                    if (s.cases.len > 0) {
                        self.allocator.free(s.cases);
                    }
                },
                else => {},
            }
        }
        self.instructions.deinit(self.allocator);
        self.predecessors.deinit(self.allocator);
        self.successors.deinit(self.allocator);
    }

    pub fn append(self: *Block, inst: Instruction) !void {
        try self.instructions.append(self.allocator, inst);
    }

    pub fn isTerminated(self: *const Block) bool {
        if (self.instructions.items.len == 0) return false;
        const last = self.instructions.items[self.instructions.items.len - 1];
        return last.isTerminator();
    }
};

/// Linkage type for functions
pub const Linkage = enum {
    /// Internal function, not visible outside module
    internal,

    /// Exported with C ABI for external callers (.NET, etc.)
    export_c,

    /// Imported from external library
    external,
};

/// A function in the IR
pub const Function = struct {
    /// Function name
    name: []const u8,

    /// Export name (may differ from internal name for C ABI)
    export_name: ?[]const u8,

    /// Function signature
    signature: FunctionType,

    /// Linkage/visibility
    linkage: Linkage,

    /// Whether this function is a test
    is_test: bool = false,

    /// Human-readable test name (if is_test is true)
    test_name: ?[]const u8 = null,

    /// Entry block
    entry: *Block,

    /// All blocks in the function
    blocks: std.ArrayListUnmanaged(*Block),

    /// Local variables (allocas)
    locals: std.ArrayListUnmanaged(Local),

    /// SSA value counter
    next_value_id: u32,

    /// Allocator for blocks and instructions
    allocator: Allocator,

    pub const Local = struct {
        name: []const u8,
        ty: Type,
        value: Value, // The alloca result
    };

    pub fn init(allocator: Allocator, name: []const u8, signature: FunctionType) !*Function {
        const self = try allocator.create(Function);
        self.* = .{
            .name = name,
            .export_name = null,
            .signature = signature,
            .linkage = .internal,
            .entry = undefined,
            .blocks = .{},
            .locals = .{},
            .next_value_id = 0,
            .allocator = allocator,
        };

        // Create entry block
        const entry = try allocator.create(Block);
        entry.* = Block.init(allocator, "entry", self);
        self.entry = entry;
        try self.blocks.append(allocator, entry);

        return self;
    }

    pub fn deinit(self: *Function) void {
        for (self.blocks.items) |block| {
            block.deinit();
            self.allocator.destroy(block);
        }
        self.blocks.deinit(self.allocator);
        self.locals.deinit(self.allocator);
        // Free the params slice from the function signature
        if (self.signature.params.len > 0) {
            self.allocator.free(self.signature.params);
        }
        self.allocator.destroy(self);
    }

    /// Create a new SSA value
    pub fn newValue(self: *Function, ty: Type) Value {
        const id = self.next_value_id;
        self.next_value_id += 1;
        return .{ .id = id, .ty = ty };
    }

    /// Create a new basic block
    pub fn createBlock(self: *Function, label: []const u8) !*Block {
        const block = try self.allocator.create(Block);
        block.* = Block.init(self.allocator, label, self);
        try self.blocks.append(self.allocator, block);
        return block;
    }

    /// Mark this function for export with C ABI
    pub fn setExport(self: *Function, export_name: []const u8) void {
        self.linkage = .export_c;
        self.export_name = export_name;
    }
};

// ============================================================================
// Module
// ============================================================================

/// An IR module (compilation unit)
pub const Module = struct {
    /// Module name
    name: []const u8,

    /// Library name for exports (if any)
    library_name: ?[]const u8,

    /// Struct type definitions
    structs: std.ArrayListUnmanaged(*StructType),

    /// Union type definitions
    unions: std.ArrayListUnmanaged(*UnionType),

    /// Global variables
    globals: std.ArrayListUnmanaged(Global),

    /// Functions
    functions: std.ArrayListUnmanaged(*Function),

    /// Allocator
    allocator: Allocator,

    /// Type pointers allocated during lowering (owned by Module)
    allocated_types: std.ArrayListUnmanaged(*Type) = .{},

    pub const Global = struct {
        name: []const u8,
        ty: Type,
        initializer: ?[]const u8, // For initialized data
        is_const: bool,
    };

    pub fn init(allocator: Allocator, name: []const u8) Module {
        return .{
            .name = name,
            .library_name = null,
            .structs = .{},
            .unions = .{},
            .globals = .{},
            .functions = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Module) void {
        for (self.functions.items) |func| {
            func.deinit();
        }
        self.functions.deinit(self.allocator);

        // Free struct types and their fields
        for (self.structs.items) |struct_type| {
            self.allocator.free(struct_type.fields);
            self.allocator.destroy(struct_type);
        }
        self.structs.deinit(self.allocator);

        // Free union types and their variants
        for (self.unions.items) |union_type| {
            self.allocator.free(union_type.variants);
            self.allocator.destroy(union_type);
        }
        self.unions.deinit(self.allocator);

        self.globals.deinit(self.allocator);

        // Free type pointers allocated during lowering
        for (self.allocated_types.items) |ty_ptr| {
            self.allocator.destroy(ty_ptr);
        }
        self.allocated_types.deinit(self.allocator);
    }

    /// Get all exported functions
    pub fn getExports(self: *Module, allocator: Allocator) ![]*Function {
        var exports = std.ArrayListUnmanaged(*Function){};
        for (self.functions.items) |func| {
            if (func.linkage == .export_c) {
                try exports.append(allocator, func);
            }
        }
        return exports.toOwnedSlice(allocator);
    }

    /// Add a function to the module
    pub fn addFunction(self: *Module, func: *Function) !void {
        try self.functions.append(self.allocator, func);
    }

    /// Add a global variable
    pub fn addGlobal(self: *Module, name: []const u8, ty: Type, is_const: bool) !void {
        try self.globals.append(self.allocator, .{
            .name = name,
            .ty = ty,
            .initializer = null,
            .is_const = is_const,
        });
    }

    /// Add a struct type definition
    pub fn addStruct(self: *Module, struct_type: *StructType) !void {
        try self.structs.append(self.allocator, struct_type);
    }

    /// Add a union type definition
    pub fn addUnion(self: *Module, union_type: *UnionType) !void {
        try self.unions.append(self.allocator, union_type);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ir type sizes" {
    try std.testing.expectEqual(@as(u32, 1), (Type{ .i8 = {} }).sizeInBytes());
    try std.testing.expectEqual(@as(u32, 2), (Type{ .i16 = {} }).sizeInBytes());
    try std.testing.expectEqual(@as(u32, 4), (Type{ .i32 = {} }).sizeInBytes());
    try std.testing.expectEqual(@as(u32, 8), (Type{ .i64 = {} }).sizeInBytes());
    try std.testing.expectEqual(@as(u32, 4), (Type{ .f32 = {} }).sizeInBytes());
    try std.testing.expectEqual(@as(u32, 8), (Type{ .f64 = {} }).sizeInBytes());
    try std.testing.expectEqual(@as(u32, 1), (Type{ .bool = {} }).sizeInBytes());

    const decimal_type = Type{ .decimal = .{ .precision = 18, .scale = 2 } };
    try std.testing.expectEqual(@as(u32, 9), decimal_type.sizeInBytes());
}

test "ir function creation" {
    const allocator = std.testing.allocator;

    const sig = FunctionType{
        .params = &[_]FunctionType.Param{
            .{ .name = "x", .ty = .{ .i64 = {} }, .direction = .in },
        },
        .return_type = .{ .i64 = {} },
        .is_variadic = false,
    };

    const func = try Function.init(allocator, "calculate_total", sig);
    defer func.deinit();

    func.setExport("cot_calculate_total");
    try std.testing.expectEqual(Linkage.export_c, func.linkage);
    try std.testing.expectEqualStrings("cot_calculate_total", func.export_name.?);
}

test "ir module with exports" {
    const allocator = std.testing.allocator;

    var module = Module.init(allocator, "pricing");
    defer module.deinit();

    module.library_name = "pricing";

    const sig = FunctionType{
        .params = &[_]FunctionType.Param{},
        .return_type = .void,
        .is_variadic = false,
    };

    const func = try Function.init(allocator, "my_func", sig);
    func.setExport("cot_my_func");
    try module.addFunction(func);

    try std.testing.expectEqual(@as(usize, 1), module.functions.items.len);
}
