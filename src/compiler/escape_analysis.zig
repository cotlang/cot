//! Escape Analysis for Stack Pointer Optimization
//!
//! Determines which local struct variables need heap allocation because
//! their addresses escape the function. Variables that don't escape can
//! safely live on the stack.
//!
//! A local variable's address **escapes** if:
//! 1. Returned from function
//! 2. Stored in heap object (struct field, list element, map value)
//! 3. Stored in global variable
//! 4. Passed to unknown callee that might store it
//!
//! Method `self` parameters are always safe - the callee can't outlive caller.

const std = @import("std");
const ir = @import("../ir/ir.zig");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.escape_analysis);

/// Escape state for a local variable
pub const EscapeState = enum {
    /// Safe to allocate on stack - address doesn't escape
    stack,
    /// Must heap allocate - address escapes the function
    heap,
    /// Not yet determined (used during analysis)
    unknown,
};

/// Result of escape analysis for a function
pub const EscapeAnalysisResult = struct {
    /// Maps local variable name to its escape state
    states: std.StringHashMapUnmanaged(EscapeState),
    allocator: Allocator,

    pub fn init(allocator: Allocator) EscapeAnalysisResult {
        return .{
            .states = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *EscapeAnalysisResult) void {
        self.states.deinit(self.allocator);
    }

    /// Get escape state for a local, defaulting to stack if not found
    pub fn get(self: *const EscapeAnalysisResult, name: []const u8) EscapeState {
        return self.states.get(name) orelse .stack;
    }

    /// Check if a local needs heap allocation
    pub fn needsHeap(self: *const EscapeAnalysisResult, name: []const u8) bool {
        return self.get(name) == .heap;
    }
};

/// Analyze a function to determine which locals escape
pub fn analyzeFunction(allocator: Allocator, func: *ir.Function) !EscapeAnalysisResult {
    var result = EscapeAnalysisResult.init(allocator);
    errdefer result.deinit();

    // Initialize all struct locals as stack-safe
    for (func.locals.items) |local| {
        if (local.ty == .@"struct") {
            try result.states.put(allocator, local.name, .stack);
        }
    }

    // Analyze each block's instructions
    for (func.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            try analyzeInstruction(allocator, &result, func, inst);
        }
    }

    return result;
}

/// Analyze a single instruction for escape patterns
fn analyzeInstruction(
    allocator: Allocator,
    result: *EscapeAnalysisResult,
    func: *ir.Function,
    inst: ir.Instruction,
) !void {
    switch (inst) {
        // Return statement - check if returning address of local
        .return_ => |ret_val| {
            if (ret_val) |val| {
                if (getAddressOfLocal(func, val)) |local_name| {
                    log.debug("Local '{s}' escapes via return", .{local_name});
                    try result.states.put(allocator, local_name, .heap);
                }
            }
        },

        // Store instruction - check if storing address to heap location
        .store => |s| {
            if (getAddressOfLocal(func, s.value)) |local_name| {
                // Check if destination is a heap location
                if (isHeapLocation(func, s.ptr)) {
                    log.debug("Local '{s}' escapes via store to heap", .{local_name});
                    try result.states.put(allocator, local_name, .heap);
                }
            }
        },

        // Store to heap record field
        .store_field_heap => |s| {
            if (getAddressOfLocal(func, s.value)) |local_name| {
                log.debug("Local '{s}' escapes via store_field_heap", .{local_name});
                try result.states.put(allocator, local_name, .heap);
            }
        },

        // Function call - check if passing address to unknown callee
        .call => |c| {
            for (c.args, 0..) |arg, i| {
                if (getAddressOfLocal(func, arg)) |local_name| {
                    // Method self parameter (index 0) is safe
                    if (!isMethodSelfParam(c.callee, i)) {
                        if (!isKnownSafeCallee(c.callee)) {
                            log.debug("Local '{s}' escapes via call to '{s}'", .{ local_name, c.callee });
                            try result.states.put(allocator, local_name, .heap);
                        }
                    }
                }
            }
        },

        // List push - storing value escapes it
        .list_push => |lp| {
            if (getAddressOfLocal(func, lp.value)) |local_name| {
                log.debug("Local '{s}' escapes via list_push", .{local_name});
                try result.states.put(allocator, local_name, .heap);
            }
        },

        // Map set - storing value escapes it
        .map_set => |ms| {
            if (getAddressOfLocal(func, ms.value)) |local_name| {
                log.debug("Local '{s}' escapes via map_set", .{local_name});
                try result.states.put(allocator, local_name, .heap);
            }
        },

        // Other instructions don't cause escapes
        else => {},
    }
}

/// Check if a value is the address of a local struct variable
fn getAddressOfLocal(func: *ir.Function, value: ir.Value) ?[]const u8 {
    // A pointer-to-struct value might be an address-of local
    if (value.ty == .ptr) {
        if (value.ty.ptr.* == .@"struct") {
            // Look up the value in locals to find its name
            for (func.locals.items) |local| {
                if (local.value.id == value.id) {
                    return local.name;
                }
            }
        }
    }
    return null;
}

/// Check if a pointer points to a heap location
fn isHeapLocation(func: *ir.Function, ptr: ir.Value) bool {
    _ = func;
    // Heap locations include:
    // - Fields of heap records (heap_record type)
    // - Global variables
    // - List/Map elements
    return switch (ptr.ty) {
        .heap_record => true,
        .ptr => |p| p.* == .heap_record,
        else => false,
    };
}

/// Check if callee is known to not store pointer arguments
fn isKnownSafeCallee(callee: []const u8) bool {
    // Built-in functions that don't store their arguments
    const safe_builtins = [_][]const u8{
        "println",
        "print",
        "string",
        "len",
        "assert",
    };

    for (safe_builtins) |safe| {
        if (std.mem.eql(u8, callee, safe)) {
            return true;
        }
    }

    return false;
}

/// Check if this is the self parameter of a method call
fn isMethodSelfParam(callee: []const u8, param_idx: usize) bool {
    // Method calls have format "TypeName.methodName"
    // The first parameter (index 0) is always self
    if (param_idx != 0) return false;

    // Check if callee contains a dot (method call pattern)
    return std.mem.indexOf(u8, callee, ".") != null;
}

// ============================================================================
// Tests
// ============================================================================

test "escape analysis - basic stack safe" {
    const allocator = std.testing.allocator;

    // Create a simple function with a local that doesn't escape
    var func = try ir.Function.init(allocator, "test_func", .{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .void,
        .is_variadic = false,
    });
    defer func.deinit();

    // Add a struct local
    const struct_type = ir.StructType{
        .name = "Point",
        .fields = &[_]ir.StructType.Field{},
        .size = 16,
        .alignment = 8,
    };
    const local_val = func.newValue(.{ .@"struct" = &struct_type });
    try func.locals.append(allocator, .{
        .name = "p",
        .ty = .{ .@"struct" = &struct_type },
        .value = local_val,
    });

    var result = try analyzeFunction(allocator, func);
    defer result.deinit();

    // Local should be stack-safe since it doesn't escape
    try std.testing.expectEqual(EscapeState.stack, result.get("p"));
}
