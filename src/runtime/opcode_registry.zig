//! Opcode Registry
//!
//! Allows extensions to register custom opcode handlers for reserved
//! opcode ranges. Core opcodes (0x00-0xCF) are fixed, while extension
//! opcodes (0xD0-0xFE) can be registered by extensions.

const std = @import("std");
const Allocator = std.mem.Allocator;

// Forward declare VM types to avoid circular imports
pub const Module = @import("bytecode/module.zig").Module;

/// Opcode ranges for extensions
pub const OpcodeRange = enum(u8) {
    /// DBL extension: 0xD0-0xDF (16 opcodes)
    dbl_start = 0xD0,
    dbl_end = 0xDF,

    /// Collections extension: 0xE0-0xEF (16 opcodes)
    collections_start = 0xE0,
    collections_end = 0xEF,

    /// User extensions: 0xF0-0xFE (15 opcodes)
    user_start = 0xF0,
    user_end = 0xFE,

    // 0xFF reserved for halt/invalid
};

/// First opcode available for extensions
pub const FIRST_EXTENSION_OPCODE: u8 = 0xD0;

/// Last opcode available for extensions
pub const LAST_EXTENSION_OPCODE: u8 = 0xFE;

/// Dispatch result from opcode handler
pub const DispatchResult = enum {
    continue_dispatch,
    return_from_main,
    yield,
};

/// VM error type (simplified, actual errors in vm.zig)
pub const VMError = error{
    InvalidOpcode,
    InvalidType,
    OutOfMemory,
    StackOverflow,
    StackUnderflow,
    DivisionByZero,
    InvalidConstant,
    InvalidJump,
    InvalidCall,
    NativeError,
    Halt,
};

/// Opcode handler function signature
/// Takes VM pointer (as anyopaque to avoid circular import) and module
pub const OpcodeHandler = *const fn (vm: *anyopaque, module: *const Module) VMError!DispatchResult;

/// Metadata for a registered opcode
pub const OpcodeMeta = struct {
    /// Human-readable name
    name: []const u8,
    /// Extension that registered it
    extension: []const u8,
};

/// Registry for extension opcodes
pub const OpcodeRegistry = struct {
    handlers: [256]?OpcodeHandler,
    metadata: [256]?OpcodeMeta,

    const Self = @This();

    pub fn init() Self {
        return .{
            .handlers = .{null} ** 256,
            .metadata = .{null} ** 256,
        };
    }

    // No deinit needed - no heap allocations

    /// Register a handler for an opcode
    /// Only extension opcodes (0xD0-0xFE) can be registered
    pub fn register(
        self: *Self,
        opcode: u8,
        handler: OpcodeHandler,
        meta: OpcodeMeta,
    ) !void {
        // Validate opcode is in extension range
        if (!isExtensionOpcode(opcode)) {
            return error.InvalidOpcode;
        }

        // Check if already registered
        if (self.handlers[opcode] != null) {
            return error.InvalidOpcode; // Already registered
        }

        self.handlers[opcode] = handler;
        self.metadata[opcode] = meta;
    }

    /// Get handler for an opcode
    pub fn getHandler(self: *const Self, opcode: u8) ?OpcodeHandler {
        return self.handlers[opcode];
    }

    /// Get metadata for an opcode
    pub fn getMeta(self: *const Self, opcode: u8) ?OpcodeMeta {
        return self.metadata[opcode];
    }

    /// Check if opcode is in extension range
    pub fn isExtensionOpcode(opcode: u8) bool {
        return opcode >= FIRST_EXTENSION_OPCODE and opcode <= LAST_EXTENSION_OPCODE;
    }

    /// Check if an opcode range is available
    pub fn isRangeAvailable(self: *const Self, start: u8, end: u8) bool {
        var i: u8 = start;
        while (i <= end) : (i += 1) {
            if (self.handlers[i] != null) return false;
        }
        return true;
    }

    /// Get the name of an opcode (for disassembly/debugging)
    pub fn getOpcodeName(self: *const Self, opcode: u8) []const u8 {
        if (self.metadata[opcode]) |meta| {
            return meta.name;
        }
        return "unknown";
    }
};

// ============================================================================
// Tests
// ============================================================================

test "OpcodeRegistry basic operations" {
    var registry = OpcodeRegistry.init();

    // Define a test handler
    const test_handler = struct {
        fn handler(_: *anyopaque, _: *const Module) VMError!DispatchResult {
            return .continue_dispatch;
        }
    }.handler;

    // Register an opcode in the extension range
    try registry.register(0xD5, test_handler, .{
        .name = "test_op",
        .extension = "test",
    });

    // Verify registration
    try std.testing.expect(registry.getHandler(0xD5) != null);
    try std.testing.expectEqualStrings("test_op", registry.getOpcodeName(0xD5));

    // Cannot register core opcodes
    try std.testing.expectError(error.InvalidOpcode, registry.register(0x10, test_handler, .{
        .name = "bad",
        .extension = "test",
    }));

    // Cannot re-register
    try std.testing.expectError(error.InvalidOpcode, registry.register(0xD5, test_handler, .{
        .name = "dup",
        .extension = "test",
    }));
}

test "isExtensionOpcode" {
    try std.testing.expect(!OpcodeRegistry.isExtensionOpcode(0x00));
    try std.testing.expect(!OpcodeRegistry.isExtensionOpcode(0xCF));
    try std.testing.expect(OpcodeRegistry.isExtensionOpcode(0xD0));
    try std.testing.expect(OpcodeRegistry.isExtensionOpcode(0xFE));
    try std.testing.expect(!OpcodeRegistry.isExtensionOpcode(0xFF));
}
