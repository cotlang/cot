//! Global values.
//!
//! Port of cranelift/codegen/src/ir/globalvalue.rs
//!
//! This module defines GlobalValueData which describes how to compute
//! the value of a GlobalValue reference.

const std = @import("std");
const types = @import("types.zig");
const dfg_mod = @import("dfg.zig");

pub const Type = types.Type;
pub const GlobalValue = dfg_mod.GlobalValue;

// ============================================================================
// External Name
// Duplicated here to avoid circular import with function.zig
// Port of cranelift/codegen/src/ir/extname.rs
// ============================================================================

/// External name for symbols.
///
/// This is a simplified version of Cranelift's ExternalName, defined here
/// to avoid circular imports between globalvalue.zig and function.zig.
pub const ExternalName = union(enum) {
    /// User-defined name (namespace + index).
    user: struct {
        namespace: u32,
        index: u32,
    },
    /// Library call name.
    libcall: []const u8,

    pub fn initUser(namespace: u32, index: u32) ExternalName {
        return .{ .user = .{ .namespace = namespace, .index = index } };
    }

    pub fn initLibcall(name: []const u8) ExternalName {
        return .{ .libcall = name };
    }
};

// ============================================================================
// Memory Flags (duplicated to avoid circular import)
// Port of cranelift memflags - subset needed for GlobalValueData
// ============================================================================

/// Memory operation flags for global value loads.
pub const MemFlags = packed struct(u8) {
    /// Memory is known to be aligned.
    aligned: bool = false,
    /// Memory access is readonly.
    readonly: bool = false,
    /// Trap on null address.
    trap_on_null: bool = false,
    /// Heap memory access.
    heap: bool = false,
    /// Memory is known to be notrap (guaranteed accessible).
    notrap: bool = false,
    _padding: u3 = 0,

    pub const DEFAULT: MemFlags = .{};

    /// Create flags that are trusted (notrap + aligned).
    pub fn trusted() MemFlags {
        return .{
            .notrap = true,
            .aligned = true,
        };
    }

    pub fn withReadonly(self: MemFlags) MemFlags {
        var f = self;
        f.readonly = true;
        return f;
    }

    pub fn withAligned(self: MemFlags) MemFlags {
        var f = self;
        f.aligned = true;
        return f;
    }

    pub fn withNotrap(self: MemFlags) MemFlags {
        var f = self;
        f.notrap = true;
        return f;
    }
};

// ============================================================================
// Offset Types
// Port of cranelift/codegen/src/ir/immediates.rs
// ============================================================================

/// A 32-bit signed offset for memory operations.
pub const Offset32 = i32;

/// A 64-bit signed immediate value.
pub const Imm64 = i64;

// ============================================================================
// GlobalValueData
// Port of cranelift/codegen/src/ir/globalvalue.rs:14-84
// ============================================================================

/// Information about a global value declaration.
///
/// A GlobalValue can represent various kinds of symbolic or computed addresses:
/// - The VM context struct address
/// - A value loaded from another global value
/// - An offset from another global value
/// - A symbolic name to be resolved by the linker
/// - A dynamic scale constant for vector operations
pub const GlobalValueData = union(enum) {
    /// Value is the address of the VM context struct.
    ///
    /// This represents the base address of the runtime context,
    /// typically passed as a hidden parameter to functions.
    vmcontext,

    /// Value is pointed to by another global value.
    ///
    /// The `base` global value is assumed to contain a pointer. This global value
    /// is computed by loading from memory at that pointer value. The memory must
    /// be accessible, and naturally aligned to hold a value of the type. The data
    /// at this address is assumed to never change while the current function is
    /// executing.
    load: struct {
        /// The base pointer global value.
        base: GlobalValue,
        /// Offset added to the base pointer before doing the load.
        offset: Offset32,
        /// Type of the loaded value.
        global_type: Type,
        /// Specifies the memory flags to be used by the load.
        /// Guaranteed to be notrap and aligned.
        flags: MemFlags,
    },

    /// Value is an offset from another global value.
    ///
    /// This is used to compute addresses relative to another global value
    /// without performing a memory load.
    iadd_imm: struct {
        /// The base pointer global value.
        base: GlobalValue,
        /// Byte offset to be added to the value.
        offset: Imm64,
        /// Type of the iadd result.
        global_type: Type,
    },

    /// Value is symbolic, meaning it's a name which will be resolved to an
    /// actual value later (eg. by linking). Cranelift itself does not interpret
    /// this name; it's used by embedders to link with other data structures.
    ///
    /// For now, symbolic values always have pointer type, and represent
    /// addresses, however in the future they could be used to represent other
    /// things as well.
    symbol: struct {
        /// The symbolic name.
        name: ExternalName,
        /// Offset from the symbol. This can be used instead of IAddImm to
        /// represent folding an offset into a symbol.
        offset: Imm64,
        /// Will this symbol be defined nearby, such that it will always be a
        /// certain distance away, after linking? If so, references to it can
        /// avoid going through a GOT. Note that symbols meant to be preemptible
        /// cannot be colocated.
        ///
        /// If `true`, some backends may use relocation forms that have limited
        /// range: for example, a +/- 2^27-byte range on AArch64.
        colocated: bool,
        /// Does this symbol refer to a thread local storage value?
        tls: bool,
    },

    /// Value is a multiple of how many instances of `vector_type` will fit in
    /// a target vector register.
    ///
    /// This is used for dynamic vector operations where the scale depends on
    /// the target's vector register size.
    dyn_scale_target_const: struct {
        /// Base vector type.
        vector_type: Type,
    },

    const Self = @This();

    /// Assume that `self` is a `GlobalValueData.symbol` and return its name.
    pub fn symbolName(self: Self) ExternalName {
        return switch (self) {
            .symbol => |s| s.name,
            else => @panic("only symbols have names"),
        };
    }

    /// Return the type of this global.
    ///
    /// For vmcontext and symbol, this returns the pointer type.
    /// For load and iadd_imm, this returns the specified global_type.
    /// For dyn_scale_target_const, this returns the pointer type.
    pub fn globalType(self: Self, pointer_type: Type) Type {
        return switch (self) {
            .vmcontext, .symbol, .dyn_scale_target_const => pointer_type,
            .load => |d| d.global_type,
            .iadd_imm => |d| d.global_type,
        };
    }

    /// Check if this global value data represents a symbol.
    pub fn isSymbol(self: Self) bool {
        return switch (self) {
            .symbol => true,
            else => false,
        };
    }

    /// Check if this global value data is colocated (for symbols).
    pub fn isColocated(self: Self) bool {
        return switch (self) {
            .symbol => |s| s.colocated,
            else => false,
        };
    }

    /// Check if this global value data refers to TLS (for symbols).
    pub fn isTls(self: Self) bool {
        return switch (self) {
            .symbol => |s| s.tls,
            else => false,
        };
    }

    /// Format for display.
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .vmcontext => try writer.writeAll("vmctx"),
            .load => |d| {
                try writer.print("load.{} ", .{d.global_type});
                if (d.flags.notrap) try writer.writeAll("notrap ");
                if (d.flags.aligned) try writer.writeAll("aligned ");
                try writer.print("{}", .{d.base});
                if (d.offset != 0) {
                    if (d.offset > 0) {
                        try writer.print("+{d}", .{d.offset});
                    } else {
                        try writer.print("{d}", .{d.offset});
                    }
                }
            },
            .iadd_imm => |d| {
                try writer.print("iadd_imm.{} {}, {d}", .{ d.global_type, d.base, d.offset });
            },
            .symbol => |s| {
                try writer.writeAll("symbol ");
                if (s.colocated) try writer.writeAll("colocated ");
                if (s.tls) try writer.writeAll("tls ");
                // ExternalName formatting would go here
                try writer.writeAll("<name>");
                if (s.offset != 0) {
                    if (s.offset > 0) {
                        try writer.print("+{d}", .{s.offset});
                    } else {
                        try writer.print("{d}", .{s.offset});
                    }
                }
            },
            .dyn_scale_target_const => |d| {
                try writer.print("dyn_scale_target_const.{}", .{d.vector_type});
            },
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

// Note: Tests for GlobalValueData methods are in function.zig to avoid
// module resolution issues when importing via the build system.

test "GlobalValueData basic" {
    // Basic existence test - method tests are in function.zig
    const gvd = GlobalValueData{ .vmcontext = {} };
    _ = gvd;
}

test "GlobalValueData load creation" {
    const gvd = GlobalValueData{
        .load = .{
            .base = GlobalValue.fromIndex(0),
            .offset = 8,
            .global_type = Type.I32,
            .flags = MemFlags.trusted(),
        },
    };
    _ = gvd;
}

test "GlobalValueData iadd_imm creation" {
    const gvd = GlobalValueData{
        .iadd_imm = .{
            .base = GlobalValue.fromIndex(0),
            .offset = 16,
            .global_type = Type.I64,
        },
    };
    _ = gvd;
}

test "GlobalValueData symbol creation" {
    const gvd = GlobalValueData{
        .symbol = .{
            .name = ExternalName.initUser(0, 0),
            .offset = 0,
            .colocated = true,
            .tls = false,
        },
    };
    _ = gvd;
}
