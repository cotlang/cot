//! Prog - Instruction in the assembler chain
//!
//! Go reference: cmd/internal/obj/link.go (Prog struct)
//!
//! A Prog represents a single assembler instruction. Instructions form
//! a linked list that represents the function body. The preprocess pass
//! transforms high-level instructions (CALL, RET) into low-level Wasm
//! instructions.

const std = @import("std");
const c = @import("constants.zig");

/// Address mode/type (like Go's obj.AddrType)
pub const AddrType = enum {
    none, // No operand
    reg, // Register
    const_int, // Integer constant
    const_float, // Float constant
    mem, // Memory reference (base + offset)
    addr, // Address of symbol
    branch, // Branch target (pointer to Prog)
};

/// Name type for memory/address operands (like Go's obj.AddrName)
pub const AddrName = enum {
    none,
    @"extern", // External symbol
    static, // Static symbol
    auto, // Stack-allocated local
    param, // Function parameter
};

/// Operand address (like Go's obj.Addr)
pub const Addr = struct {
    type: AddrType = .none,
    name: AddrName = .none,
    reg: c.Reg = .none,
    offset: i64 = 0,
    sym: ?*Symbol = null,
    val: Value = .{ .none = {} },

    /// Branch target (only valid when type == .branch)
    branch_target: ?*Prog = null,

    pub fn initReg(reg: c.Reg) Addr {
        return .{ .type = .reg, .reg = reg };
    }

    pub fn initConst(offset: i64) Addr {
        return .{ .type = .const_int, .offset = offset };
    }

    pub fn initFloat(val: f64) Addr {
        return .{ .type = .const_float, .val = .{ .float = val } };
    }

    pub fn initMem(reg: c.Reg, offset: i64) Addr {
        return .{ .type = .mem, .reg = reg, .offset = offset };
    }

};

/// Value union for Addr
pub const Value = union {
    none: void,
    float: f64,
    br_table: []const u64, // For br_table instruction
};

/// Symbol reference (like Go's obj.LSym)
pub const Symbol = struct {
    name: []const u8,
    index: u32 = 0, // Function index after linking
    size: i64 = 0,

    // For functions
    is_func: bool = false,
    frame_size: i32 = 0, // Stack frame size
    args_size: i32 = 0, // Arguments size
    locals_size: i32 = 0, // Locals size
    param_count: u32 = 0, // Number of function parameters
    float_local_count: u32 = 0, // Number of f64 locals (contiguous at end of declared range)
    gc_ref_locals: []const u32 = &.{}, // GC type indices for ref-typed locals (after f64 locals)

    // The Prog chain for this function
    text: ?*Prog = null,

    pub fn init(name: []const u8) Symbol {
        return .{ .name = name };
    }
};

/// A single instruction (like Go's obj.Prog)
pub const Prog = struct {
    as: c.As, // Instruction type
    from: Addr = .{}, // Source operand
    to: Addr = .{}, // Destination operand

    // Linked list
    link: ?*Prog = null, // Next instruction

    // Metadata
    pc: i64 = 0, // Program counter / block ID
    spadj: i32 = 0, // SP adjustment after this instruction

    // For pseudo-instructions
    mark: u32 = 0, // Flags

    /// Create a new Prog with the given instruction
    pub fn init(as: c.As) Prog {
        return .{ .as = as };
    }

    /// Create a new Prog with from operand
    pub fn initFrom(as: c.As, from: Addr) Prog {
        return .{ .as = as, .from = from };
    }

    /// Create a new Prog with to operand
    pub fn initTo(as: c.As, to: Addr) Prog {
        return .{ .as = as, .to = to };
    }

};

/// Instructions that use the destination operand as their only operand
/// (Go: wasmobj.go lines 99-117)
pub fn isUnaryDst(as: c.As) bool {
    return switch (as) {
        .set,
        .tee,
        .call,
        .call_indirect,
        .br,
        .br_if,
        .br_table,
        .i32_store,
        .i64_store,
        .f32_store,
        .f64_store,
        .i32_store8,
        .i32_store16,
        .i64_store8,
        .i64_store16,
        .i64_store32,
        .call_no_resume,
        => true,
        else => false,
    };
}

/// Prog list builder for constructing function bodies
pub const ProgBuilder = struct {
    allocator: std.mem.Allocator,
    first: ?*Prog = null,
    last: ?*Prog = null,
    count: usize = 0,

    pub fn init(allocator: std.mem.Allocator) ProgBuilder {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *ProgBuilder) void {
        var p = self.first;
        while (p) |prog| {
            const next = prog.link;
            self.allocator.destroy(prog);
            p = next;
        }
        self.first = null;
        self.last = null;
        self.count = 0;
    }

    /// Append a new instruction
    pub fn append(self: *ProgBuilder, as: c.As) !*Prog {
        const p = try self.allocator.create(Prog);
        p.* = Prog.init(as);
        return self.appendProg(p);
    }

    /// Append a new instruction with from operand
    pub fn appendFrom(self: *ProgBuilder, as: c.As, from: Addr) !*Prog {
        const p = try self.allocator.create(Prog);
        p.* = Prog.initFrom(as, from);
        return self.appendProg(p);
    }

    /// Append a new instruction with to operand
    pub fn appendTo(self: *ProgBuilder, as: c.As, to: Addr) !*Prog {
        const p = try self.allocator.create(Prog);
        p.* = Prog.initTo(as, to);
        return self.appendProg(p);
    }

    fn appendProg(self: *ProgBuilder, p: *Prog) *Prog {
        if (self.last) |last| {
            last.link = p;
        } else {
            self.first = p;
        }
        self.last = p;
        self.count += 1;
        return p;
    }

};

// ============================================================================
// Helper constructors (like Go's constAddr, regAddr)
// ============================================================================

pub fn constAddr(value: i64) Addr {
    return Addr.initConst(value);
}

pub fn regAddr(reg: c.Reg) Addr {
    return Addr.initReg(reg);
}

pub fn floatAddr(value: f64) Addr {
    return Addr.initFloat(value);
}

pub fn memAddr(reg: c.Reg, offset: i64) Addr {
    return Addr.initMem(reg, offset);
}

// ============================================================================
// Tests
// ============================================================================

test "prog builder basic" {
    const allocator = std.testing.allocator;
    var builder = ProgBuilder.init(allocator);
    defer builder.deinit();

    _ = try builder.append(.i64_const);
    _ = try builder.append(.i64_add);
    _ = try builder.append(.end);

    try std.testing.expectEqual(@as(usize, 3), builder.count);

    var count: usize = 0;
    var p = builder.first;
    while (p) |prog| : (p = prog.link) {
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);
}

test "prog with operands" {
    const allocator = std.testing.allocator;
    var builder = ProgBuilder.init(allocator);
    defer builder.deinit();

    const p1 = try builder.appendFrom(.i64_const, constAddr(42));
    try std.testing.expectEqual(@as(i64, 42), p1.from.offset);

    const p2 = try builder.appendFrom(.get, regAddr(.sp));
    try std.testing.expectEqual(c.Reg.sp, p2.from.reg);
}

test "unary dst instructions" {
    try std.testing.expect(isUnaryDst(.set));
    try std.testing.expect(isUnaryDst(.call));
    try std.testing.expect(isUnaryDst(.br));
    try std.testing.expect(!isUnaryDst(.get));
    try std.testing.expect(!isUnaryDst(.i64_add));
}
