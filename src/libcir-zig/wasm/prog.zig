//! Prog — single assembler instruction in a linked-list chain.

const std = @import("std");
const c = @import("constants.zig");

pub const AddrType = enum {
    none,
    reg,
    const_int,
    const_float,
    mem,
    addr,
    branch,
};

pub const AddrName = enum {
    none,
    @"extern",
    static,
    auto,
    param,
};

pub const Addr = struct {
    type: AddrType = .none,
    name: AddrName = .none,
    reg: c.Reg = .none,
    offset: i64 = 0,
    sym: ?*Symbol = null,
    val: Value = .{ .none = {} },

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

pub const Value = union {
    none: void,
    float: f64,
    br_table: []const u64,
};

pub const Symbol = struct {
    name: []const u8,
    index: u32 = 0,
    size: i64 = 0,

    is_func: bool = false,
    frame_size: i32 = 0,
    args_size: i32 = 0,
    locals_size: i32 = 0,
    param_count: u32 = 0,
    float_local_count: u32 = 0,
    gc_ref_locals: []const u32 = &.{},

    text: ?*Prog = null,

    pub fn init(name: []const u8) Symbol {
        return .{ .name = name };
    }
};

pub const Prog = struct {
    as: c.As,
    from: Addr = .{},
    to: Addr = .{},

    link: ?*Prog = null,

    pc: i64 = 0,
    spadj: i32 = 0,

    mark: u32 = 0,

    pub fn init(as: c.As) Prog {
        return .{ .as = as };
    }

    pub fn initFrom(as: c.As, from: Addr) Prog {
        return .{ .as = as, .from = from };
    }

    pub fn initTo(as: c.As, to: Addr) Prog {
        return .{ .as = as, .to = to };
    }
};

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
        while (p) |prog_inst| {
            const next = prog_inst.link;
            self.allocator.destroy(prog_inst);
            p = next;
        }
        self.first = null;
        self.last = null;
        self.count = 0;
    }

    pub fn append(self: *ProgBuilder, as: c.As) !*Prog {
        const p = try self.allocator.create(Prog);
        p.* = Prog.init(as);
        return self.appendProg(p);
    }

    pub fn appendFrom(self: *ProgBuilder, as: c.As, from: Addr) !*Prog {
        const p = try self.allocator.create(Prog);
        p.* = Prog.initFrom(as, from);
        return self.appendProg(p);
    }

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
    while (p) |prog_inst| : (p = prog_inst.link) {
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
