//! Preprocess — transform high-level pseudo-instructions to low-level Wasm.
//!
//! Performs: JMP→dispatch, ARET→epilogue, dispatch loop creation, branch depth resolution.

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const Prog = prog.Prog;
const Addr = prog.Addr;
const Symbol = prog.Symbol;

const debug = @import("foundation").debug;

/// Preprocess a function's instruction chain.
pub fn preprocess(allocator: std.mem.Allocator, sym: *Symbol) !void {
    debug.log(.codegen, "preprocess: '{s}' framesize={d}", .{ sym.name, sym.frame_size });

    const framesize = sym.frame_size;
    if (framesize < 0) {
        return error.BadFrameSize;
    }

    const text = sym.text orelse return;

    var numResumePoints: i64 = 0;
    var pc: i64 = 0;
    var tableIdxs = std.ArrayListUnmanaged(u64){};
    defer tableIdxs.deinit(allocator);
    var tablePC: i64 = 0;

    var explicitBlockDepth: i32 = 0;

    var p: ?*Prog = text;
    while (p) |current| : (p = current.link) {
        switch (current.as) {
            .block, .loop, .@"if" => {
                explicitBlockDepth += 1;
            },
            .end => {
                if (explicitBlockDepth > 0) {
                    explicitBlockDepth -= 1;
                }
            },
            .resume_point => {
                if (explicitBlockDepth == 0) {
                    current.as = .end;
                    while (tablePC <= pc) {
                        try tableIdxs.append(allocator, @intCast(numResumePoints));
                        tablePC += 1;
                    }
                    numResumePoints += 1;
                    pc += 1;
                } else {
                    current.as = .end;
                }
            },
            .call => {
                pc += 1;
            },
            .nop, .text => {
                pc += 1;
            },
            else => {},
        }
        current.pc = pc;
    }

    try tableIdxs.append(allocator, @intCast(numResumePoints));

    debug.log(.codegen, "  pass 1 complete: numResumePoints={d}, pc={d}, tableIdxs={d}", .{
        numResumePoints, pc, tableIdxs.items.len,
    });

    if (framesize > 0) {
        var prologue = text;

        prologue = try appendAfter(allocator, prologue, .get, prog.regAddr(.sp), .{});
        prologue = try appendAfter(allocator, prologue, .i32_const, prog.constAddr(framesize), .{});
        prologue = try appendAfter(allocator, prologue, .i32_sub, .{}, .{});
        prologue = try appendAfter(allocator, prologue, .set, .{}, prog.regAddr(.sp));
        prologue.spadj = framesize;
    }

    var entryPointLoopBranches = std.ArrayListUnmanaged(*Prog){};
    defer entryPointLoopBranches.deinit(allocator);

    var currentDepth: i32 = 0;

    p = text;
    while (p) |current| {
        const next = current.link;

        switch (current.as) {
            .block, .loop, .@"if" => currentDepth += 1,
            .end => currentDepth -= 1,
            else => {},
        }

        switch (current.as) {
            .jmp => {
                const target_prog = current.to.branch_target;

                current.as = .nop;

                if (target_prog) |target| {
                    var emit = current;

                    emit = try appendAfter(allocator, emit, .i32_const, prog.constAddr(target.pc), .{});
                    emit = try appendAfter(allocator, emit, .set, .{}, prog.regAddr(.pc_b));
                    emit = try appendAfter(allocator, emit, .br, .{}, .{});
                    emit.to.type = .branch;
                    try entryPointLoopBranches.append(allocator, emit);
                }
            },

            .aret => {
                current.as = .nop;
                var emit = current;

                if (framesize > 0) {
                    emit = try appendAfter(allocator, emit, .get, prog.regAddr(.sp), .{});
                    emit = try appendAfter(allocator, emit, .i32_const, prog.constAddr(framesize), .{});
                    emit = try appendAfter(allocator, emit, .i32_add, .{}, .{});
                    emit = try appendAfter(allocator, emit, .set, .{}, prog.regAddr(.sp));
                }

                _ = try appendAfter(allocator, emit, .@"return", .{}, .{});
            },

            .@"return" => {},

            .return_call => {
                if (framesize > 0) {
                    const saved_to = current.to;
                    const saved_link = current.link;
                    current.as = .nop;
                    current.to = .{};
                    var emit = current;
                    emit = try appendAfter(allocator, emit, .get, prog.regAddr(.sp), .{});
                    emit = try appendAfter(allocator, emit, .i32_const, prog.constAddr(framesize), .{});
                    emit = try appendAfter(allocator, emit, .i32_add, .{}, .{});
                    emit = try appendAfter(allocator, emit, .set, .{}, prog.regAddr(.sp));
                    emit = try appendAfter(allocator, emit, .return_call, .{}, saved_to);
                    emit.link = saved_link;
                }
            },

            .get => {
                if (current.from.type == .addr) {
                    try expandGetAddr(allocator, current, framesize);
                }
            },

            .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u => {
                if (current.from.type == .mem) {
                    try expandLoadMem(allocator, current);
                }
            },

            .mov_b, .mov_h, .mov_w, .mov_d => {
                try expandMov(allocator, current, framesize);
            },

            else => {},
        }

        p = next;
    }

    p = text;
    while (p) |current| : (p = current.link) {
        switch (current.from.name) {
            .auto => current.from.offset += framesize,
            .param => {
                current.from.reg = .sp;
                current.from.offset += framesize + 8;
            },
            else => {},
        }

        switch (current.to.name) {
            .auto => current.to.offset += framesize,
            .param => {
                current.to.reg = .sp;
                current.to.offset += framesize + 8;
            },
            else => {},
        }
    }

    var entryPointLoop: ?*Prog = null;
    var insert_point = text;

    if (entryPointLoopBranches.items.len > 0) {
        insert_point = try appendAfter(allocator, insert_point, .loop, .{}, .{});
        entryPointLoop = insert_point;

        for (entryPointLoopBranches.items) |b| {
            b.to.branch_target = entryPointLoop;
        }
    }

    if (numResumePoints > 0) {
        var i: i64 = 0;
        while (i < numResumePoints + 1) : (i += 1) {
            insert_point = try appendAfter(allocator, insert_point, .block, .{}, .{});
        }

        insert_point = try appendAfter(allocator, insert_point, .get, prog.regAddr(.pc_b), .{});

        insert_point = try appendAfter(allocator, insert_point, .br_table, .{}, .{});
        const table_copy = try allocator.alloc(u64, tableIdxs.items.len);
        @memcpy(table_copy, tableIdxs.items);
        insert_point.to.val = .{ .br_table = table_copy };

        insert_point = try appendAfter(allocator, insert_point, .end, .{}, .{});
    }

    var last = text;
    while (last.link) |n| {
        last = n;
    }

    if (entryPointLoopBranches.items.len > 0) {
        last = try appendAfter(allocator, last, .end, .{}, .{});
    }

    _ = try appendAfter(allocator, last, .@"unreachable", .{}, .{});

    currentDepth = 0;
    var blockDepths = std.AutoHashMap(*Prog, i32).init(allocator);
    defer blockDepths.deinit();

    p = text;
    while (p) |current| : (p = current.link) {
        switch (current.as) {
            .block, .loop, .@"if" => {
                currentDepth += 1;
                try blockDepths.put(current, currentDepth);
            },
            .end => currentDepth -= 1,
            else => {},
        }

        switch (current.as) {
            .br, .br_if => {
                if (current.to.type == .branch) {
                    if (current.to.branch_target) |target| {
                        if (blockDepths.get(target)) |blockDepth| {
                            const relative_depth = currentDepth - blockDepth;
                            current.to = prog.constAddr(relative_depth);
                        }
                    }
                }
            },
            else => {},
        }
    }

    debug.log(.codegen, "  preprocess complete: {d} branches resolved, dispatch_loop={}, has_loop={}", .{
        @as(usize, if (entryPointLoopBranches.items.len > 0) entryPointLoopBranches.items.len else 0),
        numResumePoints > 0,
        entryPointLoopBranches.items.len > 0,
    });
}

fn expandGetAddr(allocator: std.mem.Allocator, p: *Prog, framesize: i32) !void {
    _ = framesize;
    const from = p.from;
    p.as = .nop;

    var current = p;

    switch (from.name) {
        .@"extern" => {
            current = try appendAfter(allocator, current, .i64_const, from, .{});
        },
        .auto, .param, .none => {
            current = try appendAfter(allocator, current, .get, prog.regAddr(from.reg), .{});

            if (from.reg == .sp) {
                current = try appendAfter(allocator, current, .i64_extend_i32_u, .{}, .{});
            }

            if (from.offset != 0) {
                current = try appendAfter(allocator, current, .i64_const, prog.constAddr(from.offset), .{});
                current = try appendAfter(allocator, current, .i64_add, .{}, .{});
            }
        },
        else => {},
    }
}

fn expandLoadMem(allocator: std.mem.Allocator, p: *Prog) !void {
    var as = p.as;
    const from = p.from;

    if (as == .f64_load) as = .i64_load;
    if (as == .f32_load) as = .i64_load;

    p.as = .get;
    p.from = prog.regAddr(from.reg);
    p.to = .{};

    var current = p;

    if (from.reg != .sp) {
        current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
    }

    _ = try appendAfter(allocator, current, as, prog.constAddr(from.offset), .{});
}

fn expandMov(allocator: std.mem.Allocator, p: *Prog, framesize: i32) !void {
    _ = framesize;

    const mov_as = p.as;
    const from = p.from;
    const to = p.to;

    const load_as: c.As = switch (mov_as) {
        .mov_b => .i64_load8_u,
        .mov_h => .i64_load16_u,
        .mov_w => .i64_load32_u,
        .mov_d => .i64_load,
        else => unreachable,
    };
    const store_as: c.As = switch (mov_as) {
        .mov_b => .i64_store8,
        .mov_h => .i64_store16,
        .mov_w => .i64_store32,
        .mov_d => .i64_store,
        else => unreachable,
    };

    p.as = .nop;
    var current = p;

    if (to.type == .mem) {
        current = try appendAfter(allocator, current, .get, prog.regAddr(to.reg), .{});
        if (to.reg != .sp and to.reg != .pc_b) {
            current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
        }
    }

    switch (from.type) {
        .const_int => {
            current = try appendAfter(allocator, current, .i64_const, prog.constAddr(from.offset), .{});
        },
        .reg => {
            current = try appendAfter(allocator, current, .get, from, .{});
            if (from.reg == .sp or from.reg == .pc_b) {
                current = try appendAfter(allocator, current, .i64_extend_i32_u, .{}, .{});
            }
        },
        .mem => {
            current = try appendAfter(allocator, current, .get, prog.regAddr(from.reg), .{});
            if (from.reg != .sp and from.reg != .pc_b) {
                current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
            }
            current = try appendAfter(allocator, current, load_as, prog.constAddr(from.offset), .{});
        },
        .addr => {
            current = try appendAfter(allocator, current, .get, prog.regAddr(from.reg), .{});
            if (from.reg == .sp or from.reg == .pc_b) {
                current = try appendAfter(allocator, current, .i64_extend_i32_u, .{}, .{});
            }
            if (from.offset != 0) {
                current = try appendAfter(allocator, current, .i64_const, prog.constAddr(from.offset), .{});
                current = try appendAfter(allocator, current, .i64_add, .{}, .{});
            }
        },
        else => {},
    }

    switch (to.type) {
        .reg => {
            if (to.reg == .sp or to.reg == .pc_b) {
                current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
            }
            current = try appendAfter(allocator, current, .set, .{}, to);
        },
        .mem => {
            current = try appendAfter(allocator, current, store_as, .{}, prog.constAddr(to.offset));
        },
        else => {},
    }
}

fn appendAfter(allocator: std.mem.Allocator, prev: *Prog, as: c.As, from: Addr, to: Addr) !*Prog {
    const p = try allocator.create(Prog);
    p.* = .{
        .as = as,
        .from = from,
        .to = to,
        .link = prev.link,
        .pc = prev.pc,
    };
    prev.link = p;
    return p;
}

fn insertBefore(allocator: std.mem.Allocator, target: *Prog, as: c.As, from: Addr, to: Addr) !*Prog {
    const new_target = try allocator.create(Prog);
    new_target.* = target.*;

    target.as = as;
    target.from = from;
    target.to = to;
    target.link = new_target;

    return new_target;
}

const testing = std.testing;

test "preprocess empty function" {
    const allocator = testing.allocator;

    var sym = Symbol.init("test");
    sym.frame_size = 0;

    const text = try allocator.create(Prog);
    text.* = Prog.init(.text);
    sym.text = text;

    defer {
        var pp: ?*Prog = sym.text;
        while (pp) |current| {
            const next = current.link;
            allocator.destroy(current);
            pp = next;
        }
    }

    try preprocess(allocator, &sym);
}

test "preprocess with frame allocation" {
    const allocator = testing.allocator;

    var sym = Symbol.init("test");
    sym.frame_size = 16;

    const text = try allocator.create(Prog);
    text.* = Prog.init(.text);

    const ret = try allocator.create(Prog);
    ret.* = Prog.init(.@"return");
    text.link = ret;

    sym.text = text;

    defer {
        var pp: ?*Prog = sym.text;
        while (pp) |current| {
            const next = current.link;
            allocator.destroy(current);
            pp = next;
        }
    }

    try preprocess(allocator, &sym);

    var count: usize = 0;
    var pp: ?*Prog = sym.text;
    while (pp) |current| {
        count += 1;
        pp = current.link;
    }

    try testing.expect(count >= 5);
}

test "preprocess with resume points creates br_table dispatch" {
    const allocator = testing.allocator;

    var sym = Symbol.init("test");
    sym.frame_size = 0;

    const text = try allocator.create(Prog);
    text.* = Prog.init(.text);

    const resume_prog = try allocator.create(Prog);
    resume_prog.* = Prog.init(.resume_point);
    text.link = resume_prog;

    const ret = try allocator.create(Prog);
    ret.* = Prog.init(.@"return");
    resume_prog.link = ret;

    sym.text = text;

    defer {
        var pp: ?*Prog = sym.text;
        while (pp) |current| {
            const next = current.link;
            if (current.as == .br_table) {
                if (current.to.val.br_table.len > 0) {
                    allocator.free(current.to.val.br_table);
                }
            }
            allocator.destroy(current);
            pp = next;
        }
    }

    try preprocess(allocator, &sym);

    var has_loop = false;
    var has_block = false;
    var has_br_table = false;

    var pp: ?*Prog = sym.text;
    while (pp) |current| {
        switch (current.as) {
            .loop => has_loop = true,
            .block => has_block = true,
            .br_table => has_br_table = true,
            else => {},
        }
        pp = current.link;
    }

    try testing.expect(!has_loop);
    try testing.expect(has_block);
    try testing.expect(has_br_table);
}
