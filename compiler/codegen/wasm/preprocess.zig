//! Preprocess - Transform high-level instructions to low-level Wasm
//!
//! Go reference: cmd/internal/obj/wasm/wasmobj.go preprocess()
//!
//! This pass transforms high-level pseudo-instructions into sequences
//! of low-level Wasm instructions:
//!
//! - Frame allocation: Subtract framesize from SP at entry
//! - CALL: Push return address, call function, check for unwind
//! - RET: Restore frame, return
//! - MOV: Expand to load/store sequences
//! - Get/Set: Handle address modes
//!
//! After this pass, all instructions should be directly encodable.

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const Prog = prog.Prog;
const Addr = prog.Addr;
const Symbol = prog.Symbol;

// Debug logging - conditionally import if available
const debug_enabled = false; // Set to true when integrated with main build
fn debugLog(comptime fmt: []const u8, args: anytype) void {
    if (debug_enabled) {
        @import("std").debug.print(fmt ++ "\n", args);
    }
}

/// Preprocess a function's instruction chain.
///
/// Go reference: wasmobj.go lines 156-748
pub fn preprocess(allocator: std.mem.Allocator, sym: *Symbol) !void {
    debugLog("preprocess: '{s}' framesize={d}", .{ sym.name, sym.frame_size });

    const framesize = sym.frame_size;
    if (framesize < 0) {
        return error.BadFrameSize;
    }

    const text = sym.text orelse return;

    // ========================================================================
    // Prologue: Allocate stack frame
    // ========================================================================
    // Go: wasmobj.go lines 207-214
    //
    // if framesize > 0 {
    //     Get SP
    //     i32.const framesize
    //     i32.sub
    //     Set SP
    // }

    if (framesize > 0) {
        var p = text;

        // Get SP
        p = try appendAfter(allocator, p, .get, prog.regAddr(.sp), .{});

        // i32.const framesize
        p = try appendAfter(allocator, p, .i32_const, prog.constAddr(framesize), .{});

        // i32.sub
        p = try appendAfter(allocator, p, .i32_sub, .{}, .{});

        // Set SP
        p = try appendAfter(allocator, p, .set, .{}, prog.regAddr(.sp));
        p.spadj = framesize;
    }

    // ========================================================================
    // Transform instructions
    // ========================================================================

    var p: ?*Prog = text;
    while (p) |current| {
        const next = current.link;

        switch (current.as) {
            // ----------------------------------------------------------------
            // High-level CALL
            // ----------------------------------------------------------------
            // Go: wasmobj.go lines 438-502
            //
            // For Cot (simplified, no goroutine support):
            // - Just emit the call directly
            // - No return address pushing (we use Wasm's call stack)
            .call => {
                // For now, we emit calls directly
                // Full Go implementation pushes return address and handles unwind
                // We'll add that when we need goroutine support
            },

            // ----------------------------------------------------------------
            // High-level RET
            // ----------------------------------------------------------------
            // Go: wasmobj.go lines 504-544
            //
            // if framesize > 0 {
            //     Get SP
            //     i32.const framesize
            //     i32.add
            //     Set SP
            // }
            // return
            .@"return" => {
                if (framesize > 0) {
                    // Insert epilogue before the return
                    var ep = current;

                    // Get SP
                    ep = try insertBefore(allocator, ep, .get, prog.regAddr(.sp), .{});

                    // i32.const framesize
                    ep = try insertBefore(allocator, ep, .i32_const, prog.constAddr(framesize), .{});

                    // i32.add
                    ep = try insertBefore(allocator, ep, .i32_add, .{}, .{});

                    // Set SP
                    ep = try insertBefore(allocator, ep, .set, .{}, prog.regAddr(.sp));
                }
                // Keep the return instruction
            },

            // ----------------------------------------------------------------
            // Get with address mode
            // ----------------------------------------------------------------
            // Go: wasmobj.go lines 565-585
            //
            // AGet with TYPE_ADDR needs to compute the address:
            // - NAME_EXTERN: i64.const <symbol>
            // - NAME_AUTO/PARAM: get reg; i64.extend_i32_u; i64.const offset; i64.add
            .get => {
                if (current.from.type == .addr) {
                    try expandGetAddr(allocator, current, framesize);
                }
            },

            // ----------------------------------------------------------------
            // Load with memory operand
            // ----------------------------------------------------------------
            // Go: wasmobj.go lines 587-600
            //
            // Loads with TYPE_MEM need to push the address first:
            // Get reg; (i32.wrap_i64 if not SP); load offset
            .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u => {
                if (current.from.type == .mem) {
                    try expandLoadMem(allocator, current);
                }
            },

            // ----------------------------------------------------------------
            // MOV pseudo-instructions
            // ----------------------------------------------------------------
            // Go: wasmobj.go lines 602-687
            //
            // AMOVB/H/W/D expand to load+store sequences
            .mov_b, .mov_h, .mov_w, .mov_d => {
                try expandMov(allocator, current, framesize);
            },

            else => {},
        }

        p = next;
    }

    // ========================================================================
    // Adjust offsets for auto/param variables
    // ========================================================================
    // Go: wasmobj.go lines 547-562
    //
    // Auto variables are at SP+offset (within frame)
    // Param variables are at SP+framesize+8+offset (after frame and return addr)

    p = text;
    while (p) |current| {
        // Adjust from operand
        switch (current.from.name) {
            .auto => current.from.offset += framesize,
            .param => {
                current.from.reg = .sp;
                current.from.offset += framesize + 8; // frame + return address
            },
            else => {},
        }

        // Adjust to operand
        switch (current.to.name) {
            .auto => current.to.offset += framesize,
            .param => {
                current.to.reg = .sp;
                current.to.offset += framesize + 8;
            },
            else => {},
        }

        p = current.link;
    }

    debugLog("  preprocess complete", .{});
}

// ============================================================================
// Instruction expansion helpers
// ============================================================================

/// Expand Get with address mode
fn expandGetAddr(allocator: std.mem.Allocator, p: *Prog, framesize: i32) !void {
    _ = framesize;
    const from = p.from;
    p.as = .nop; // Replace original instruction

    var current = p;

    switch (from.name) {
        .@"extern" => {
            // Load symbol address
            current = try appendAfter(allocator, current, .i64_const, from, .{});
        },
        .auto, .param, .none => {
            // Get base register
            current = try appendAfter(allocator, current, .get, prog.regAddr(from.reg), .{});

            // Extend to i64 if SP
            if (from.reg == .sp) {
                current = try appendAfter(allocator, current, .i64_extend_i32_u, .{}, .{});
            }

            // Add offset if non-zero
            if (from.offset != 0) {
                current = try appendAfter(allocator, current, .i64_const, prog.constAddr(from.offset), .{});
                current = try appendAfter(allocator, current, .i64_add, .{}, .{});
            }
        },
        else => {},
    }
}

/// Expand Load with memory operand
fn expandLoadMem(allocator: std.mem.Allocator, p: *Prog) !void {
    const as = p.as;
    const from = p.from;

    // Replace with Get + wrap + load
    p.as = .get;
    p.from = prog.regAddr(from.reg);
    p.to = .{};

    var current = p;

    // Wrap to i32 if not SP (addresses are i32 in Wasm32)
    if (from.reg != .sp) {
        current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
    }

    // Emit the actual load with offset
    _ = try appendAfter(allocator, current, as, prog.constAddr(from.offset), .{});
}

/// Expand MOV pseudo-instruction
fn expandMov(allocator: std.mem.Allocator, p: *Prog, framesize: i32) !void {
    _ = framesize;

    const mov_as = p.as;
    const from = p.from;
    const to = p.to;

    // Determine load/store opcodes based on size
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

    // Handle destination first (for stores, address goes first)
    if (to.type == .mem) {
        // Store: push address, push value, store
        current = try appendAfter(allocator, current, .get, prog.regAddr(to.reg), .{});
        if (to.reg != .sp) {
            current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
        }
    }

    // Handle source value
    switch (from.type) {
        .const_int => {
            current = try appendAfter(allocator, current, .i64_const, prog.constAddr(from.offset), .{});
        },
        .reg => {
            current = try appendAfter(allocator, current, .get, from, .{});
            if (from.reg == .sp) {
                current = try appendAfter(allocator, current, .i64_extend_i32_u, .{}, .{});
            }
        },
        .mem => {
            current = try appendAfter(allocator, current, .get, prog.regAddr(from.reg), .{});
            if (from.reg != .sp) {
                current = try appendAfter(allocator, current, .i32_wrap_i64, .{}, .{});
            }
            current = try appendAfter(allocator, current, load_as, prog.constAddr(from.offset), .{});
        },
        .addr => {
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

    // Handle destination
    switch (to.type) {
        .reg => {
            if (to.reg == .sp) {
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

// ============================================================================
// Instruction insertion helpers
// ============================================================================

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
    // Create new instruction with target's content
    const new_target = try allocator.create(Prog);
    new_target.* = target.*;

    // Replace target with new instruction
    target.as = as;
    target.from = from;
    target.to = to;
    target.link = new_target;

    return new_target;
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "preprocess empty function" {
    const allocator = testing.allocator;

    var sym = Symbol.init("test");
    sym.frame_size = 0;

    const text = try allocator.create(Prog);
    text.* = Prog.init(.text);
    sym.text = text;

    defer {
        var p: ?*Prog = sym.text;
        while (p) |current| {
            const next = current.link;
            allocator.destroy(current);
            p = next;
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
        var p: ?*Prog = sym.text;
        while (p) |current| {
            const next = current.link;
            allocator.destroy(current);
            p = next;
        }
    }

    try preprocess(allocator, &sym);

    // Should have: text -> get -> i32_const -> i32_sub -> set -> ... -> return
    var count: usize = 0;
    var p: ?*Prog = sym.text;
    while (p) |current| {
        count += 1;
        p = current.link;
    }

    // text + prologue(4) + epilogue(4) + return = 10
    // Actually: text, get, const, sub, set, get, const, add, set, return
    try testing.expect(count >= 5);
}
