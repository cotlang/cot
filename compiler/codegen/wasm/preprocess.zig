//! Preprocess - Transform high-level instructions to low-level Wasm
//!
//! Go reference: cmd/internal/obj/wasm/wasmobj.go preprocess()
//!
//! This pass performs critical transformations:
//!
//! 1. AJMP transformation (Go: lines 394-404)
//!    - Block-to-block jumps become: i32.const <pc>, set PC_B, br
//!
//! 2. Dispatch loop structure (Go: lines 690-724)
//!    - Wraps function body in: loop { block { ... } block { ... } br_table ... end }
//!
//! 3. Branch depth resolution (Go: lines 726-747)
//!    - Computes relative depths for all br/br_if instructions
//!
//! After this pass, all instructions are directly encodable Wasm.

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const Prog = prog.Prog;
const Addr = prog.Addr;
const Symbol = prog.Symbol;

// Debug logging
const debug = @import("../../pipeline_debug.zig");

/// Preprocess a function's instruction chain.
///
/// Go reference: wasmobj.go lines 156-748
pub fn preprocess(allocator: std.mem.Allocator, sym: *Symbol) !void {
    debug.log(.codegen, "preprocess: '{s}' framesize={d}", .{ sym.name, sym.frame_size });

    const framesize = sym.frame_size;
    if (framesize < 0) {
        return error.BadFrameSize;
    }

    const text = sym.text orelse return;

    // ========================================================================
    // Pass 1: Count resume points and assign PC values
    // Go: wasmobj.go lines 265-319
    // ========================================================================

    var numResumePoints: i64 = 0;
    var pc: i64 = 0;
    var tableIdxs = std.ArrayListUnmanaged(u64){};
    defer tableIdxs.deinit(allocator);
    var tablePC: i64 = 0;

    var p: ?*Prog = text;
    while (p) |current| : (p = current.link) {
        switch (current.as) {
            .resume_point => {
                // Go: lines 284-294
                // Convert to AEnd, assign PC
                current.as = .end;
                while (tablePC <= pc) {
                    try tableIdxs.append(allocator, @intCast(numResumePoints));
                    tablePC += 1;
                }
                numResumePoints += 1;
                pc += 1;
            },
            .call => {
                // Go: lines 296-300
                // CALL at top level gets implicit resume point after
                // We'll handle this in phase 2
                pc += 1;
            },
            .nop, .text => {
                // Go: lines 309-310
                pc += 1;
            },
            else => {},
        }
        current.pc = pc;
    }

    // Final table entry
    try tableIdxs.append(allocator, @intCast(numResumePoints));

    debug.log(.codegen, "  numResumePoints={d}, pc={d}", .{ numResumePoints, pc });

    // ========================================================================
    // Pass 2: Prologue - Allocate stack frame
    // Go: wasmobj.go lines 207-214
    // ========================================================================

    if (framesize > 0) {
        var prologue = text;

        // Get SP
        prologue = try appendAfter(allocator, prologue, .get, prog.regAddr(.sp), .{});

        // i32.const framesize
        prologue = try appendAfter(allocator, prologue, .i32_const, prog.constAddr(framesize), .{});

        // i32.sub
        prologue = try appendAfter(allocator, prologue, .i32_sub, .{}, .{});

        // Set SP
        prologue = try appendAfter(allocator, prologue, .set, .{}, prog.regAddr(.sp));
        prologue.spadj = framesize;
    }

    // ========================================================================
    // Pass 3: Transform instructions
    // Go: wasmobj.go lines 380-687
    // ========================================================================

    // Record branches to the entry point loop (for later depth resolution)
    var entryPointLoopBranches = std.ArrayListUnmanaged(*Prog){};
    defer entryPointLoopBranches.deinit(allocator);

    // Track control flow depth for checking
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
            // ----------------------------------------------------------------
            // AJMP transformation
            // Go: wasmobj.go lines 394-404
            // ----------------------------------------------------------------
            .jmp => {
                // Save the jump target info before replacing
                const target_prog = current.to.branch_target;

                // Replace JMP with NOP
                current.as = .nop;

                if (target_prog) |target| {
                    // Jump to basic block:
                    // i32.const <target_pc>  (PC_B is i32 in Go!)
                    // set PC_B
                    // br <to entryPointLoop>

                    var emit = current;

                    // i32.const <target block's pc>
                    // Go line 400: p = appendp(p, AI32Const, constAddr(jmp.To.Val.(*obj.Prog).Pc))
                    emit = try appendAfter(allocator, emit, .i32_const, prog.constAddr(target.pc), .{});

                    // set PC_B
                    // Go line 401: p = appendp(p, ASet, regAddr(REG_PC_B))
                    emit = try appendAfter(allocator, emit, .set, .{}, prog.regAddr(.pc_b));

                    // br (depth to be filled in later)
                    // Go line 402: p = appendp(p, ABr)
                    emit = try appendAfter(allocator, emit, .br, .{}, .{});
                    emit.to.type = .branch; // Mark for later resolution
                    try entryPointLoopBranches.append(allocator, emit);
                }
            },

            // ----------------------------------------------------------------
            // ARET transformation (pseudo-return)
            // Go: wasmobj.go lines 504-544
            // ----------------------------------------------------------------
            .aret => {
                // Replace ARET with NOP, then append epilogue
                current.as = .nop;
                var emit = current;

                if (framesize > 0) {
                    // SP += framesize (restore stack frame)
                    // Go lines 509-513
                    emit = try appendAfter(allocator, emit, .get, prog.regAddr(.sp), .{});
                    emit = try appendAfter(allocator, emit, .i32_const, prog.constAddr(framesize), .{});
                    emit = try appendAfter(allocator, emit, .i32_add, .{}, .{});
                    emit = try appendAfter(allocator, emit, .set, .{}, prog.regAddr(.sp));
                }

                // Emit actual wasm return
                // Note: return value (if any) is already on the wasm stack from gen.zig
                _ = try appendAfter(allocator, emit, .@"return", .{}, .{});
            },

            // ----------------------------------------------------------------
            // Real RET (keep as-is, used for wasm return directly)
            // ----------------------------------------------------------------
            .@"return" => {
                // Keep the return instruction as-is
            },

            // ----------------------------------------------------------------
            // Get with address mode
            // Go: wasmobj.go lines 565-585
            // ----------------------------------------------------------------
            .get => {
                if (current.from.type == .addr) {
                    try expandGetAddr(allocator, current, framesize);
                }
            },

            // ----------------------------------------------------------------
            // Load with memory operand
            // Go: wasmobj.go lines 587-600
            // ----------------------------------------------------------------
            .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u => {
                if (current.from.type == .mem) {
                    try expandLoadMem(allocator, current);
                }
            },

            // ----------------------------------------------------------------
            // MOV pseudo-instructions
            // Go: wasmobj.go lines 602-687
            // ----------------------------------------------------------------
            .mov_b, .mov_h, .mov_w, .mov_d => {
                try expandMov(allocator, current, framesize);
            },

            else => {},
        }

        p = next;
    }

    // ========================================================================
    // Pass 4: Adjust offsets for auto/param variables
    // Go: wasmobj.go lines 547-562
    // ========================================================================

    p = text;
    while (p) |current| : (p = current.link) {
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
    }

    // ========================================================================
    // Pass 5: Create dispatch loop structure
    // Go: wasmobj.go lines 690-724
    // ========================================================================

    var entryPointLoop: ?*Prog = null;

    if (entryPointLoopBranches.items.len > 0 or numResumePoints > 0) {
        // Find end of text marker to insert after
        var insert_point = text;

        // Insert loop for entryPointLoop
        // Go: lines 698-702
        insert_point = try appendAfter(allocator, insert_point, .loop, .{}, .{});
        entryPointLoop = insert_point;

        // Point all entry loop branches to the loop instruction
        for (entryPointLoopBranches.items) |b| {
            b.to.branch_target = entryPointLoop;
        }

        if (numResumePoints > 0) {
            // Add Block instructions for resume points (N+1 blocks)
            // Go: lines 704-711
            var i: i64 = 0;
            while (i < numResumePoints + 1) : (i += 1) {
                insert_point = try appendAfter(allocator, insert_point, .block, .{}, .{});
            }

            // Get PC_B (i32, used directly by br_table)
            // Go line 709: p = appendp(p, AGet, regAddr(REG_PC_B))
            insert_point = try appendAfter(allocator, insert_point, .get, prog.regAddr(.pc_b), .{});

            // br_table with indices
            insert_point = try appendAfter(allocator, insert_point, .br_table, .{}, .{});
            // Store table indices in the instruction
            const table_copy = try allocator.alloc(u64, tableIdxs.items.len);
            @memcpy(table_copy, tableIdxs.items);
            insert_point.to.val = .{ .br_table = table_copy };

            // End of first block (innermost)
            insert_point = try appendAfter(allocator, insert_point, .end, .{}, .{});
        }

        // Find end of function body to add loop terminator
        var last = text;
        while (last.link) |n| {
            last = n;
        }

        // End of entryPointLoop
        // Go: lines 716-718
        last = try appendAfter(allocator, last, .end, .{}, .{});

        // Unreachable (should never reach here)
        // Go: line 719
        _ = try appendAfter(allocator, last, .@"unreachable", .{}, .{});
    }

    // ========================================================================
    // Pass 6: Compute relative branch depths
    // Go: wasmobj.go lines 726-747
    // ========================================================================

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
                // Go: lines 737-746
                if (current.to.type == .branch) {
                    if (current.to.branch_target) |target| {
                        if (blockDepths.get(target)) |blockDepth| {
                            // Convert branch target to relative depth
                            const relative_depth = currentDepth - blockDepth;
                            current.to = prog.constAddr(relative_depth);
                        }
                    }
                }
            },
            else => {},
        }
    }

    debug.log(.codegen, "  preprocess complete, branches resolved", .{});
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

    // Should have prologue and epilogue instructions
    var count: usize = 0;
    var p: ?*Prog = sym.text;
    while (p) |current| {
        count += 1;
        p = current.link;
    }

    // Verify we have more instructions than we started with
    try testing.expect(count >= 5);
}

test "preprocess with resume points creates dispatch loop" {
    const allocator = testing.allocator;

    var sym = Symbol.init("test");
    sym.frame_size = 0;

    // Create: text -> resume_point -> return
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
        var p: ?*Prog = sym.text;
        while (p) |current| {
            const next = current.link;
            // Free br_table data if present
            if (current.as == .br_table) {
                if (current.to.val.br_table.len > 0) {
                    allocator.free(current.to.val.br_table);
                }
            }
            allocator.destroy(current);
            p = next;
        }
    }

    try preprocess(allocator, &sym);

    // Should have loop, blocks, br_table in the output
    var has_loop = false;
    var has_block = false;
    var has_br_table = false;

    var p: ?*Prog = sym.text;
    while (p) |current| {
        switch (current.as) {
            .loop => has_loop = true,
            .block => has_block = true,
            .br_table => has_br_table = true,
            else => {},
        }
        p = current.link;
    }

    try testing.expect(has_loop);
    try testing.expect(has_block);
    try testing.expect(has_br_table);
}
