// New register-based run function for VM
// This will replace the old stack-based run function

    /// Main execution loop - Register-Based Architecture
    fn run(self: *Self) VMError!void {
        // Mark VM as active for crash reporting
        crash_context.active = true;
        defer {
            crash_context.active = false;
        }

        // Main execution loop
        while (true) {
            const module = self.current_module orelse return VMError.InvalidRoutine;

            if (self.ip >= module.code.len) break;

            const byte = module.code[self.ip];
            const opcode: Opcode = std.meta.intToEnum(Opcode, byte) catch {
                return self.fail(VMError.InvalidOpcode, "Unknown opcode byte value");
            };

            // Update crash context
            crash_context.ip = @intCast(self.ip);
            crash_context.last_opcode = opcode;
            crash_context.source_line = self.debug_current_line;

            // Record for profiling
            if (profiling_enabled) {
                self.profiler.recordOpcode(opcode);
            }

            self.ip += 1;

            switch (opcode) {
                // ============================================
                // Core Operations (0x00-0x0F)
                // ============================================

                .nop => {},

                .halt => return,

                // ============================================
                // Register Moves & Immediates (0x10-0x1F)
                // ============================================

                .mov => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs: u4 = @truncate(ops & 0xF);
                    self.registers[rd] = self.registers[rs];
                },

                .movi => {
                    const ops = module.code[self.ip];
                    const imm: i8 = @bitCast(module.code[self.ip + 1]);
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.initInt(imm);
                },

                .movi16 => {
                    const ops = module.code[self.ip];
                    const imm: i16 = @bitCast(std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little));
                    self.ip += 3;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.initInt(imm);
                },

                .movi32 => {
                    const ops = module.code[self.ip];
                    const imm: i32 = @bitCast(std.mem.readInt(u32, module.code[self.ip + 1 ..][0..4], .little));
                    self.ip += 5;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.initInt(imm);
                },

                .load_const => {
                    const ops = module.code[self.ip];
                    const idx = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    const rd: u4 = @truncate(ops >> 4);
                    if (idx >= module.constants.len) {
                        return self.fail(VMError.InvalidConstant, "Constant index out of bounds");
                    }
                    self.registers[rd] = self.constantToValue(module.constants[idx]);
                },

                .load_null => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.null_val;
                },

                .load_true => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.true_val;
                },

                .load_false => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.false_val;
                },

                // ============================================
                // Local & Global Variables (0x20-0x2F)
                // ============================================

                .load_local => {
                    const ops = module.code[self.ip];
                    const slot = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = self.stack[self.fp + slot];
                },

                .store_local => {
                    const ops = module.code[self.ip];
                    const slot = module.code[self.ip + 1];
                    self.ip += 2;
                    const rs: u4 = @truncate(ops >> 4);
                    self.stack[self.fp + slot] = self.registers[rs];
                },

                .load_local16 => {
                    const ops = module.code[self.ip];
                    const slot = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = self.stack[self.fp + slot];
                },

                .store_local16 => {
                    const ops = module.code[self.ip];
                    const slot = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    const rs: u4 = @truncate(ops >> 4);
                    self.stack[self.fp + slot] = self.registers[rs];
                },

                .load_global => {
                    const ops = module.code[self.ip];
                    const idx = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    const rd: u4 = @truncate(ops >> 4);
                    if (idx >= self.globals.items.len) {
                        return self.fail(VMError.InvalidMemoryAccess, "Global index out of bounds");
                    }
                    self.registers[rd] = self.globals.items[idx];
                },

                .store_global => {
                    const ops = module.code[self.ip];
                    const idx = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    const rs: u4 = @truncate(ops >> 4);
                    if (idx >= self.globals.items.len) {
                        return self.fail(VMError.InvalidMemoryAccess, "Global index out of bounds");
                    }
                    self.globals.items[idx] = self.registers[rs];
                },

                // ============================================
                // Arithmetic (0x30-0x3F)
                // ============================================

                .add => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initInt(self.registers[rs1].toInt() + self.registers[rs2].toInt());
                },

                .sub => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initInt(self.registers[rs1].toInt() - self.registers[rs2].toInt());
                },

                .mul => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initInt(self.registers[rs1].toInt() * self.registers[rs2].toInt());
                },

                .div => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    const divisor = self.registers[rs2].toInt();
                    if (divisor == 0) {
                        return self.fail(VMError.DivisionByZero, "Division by zero");
                    }
                    self.registers[rd] = Value.initInt(@divTrunc(self.registers[rs1].toInt(), divisor));
                },

                .mod => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    const divisor = self.registers[rs2].toInt();
                    if (divisor == 0) {
                        return self.fail(VMError.DivisionByZero, "Modulo by zero");
                    }
                    self.registers[rd] = Value.initInt(@mod(self.registers[rs1].toInt(), divisor));
                },

                .neg => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs: u4 = @truncate(ops & 0xF);
                    self.registers[rd] = Value.initInt(-self.registers[rs].toInt());
                },

                .addi => {
                    const ops = module.code[self.ip];
                    const imm: i8 = @bitCast(module.code[self.ip + 1]);
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs: u4 = @truncate(ops & 0xF);
                    self.registers[rd] = Value.initInt(self.registers[rs].toInt() + imm);
                },

                .subi => {
                    const ops = module.code[self.ip];
                    const imm: i8 = @bitCast(module.code[self.ip + 1]);
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs: u4 = @truncate(ops & 0xF);
                    self.registers[rd] = Value.initInt(self.registers[rs].toInt() - imm);
                },

                .muli => {
                    const ops = module.code[self.ip];
                    const imm: i8 = @bitCast(module.code[self.ip + 1]);
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs: u4 = @truncate(ops & 0xF);
                    self.registers[rd] = Value.initInt(self.registers[rs].toInt() * imm);
                },

                .incr => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.initInt(self.registers[rd].toInt() + 1);
                },

                .decr => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    self.registers[rd] = Value.initInt(self.registers[rd].toInt() - 1);
                },

                // ============================================
                // Comparison (0x40-0x4F)
                // ============================================

                .cmp_eq => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toInt() == self.registers[rs2].toInt());
                },

                .cmp_ne => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toInt() != self.registers[rs2].toInt());
                },

                .cmp_lt => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toInt() < self.registers[rs2].toInt());
                },

                .cmp_le => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toInt() <= self.registers[rs2].toInt());
                },

                .cmp_gt => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toInt() > self.registers[rs2].toInt());
                },

                .cmp_ge => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toInt() >= self.registers[rs2].toInt());
                },

                // ============================================
                // Logical & Bitwise (0x50-0x5F)
                // ============================================

                .log_and => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toBool() and self.registers[rs2].toBool());
                },

                .log_or => {
                    const ops = module.code[self.ip];
                    const byte2 = module.code[self.ip + 1];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs1: u4 = @truncate(ops & 0xF);
                    const rs2: u4 = @truncate(byte2 >> 4);
                    self.registers[rd] = Value.initBool(self.registers[rs1].toBool() or self.registers[rs2].toBool());
                },

                .log_not => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rd: u4 = @truncate(ops >> 4);
                    const rs: u4 = @truncate(ops & 0xF);
                    self.registers[rd] = Value.initBool(!self.registers[rs].toBool());
                },

                // ============================================
                // Control Flow (0x60-0x6F)
                // ============================================

                .jmp => {
                    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little));
                    self.ip += 3;
                    const new_ip = @as(i64, @intCast(self.ip)) + offset;
                    if (new_ip < 0 or new_ip > module.code.len) {
                        return self.fail(VMError.BytecodeOutOfBounds, "Jump target out of bounds");
                    }
                    self.ip = @intCast(new_ip);
                },

                .jz => {
                    const ops = module.code[self.ip];
                    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little));
                    self.ip += 3;
                    const rs: u4 = @truncate(ops >> 4);
                    if (!self.registers[rs].toBool()) {
                        const new_ip = @as(i64, @intCast(self.ip)) + offset;
                        if (new_ip < 0 or new_ip > module.code.len) {
                            return self.fail(VMError.BytecodeOutOfBounds, "Jump target out of bounds");
                        }
                        self.ip = @intCast(new_ip);
                    }
                },

                .jnz => {
                    const ops = module.code[self.ip];
                    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little));
                    self.ip += 3;
                    const rs: u4 = @truncate(ops >> 4);
                    if (self.registers[rs].toBool()) {
                        const new_ip = @as(i64, @intCast(self.ip)) + offset;
                        if (new_ip < 0 or new_ip > module.code.len) {
                            return self.fail(VMError.BytecodeOutOfBounds, "Jump target out of bounds");
                        }
                        self.ip = @intCast(new_ip);
                    }
                },

                .loop_start, .loop_end, .clear_error_handler => {},

                // ============================================
                // Function Calls (0x70-0x7F)
                // ============================================

                .call => {
                    const ops = module.code[self.ip];
                    const routine_idx = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    const argc: u4 = @truncate(ops >> 4);

                    if (routine_idx >= module.routines.len) {
                        return self.fail(VMError.InvalidRoutine, "Routine index out of bounds");
                    }

                    // Push call frame
                    self.call_stack.append(.{
                        .module = module,
                        .routine_index = routine_idx,
                        .return_ip = self.ip,
                        .base_pointer = self.fp,
                        .stack_pointer = self.sp,
                        .caller_module_index = self.current_module_index,
                    }) catch return self.fail(VMError.StackOverflow, "Call stack overflow");

                    // Copy args from r0..r(argc-1) to stack (as locals for callee)
                    const routine = module.routines[routine_idx];
                    self.fp = self.sp;
                    for (0..argc) |i| {
                        self.stack[self.sp] = self.registers[i];
                        self.sp += 1;
                    }
                    // Reserve space for remaining locals
                    for (argc..routine.local_count) |_| {
                        self.stack[self.sp] = Value.null_val;
                        self.sp += 1;
                    }

                    // Jump to routine
                    self.ip = routine.code_offset;
                },

                .ret => {
                    self.ip += 2; // Skip operand bytes
                    if (self.call_stack.pop()) |frame| {
                        self.sp = frame.stack_pointer;
                        self.fp = frame.base_pointer;
                        self.ip = frame.return_ip;
                        self.current_module = frame.module;
                        self.current_module_index = frame.caller_module_index;
                    } else {
                        return; // Return from main - halt
                    }
                },

                .ret_val => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rs: u4 = @truncate(ops >> 4);
                    const return_value = self.registers[rs];

                    if (self.call_stack.pop()) |frame| {
                        self.sp = frame.stack_pointer;
                        self.fp = frame.base_pointer;
                        self.ip = frame.return_ip;
                        self.current_module = frame.module;
                        self.current_module_index = frame.caller_module_index;
                        // Store return value in r15 for caller
                        self.registers[15] = return_value;
                    } else {
                        return; // Return from main - halt
                    }
                },

                // ============================================
                // Debug (0xF0-0xFF)
                // ============================================

                .debug_break => {
                    self.stop_reason = .breakpoint;
                    return;
                },

                .debug_line => {
                    const line = std.mem.readInt(u16, module.code[self.ip + 1 ..][0..2], .little);
                    self.ip += 3;
                    self.debug_current_line = line;
                },

                // ============================================
                // Console I/O (0xD0-0xDF)
                // ============================================

                .console_writeln => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rs: u4 = @truncate(ops >> 4);
                    const val = self.registers[rs];
                    const str = val.toString(self.allocator) catch "(error)";
                    defer if (val.tag() != .string) self.allocator.free(str);
                    self.stdout.writeAll(str) catch {};
                    self.stdout.writeAll("\n") catch {};
                },

                .console_write => {
                    const ops = module.code[self.ip];
                    self.ip += 2;
                    const rs: u4 = @truncate(ops >> 4);
                    const val = self.registers[rs];
                    const str = val.toString(self.allocator) catch "(error)";
                    defer if (val.tag() != .string) self.allocator.free(str);
                    self.stdout.writeAll(str) catch {};
                },

                else => |unhandled| {
                    const msg = std.fmt.bufPrint(&error_message_buf, "Unimplemented opcode: {s}", .{@tagName(unhandled)}) catch "Unimplemented opcode";
                    return self.fail(VMError.InvalidOpcode, msg);
                },
            }
        }
    }
