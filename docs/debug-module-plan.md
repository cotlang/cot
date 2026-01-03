# Cot Debug Module Design

## Overview

A comprehensive debugging infrastructure for the cot compiler and runtime, designed to:
1. Speed up bug diagnosis from hours to minutes
2. Provide execution visibility at multiple granularities
3. Support both interactive and batch debugging
4. Enable programmatic analysis via structured output

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         CLI Commands Layer                               │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐    │
│  │ cot trace   │  │ cot debug   │  │ cot validate│  │ cot disasm  │    │
│  │ (batch)     │  │ (interactive)│  │ (bytecode) │  │ (existing)  │    │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └─────────────┘    │
└─────────┼────────────────┼────────────────┼─────────────────────────────┘
          │                │                │
          ▼                ▼                ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         src/debug/ Module                                │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────┐  │
│  │ trace.zig       │  │ inspector.zig   │  │ validator.zig           │  │
│  │ - TraceConfig   │  │ - StateInspector│  │ - BytecodeValidator     │  │
│  │ - Tracer        │  │ - Breakpoints   │  │ - InstructionWalker     │  │
│  │ - TraceEntry    │  │ - Watchpoints   │  │ - FormatChecker         │  │
│  └────────┬────────┘  └────────┬────────┘  └────────────┬────────────┘  │
│           │                    │                        │               │
│           ▼                    ▼                        ▼               │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                      history.zig                                 │   │
│  │  RingBuffer for execution history, crash dumps, state snapshots  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                    │                                    │
│  ┌─────────────────────────────────▼───────────────────────────────┐   │
│  │                      output.zig                                  │   │
│  │  OutputFormat: human | json | compact                            │   │
│  │  OutputTarget: stderr | stdout | file | buffer                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    VM Integration (src/runtime/bytecode/)                │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────┐  │
│  │ vm.zig          │  │ vm_opcodes.zig  │  │ instruction_format.zig  │  │
│  │ - tracer: ?*T   │  │ - trace hooks   │  │ - format registry       │  │
│  │ - onError hook  │  │ - validation    │  │ - size assertions       │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

## Module Structure

```
src/debug/
├── debug.zig           # Module root, public API
├── trace.zig           # Execution tracing infrastructure
├── history.zig         # Ring buffer for execution history
├── inspector.zig       # State inspection (registers, stack, memory)
├── breakpoint.zig      # Breakpoint and watchpoint management
├── validator.zig       # Bytecode validation and consistency checks
├── output.zig          # Output formatting (human, JSON, compact)
└── commands/
    ├── trace_cmd.zig   # `cot trace` implementation
    ├── debug_cmd.zig   # `cot debug` implementation (interactive)
    └── validate_cmd.zig # `cot validate` implementation
```

## Core Types

### trace.zig

```zig
//! Execution tracing infrastructure
//!
//! Provides configurable tracing of VM execution with multiple
//! granularity levels and output formats.

const std = @import("std");
const Opcode = @import("../runtime/bytecode/opcodes.zig").Opcode;
const Value = @import("../runtime/bytecode/value.zig").Value;
const History = @import("history.zig").History;
const Output = @import("output.zig").Output;

/// Trace granularity levels
pub const TraceLevel = enum(u8) {
    /// No tracing (production default)
    none = 0,

    /// Routine entry/exit only - minimal overhead
    /// Output: [enter] main | [exit] main -> void
    routines = 1,

    /// Each opcode with IP and current line
    /// Output: [main:10] 0x0004 load_const r0, #3
    opcodes = 2,

    /// Opcodes + register state after each instruction
    /// Output: [main:10] 0x0004 load_const r0, #3 | r0=42 r1=nil sp=2
    verbose = 3,

    /// Full state dump including stack and locals
    /// Output: (multi-line detailed state)
    full = 4,
};

/// What triggered a trace event
pub const TraceEvent = enum {
    opcode_pre,      // Before opcode execution
    opcode_post,     // After opcode execution
    call,            // Routine call
    return_,         // Routine return
    error_,          // Error occurred
    breakpoint,      // Hit breakpoint
    watchpoint,      // Watchpoint triggered
    native_call,     // Native function call
    native_return,   // Native function return
};

/// A single trace entry (stored in history ring buffer)
pub const TraceEntry = struct {
    timestamp: i64,           // Monotonic nanoseconds
    event: TraceEvent,
    ip: u32,
    opcode: ?Opcode,
    line: u32,
    routine: ?[]const u8,     // Routine name (pointer into module constants)

    // Optional extended data based on event type
    data: union {
        none: void,
        registers: [4]Value,  // r0-r3 snapshot
        call_info: struct {
            arg_count: u8,
            target: []const u8,
        },
        return_info: struct {
            value: ?Value,
        },
        error_info: struct {
            err: anyerror,
            message: []const u8,
        },
    },

    pub fn format(self: TraceEntry, writer: anytype, fmt_type: Output.Format) !void {
        // Format based on output type
    }
};

/// Trace filter configuration
pub const TraceFilter = struct {
    /// Only trace these routines (null = all)
    routines: ?[]const []const u8 = null,

    /// Only trace these opcodes (null = all)
    opcodes: ?[]const Opcode = null,

    /// Only trace this IP range (null = all)
    ip_range: ?struct { start: u32, end: u32 } = null,

    /// Only trace these source lines (null = all)
    line_range: ?struct { start: u32, end: u32 } = null,

    /// Skip these routines
    exclude_routines: []const []const u8 = &.{},

    /// Skip native function calls
    exclude_natives: bool = false,

    pub fn matches(self: TraceFilter, entry: TraceEntry) bool {
        // Check if entry passes all filters
    }
};

/// Main tracer configuration
pub const TraceConfig = struct {
    level: TraceLevel = .none,
    filter: TraceFilter = .{},
    output: Output.Config = .{},

    /// Keep history for crash dumps (even if level is .none)
    history_size: u16 = 64,

    /// Pause execution at these IPs
    breakpoints: []const u32 = &.{},

    /// Call this on breakpoint hit (for interactive debugging)
    break_handler: ?*const fn (*Tracer, *VM) BreakAction = null,

    pub const BreakAction = enum {
        continue_,
        step,
        step_over,
        step_out,
        abort,
    };
};

/// Main tracer - attached to VM during execution
pub const Tracer = struct {
    allocator: std.mem.Allocator,
    config: TraceConfig,
    output: Output,
    history: History(TraceEntry),

    // Statistics
    stats: struct {
        opcodes_executed: u64 = 0,
        calls_made: u64 = 0,
        native_calls: u64 = 0,
        start_time: i64 = 0,
        end_time: i64 = 0,
    } = .{},

    // Current state for context
    current_routine: ?[]const u8 = null,
    current_line: u32 = 0,
    call_depth: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, config: TraceConfig) Tracer {
        return .{
            .allocator = allocator,
            .config = config,
            .output = Output.init(allocator, config.output),
            .history = History(TraceEntry).init(allocator, config.history_size),
        };
    }

    pub fn deinit(self: *Tracer) void {
        self.history.deinit();
        self.output.deinit();
    }

    // ===== Event Hooks (called by VM) =====

    pub fn onOpcodeStart(self: *Tracer, vm: *VM, opcode: Opcode) void {
        self.stats.opcodes_executed += 1;

        const entry = TraceEntry{
            .timestamp = std.time.nanoTimestamp(),
            .event = .opcode_pre,
            .ip = @intCast(vm.ip - 1),
            .opcode = opcode,
            .line = vm.debug_current_line,
            .routine = self.current_routine,
            .data = .{ .none = {} },
        };

        self.history.push(entry);

        if (self.config.level >= .opcodes and self.config.filter.matches(entry)) {
            self.output.writeEntry(entry);
        }

        // Check breakpoints
        if (self.isBreakpoint(entry.ip)) {
            self.handleBreakpoint(vm);
        }
    }

    pub fn onOpcodeEnd(self: *Tracer, vm: *VM, opcode: Opcode) void {
        if (self.config.level >= .verbose) {
            const entry = TraceEntry{
                .timestamp = std.time.nanoTimestamp(),
                .event = .opcode_post,
                .ip = @intCast(vm.ip),
                .opcode = opcode,
                .line = vm.debug_current_line,
                .routine = self.current_routine,
                .data = .{ .registers = vm.registers[0..4].* },
            };

            if (self.config.filter.matches(entry)) {
                self.output.writeEntry(entry);
            }
        }
    }

    pub fn onCall(self: *Tracer, target: []const u8, arg_count: u8) void {
        self.call_depth += 1;
        self.stats.calls_made += 1;
        // ... record entry
    }

    pub fn onReturn(self: *Tracer, value: ?Value) void {
        self.call_depth -= 1;
        // ... record entry
    }

    pub fn onError(self: *Tracer, vm: *VM, err: anyerror, message: []const u8) void {
        // Always record errors in history
        const entry = TraceEntry{
            .timestamp = std.time.nanoTimestamp(),
            .event = .error_,
            .ip = @intCast(vm.ip),
            .opcode = null,
            .line = vm.debug_current_line,
            .routine = self.current_routine,
            .data = .{ .error_info = .{ .err = err, .message = message } },
        };
        self.history.push(entry);
    }

    // ===== Crash Dump =====

    pub fn dumpHistory(self: *Tracer, writer: anytype) !void {
        try writer.writeAll("\n=== Execution History (last ");
        try writer.print("{d}", .{self.history.len()});
        try writer.writeAll(" events) ===\n\n");

        var iter = self.history.iterator();
        while (iter.next()) |entry| {
            try entry.format(writer, self.config.output.format);
            try writer.writeByte('\n');
        }

        try writer.writeAll("\n=== Statistics ===\n");
        try writer.print("Opcodes executed: {d}\n", .{self.stats.opcodes_executed});
        try writer.print("Calls made: {d}\n", .{self.stats.calls_made});
        // ... more stats
    }

    // ===== Breakpoint Handling =====

    fn isBreakpoint(self: *Tracer, ip: u32) bool {
        for (self.config.breakpoints) |bp| {
            if (bp == ip) return true;
        }
        return false;
    }

    fn handleBreakpoint(self: *Tracer, vm: *VM) void {
        if (self.config.break_handler) |handler| {
            const action = handler(self, vm);
            // Handle action...
        }
    }
};
```

### history.zig

```zig
//! Ring buffer for execution history
//!
//! Fixed-size circular buffer that efficiently stores the last N events.
//! Used for crash dumps and post-mortem debugging.

pub fn History(comptime T: type) type {
    return struct {
        const Self = @This();

        buffer: []T,
        head: usize = 0,      // Next write position
        count: usize = 0,     // Number of valid entries
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator, capacity: u16) Self {
            return .{
                .buffer = allocator.alloc(T, capacity) catch &[_]T{},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.buffer.len > 0) {
                self.allocator.free(self.buffer);
            }
        }

        pub fn push(self: *Self, item: T) void {
            if (self.buffer.len == 0) return;

            self.buffer[self.head] = item;
            self.head = (self.head + 1) % self.buffer.len;
            if (self.count < self.buffer.len) {
                self.count += 1;
            }
        }

        pub fn len(self: *const Self) usize {
            return self.count;
        }

        pub fn iterator(self: *const Self) Iterator {
            return .{
                .history = self,
                .pos = 0,
            };
        }

        pub const Iterator = struct {
            history: *const Self,
            pos: usize,

            pub fn next(self: *Iterator) ?T {
                if (self.pos >= self.history.count) return null;

                // Calculate actual index (oldest to newest)
                const start = if (self.history.count == self.history.buffer.len)
                    self.history.head
                else
                    0;
                const idx = (start + self.pos) % self.history.buffer.len;
                self.pos += 1;
                return self.history.buffer[idx];
            }
        };

        /// Get last N entries (most recent first)
        pub fn lastN(self: *const Self, n: usize) []const T {
            // Return slice of last n entries
        }

        /// Clear all history
        pub fn clear(self: *Self) void {
            self.head = 0;
            self.count = 0;
        }
    };
}
```

### output.zig

```zig
//! Output formatting for debug/trace output
//!
//! Supports multiple output formats and destinations.

pub const Format = enum {
    /// Human-readable, colored terminal output
    /// [main:10] 0x0004 load_const r0, #3
    human,

    /// Machine-readable JSON (one object per line)
    /// {"ip":"0x0004","op":"load_const","line":10,...}
    json,

    /// Compact single-line format for log files
    /// 0004:load_const:r0:#3
    compact,
};

pub const Target = enum {
    stderr,
    stdout,
    file,
    buffer,
};

pub const Config = struct {
    format: Format = .human,
    target: Target = .stderr,
    file_path: ?[]const u8 = null,

    /// Colorize output (human format only)
    color: bool = true,

    /// Include timestamps
    timestamps: bool = false,

    /// Indent based on call depth
    indent_calls: bool = true,
};

pub const Output = struct {
    config: Config,
    writer: Writer,
    allocator: std.mem.Allocator,

    const Writer = union(Target) {
        stderr: std.fs.File,
        stdout: std.fs.File,
        file: std.fs.File,
        buffer: *std.ArrayList(u8),
    };

    pub fn init(allocator: std.mem.Allocator, config: Config) Output {
        // Initialize based on target
    }

    pub fn deinit(self: *Output) void {
        // Close file if needed
    }

    pub fn writeEntry(self: *Output, entry: TraceEntry) void {
        switch (self.config.format) {
            .human => self.writeHuman(entry),
            .json => self.writeJson(entry),
            .compact => self.writeCompact(entry),
        }
    }

    fn writeHuman(self: *Output, entry: TraceEntry) void {
        // Format: [routine:line] IP opcode operands | registers
        // With colors for different parts
    }

    fn writeJson(self: *Output, entry: TraceEntry) void {
        // One JSON object per line
    }

    fn writeCompact(self: *Output, entry: TraceEntry) void {
        // IP:opcode:operands
    }
};
```

### validator.zig

```zig
//! Bytecode validation and consistency checking
//!
//! Validates bytecode files for:
//! - Structural integrity (header, sections, offsets)
//! - Instruction validity (opcodes, operands, sizes)
//! - Reference validity (constant indices, routine indices)
//! - Control flow validity (jumps, calls, returns)

pub const ValidationError = struct {
    offset: u32,
    kind: Kind,
    message: []const u8,

    pub const Kind = enum {
        invalid_header,
        invalid_section,
        invalid_opcode,
        invalid_operand,
        size_mismatch,
        out_of_bounds_reference,
        unreachable_code,
        missing_return,
        stack_imbalance,
    };
};

pub const ValidationResult = struct {
    errors: []ValidationError,
    warnings: []ValidationError,

    // Statistics
    instruction_count: u32,
    routine_count: u32,
    constant_count: u32,
    type_count: u32,
    code_coverage: f32,  // Percentage of code that's reachable

    pub fn isValid(self: ValidationResult) bool {
        return self.errors.len == 0;
    }
};

pub const Validator = struct {
    allocator: std.mem.Allocator,
    module: *const Module,
    errors: std.ArrayList(ValidationError),
    warnings: std.ArrayList(ValidationError),

    pub fn init(allocator: std.mem.Allocator, module: *const Module) Validator {
        return .{
            .allocator = allocator,
            .module = module,
            .errors = std.ArrayList(ValidationError).init(allocator),
            .warnings = std.ArrayList(ValidationError).init(allocator),
        };
    }

    pub fn validate(self: *Validator) ValidationResult {
        self.validateHeader();
        self.validateConstants();
        self.validateTypes();
        self.validateRoutines();
        self.validateCode();
        self.validateControlFlow();

        return .{
            .errors = self.errors.toOwnedSlice(),
            .warnings = self.warnings.toOwnedSlice(),
            // ... stats
        };
    }

    fn validateHeader(self: *Validator) void {
        // Check magic, version, entry point
    }

    fn validateCode(self: *Validator) void {
        // Walk through all instructions
        // Verify each opcode is valid
        // Verify operands are in range
        // Verify instruction sizes match format registry
    }

    fn validateControlFlow(self: *Validator) void {
        // Check jump targets are valid instruction boundaries
        // Check all paths eventually return or halt
        // Check stack balance
    }
};

/// Walk through bytecode instruction by instruction
pub const InstructionWalker = struct {
    code: []const u8,
    pos: usize = 0,

    pub const Instruction = struct {
        offset: u32,
        opcode: Opcode,
        size: u8,
        operands: []const u8,
    };

    pub fn next(self: *InstructionWalker) ?Instruction {
        if (self.pos >= self.code.len) return null;

        const offset = self.pos;
        const opcode: Opcode = @enumFromInt(self.code[self.pos]);
        const size = instruction_format.instructionSize(opcode);

        if (self.pos + size > self.code.len) {
            // Instruction extends past end of code
            return null;
        }

        const inst = Instruction{
            .offset = @intCast(offset),
            .opcode = opcode,
            .size = size,
            .operands = self.code[self.pos + 1 .. self.pos + size],
        };

        self.pos += size;
        return inst;
    }
};
```

### inspector.zig

```zig
//! Runtime state inspection
//!
//! Provides read-only access to VM state for debugging.

pub const StateInspector = struct {
    vm: *const VM,
    module: *const Module,

    // ===== Register Access =====

    pub fn getRegister(self: StateInspector, reg: u4) Value {
        return self.vm.registers[reg];
    }

    pub fn getAllRegisters(self: StateInspector) [16]Value {
        return self.vm.registers;
    }

    pub fn formatRegister(self: StateInspector, reg: u4) []const u8 {
        // Return human-readable register value
    }

    // ===== Stack Access =====

    pub fn getStackDepth(self: StateInspector) usize {
        return self.vm.sp;
    }

    pub fn getStackValue(self: StateInspector, offset: usize) ?Value {
        if (offset >= self.vm.sp) return null;
        return self.vm.stack[offset];
    }

    pub fn getStackSlice(self: StateInspector, start: usize, count: usize) []const Value {
        const end = @min(start + count, self.vm.sp);
        return self.vm.stack[start..end];
    }

    // ===== Local Variables =====

    pub fn getLocal(self: StateInspector, slot: u16) ?Value {
        // Get local variable by slot
    }

    pub fn getLocalByName(self: StateInspector, name: []const u8) ?Value {
        // Look up local by name (requires debug info)
    }

    pub fn getAllLocals(self: StateInspector) []const LocalInfo {
        // Return all locals with names and values
    }

    pub const LocalInfo = struct {
        name: []const u8,
        slot: u16,
        value: Value,
        type_name: []const u8,
    };

    // ===== Call Stack =====

    pub fn getCallStack(self: StateInspector) []const FrameInfo {
        // Return call stack frames
    }

    pub const FrameInfo = struct {
        routine_name: []const u8,
        return_ip: u32,
        source_line: u32,
        local_count: u16,
    };

    // ===== Code Context =====

    pub fn getCurrentInstruction(self: StateInspector) ?InstructionInfo {
        // Return info about instruction at current IP
    }

    pub fn getSourceContext(self: StateInspector, lines_before: u32, lines_after: u32) ?SourceContext {
        // Return source code context around current line
    }

    pub const InstructionInfo = struct {
        ip: u32,
        opcode: Opcode,
        operands: []const u8,
        source_line: u32,
        disassembly: []const u8,
    };

    pub const SourceContext = struct {
        current_line: u32,
        lines: []const SourceLine,
    };

    pub const SourceLine = struct {
        number: u32,
        text: []const u8,
        is_current: bool,
    };
};
```

### breakpoint.zig

```zig
//! Breakpoint and watchpoint management

pub const BreakpointKind = enum {
    /// Break at specific IP
    address,

    /// Break at source line
    line,

    /// Break when entering routine
    routine_entry,

    /// Break when exiting routine
    routine_exit,

    /// Break on specific opcode
    opcode,

    /// Break when condition is true
    conditional,
};

pub const Breakpoint = struct {
    id: u32,
    kind: BreakpointKind,
    enabled: bool = true,
    hit_count: u32 = 0,

    // Kind-specific data
    data: union(BreakpointKind) {
        address: u32,
        line: struct { file: []const u8, line: u32 },
        routine_entry: []const u8,
        routine_exit: []const u8,
        opcode: Opcode,
        conditional: struct {
            address: u32,
            condition: []const u8,  // Expression to evaluate
        },
    },

    /// Condition that must be true (in addition to location match)
    condition: ?[]const u8 = null,

    /// Action when hit
    action: Action = .break_,

    pub const Action = enum {
        break_,      // Stop execution
        log,         // Log and continue
        trace_on,    // Enable tracing
        trace_off,   // Disable tracing
    };
};

pub const Watchpoint = struct {
    id: u32,
    kind: Kind,
    enabled: bool = true,

    pub const Kind = enum {
        /// Watch memory location
        memory,

        /// Watch register
        register,

        /// Watch local variable
        local,

        /// Watch expression value
        expression,
    };

    // ... data for each kind
};

pub const BreakpointManager = struct {
    breakpoints: std.ArrayList(Breakpoint),
    watchpoints: std.ArrayList(Watchpoint),
    next_id: u32 = 1,

    pub fn addBreakpoint(self: *BreakpointManager, bp: Breakpoint) u32 {
        // Add and return ID
    }

    pub fn removeBreakpoint(self: *BreakpointManager, id: u32) bool {
        // Remove by ID
    }

    pub fn checkBreakpoints(self: *BreakpointManager, ip: u32, vm: *VM) ?*Breakpoint {
        // Check if any breakpoint matches current state
    }

    pub fn checkWatchpoints(self: *BreakpointManager, vm: *VM) ?*Watchpoint {
        // Check if any watchpoint triggered
    }
};
```

## CLI Commands

### cot trace

```
Usage: cot trace [options] <file.cbo>

Trace execution of a bytecode file.

Options:
  -l, --level <level>    Trace level: routines, opcodes, verbose, full
                         (default: opcodes)
  -f, --format <format>  Output format: human, json, compact
                         (default: human)
  -o, --output <file>    Write trace to file instead of stderr
  --no-color             Disable colored output
  --timestamps           Include timestamps in output

Filtering:
  --routine <name>       Only trace this routine (can repeat)
  --exclude <name>       Exclude this routine (can repeat)
  --ip-range <start:end> Only trace IP range (hex)
  --lines <start:end>    Only trace source line range

Breakpoints:
  --break <ip>           Break at IP (hex, can repeat)
  --break-line <line>    Break at source line
  --break-routine <name> Break on routine entry

Examples:
  cot trace program.cbo
  cot trace -l verbose program.cbo
  cot trace --format=json -o trace.log program.cbo
  cot trace --routine=main --break=0x0100 program.cbo
```

### cot validate

```
Usage: cot validate [options] <file.cbo>

Validate bytecode file integrity.

Options:
  --strict               Treat warnings as errors
  --format <format>      Output format: human, json

Output:
  Validation report showing:
  - Header validity
  - Section integrity
  - Instruction validity
  - Control flow analysis
  - Reference validity

Examples:
  cot validate program.cbo
  cot validate --strict program.cbo
  cot validate --format=json program.cbo
```

### cot debug (future)

```
Usage: cot debug [options] <file.cbo>

Interactive debugger for bytecode files.

Commands:
  run, r           Run until breakpoint or end
  step, s          Step one instruction
  next, n          Step over calls
  finish, f        Run until current routine returns
  continue, c      Continue execution

  break <loc>      Set breakpoint (IP, line, or routine)
  delete <id>      Delete breakpoint
  list             List breakpoints

  print <expr>     Print value
  registers        Show all registers
  stack            Show stack
  locals           Show local variables
  backtrace, bt    Show call stack

  disasm [range]   Disassemble code
  memory <addr>    Show memory

  quit, q          Exit debugger
```

## VM Integration

### Changes to vm.zig

```zig
pub const VM = struct {
    // ... existing fields ...

    /// Optional tracer for debugging
    tracer: ?*Tracer = null,

    /// Attach a tracer
    pub fn setTracer(self: *Self, tracer: *Tracer) void {
        self.tracer = tracer;
        tracer.stats.start_time = std.time.nanoTimestamp();
    }

    /// Detach tracer and return it
    pub fn detachTracer(self: *Self) ?*Tracer {
        const t = self.tracer;
        self.tracer = null;
        if (t) |tracer| {
            tracer.stats.end_time = std.time.nanoTimestamp();
        }
        return t;
    }
};
```

### Changes to dispatch loop

```zig
fn runDispatchTable(self: *Self) VMError!void {
    const module = self.current_module.?;

    while (true) {
        if (self.ip >= module.code.len) break;

        const opcode_byte = module.code[self.ip];
        const opcode: Opcode = @enumFromInt(opcode_byte);

        // Trace hook (before)
        if (self.tracer) |t| {
            t.onOpcodeStart(self, opcode);
        }

        // Update crash context
        crash_context.ip = @intCast(self.ip);
        crash_context.last_opcode = opcode;
        crash_context.source_line = self.debug_current_line;

        self.ip += 1;

        const result = dispatch_table[opcode_byte](self, module) catch |err| {
            // Error hook
            if (self.tracer) |t| {
                t.onError(self, err, @errorName(err));
            }
            return err;
        };

        // Trace hook (after)
        if (self.tracer) |t| {
            t.onOpcodeEnd(self, opcode);
        }

        switch (result) {
            .continue_dispatch => continue,
            .halt => return,
            .return_from_main => return,
        }
    }
}
```

## Implementation Phases

### Phase 1: Foundation (Core Infrastructure)
- [ ] Create `src/debug/` directory structure
- [ ] Implement `history.zig` - ring buffer
- [ ] Implement `output.zig` - output formatting
- [ ] Implement basic `trace.zig` - TraceConfig, TraceEntry, TraceLevel
- [ ] Add tracer field to VM
- [ ] Add trace hooks to dispatch loop

### Phase 2: Basic Tracing
- [ ] Implement `Tracer.onOpcodeStart`
- [ ] Implement `Tracer.onOpcodeEnd`
- [ ] Implement human output format
- [ ] Implement `cot trace` command (basic)
- [ ] Add `--level` option
- [ ] Test with simple programs

### Phase 3: Enhanced Output
- [ ] Implement JSON output format
- [ ] Implement compact output format
- [ ] Add `--format` option
- [ ] Add `--output` file option
- [ ] Add `--no-color` option
- [ ] Implement call/return tracing
- [ ] Add call depth indentation

### Phase 4: Filtering
- [ ] Implement TraceFilter
- [ ] Add `--routine` filter
- [ ] Add `--exclude` filter
- [ ] Add `--ip-range` filter
- [ ] Add `--lines` filter

### Phase 5: Crash Dumps
- [ ] Implement `Tracer.dumpHistory`
- [ ] Integrate with VM error handling
- [ ] Auto-dump on InvalidOpcode
- [ ] Auto-dump on other fatal errors
- [ ] Include register/stack snapshots in history

### Phase 6: Validation
- [ ] Implement `validator.zig`
- [ ] Implement `InstructionWalker`
- [ ] Implement header validation
- [ ] Implement instruction validation
- [ ] Implement control flow validation
- [ ] Implement `cot validate` command

### Phase 7: Breakpoints
- [ ] Implement `breakpoint.zig`
- [ ] Add breakpoint support to Tracer
- [ ] Add `--break` option to `cot trace`
- [ ] Implement break handler callback
- [ ] Basic interactive mode (step/continue)

### Phase 8: State Inspection
- [ ] Implement `inspector.zig`
- [ ] Register inspection
- [ ] Stack inspection
- [ ] Local variable inspection
- [ ] Call stack inspection

### Phase 9: Interactive Debugger
- [ ] Implement `cot debug` command
- [ ] REPL interface
- [ ] Command parsing
- [ ] Integrate inspector
- [ ] Integrate breakpoints

### Phase 10: Polish
- [ ] Deprecate `COT_DEBUG` env var (or integrate)
- [ ] Documentation
- [ ] Tests for debug module
- [ ] Performance optimization (minimal overhead when tracing disabled)

## Success Criteria

After implementation, debugging the original bug (VM types issue) should:

1. **Take < 2 minutes** instead of 20+ minutes
2. Require only: `cot trace /tmp/test.cbo 2>&1 | head -20`
3. Immediately show IP jumping incorrectly after `debug_line`
4. Provide enough context to identify the emitter/VM mismatch

Example expected output that would have solved the bug:
```
[main:0] 0x0000 debug_line line=10
[main:10] 0x0004 load_const r0, #3   << IP jumped 4, expected 4 ✓
[main:10] 0x0008 store_local $0=0
[main:10] 0x000B debug_line line=15  << Wait, 0x0008 + 3 = 0x000B ✓
...

OR with the bug:

[main:0] 0x0000 debug_line line=10
[main:10] 0x0005 ??? (0x00)          << IP jumped 5, instruction at 0x0005 is garbage!
INVALID OPCODE at 0x0005
```

The trace immediately reveals the debug_line instruction advanced IP by 5 instead of 4.
