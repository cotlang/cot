//! Cot Runtime - Bytecode VM and execution environment (native only, WASM removed)

const std = @import("std");
const build_options = @import("build_options");

// Profiling configuration
pub const profiling_enabled = if (@hasDecl(build_options, "enable_profiling")) build_options.enable_profiling else false;

// Core bytecode execution
pub const bytecode = struct {
    pub const vm = @import("bytecode/vm.zig");
    pub const module = @import("bytecode/module.zig");
    pub const opcodes = @import("bytecode/opcodes.zig");
    pub const disasm = @import("bytecode/disasm.zig");
    pub const profiler = @import("bytecode/profiler.zig");

    pub const VM = vm.VM;
    pub const Module = module.Module;
    pub const Opcode = opcodes.Opcode;
    pub const Disassembler = disasm.Disassembler;
    pub const StopReason = vm.StopReason;
    pub const Profiler = profiler.Profiler;

    // Crash context for improved error reporting
    pub const CrashContext = vm.CrashContext;
    pub const crash_context = &vm.crash_context;
    pub const enterNativeContext = vm.enterNativeContext;
    pub const exitNativeContext = vm.exitNativeContext;
};

// Native functions
pub const native = @import("native/native.zig");

// TUI
pub const tui = @import("tui/tui.zig");

// ISAM backends - re-export from cotdb
pub const sqlite_isam = @import("cotdb").sqlite_isam;

// Unified handle management
pub const handles = @import("handles/handles.zig");
pub const UnifiedHandleManager = handles.UnifiedHandleManager;

// Debug utilities
pub const debug = @import("debug.zig");

// Execution tracing
pub const trace = @import("trace/trace.zig");

// Extension system
pub const extension = @import("extension.zig");
pub const extension_manager = @import("extension_manager.zig");
pub const type_registry = @import("type_registry.zig");
pub const opcode_registry = @import("opcode_registry.zig");

// DBL extension (Map type, NSPC_* functions)
pub const dbl_ext = @import("extensions/dbl/dbl.zig");

// Debugger support (breakpoints, stepping, inspection)
pub const debugger = @import("debugger.zig");
pub const Debugger = debugger.Debugger;

// Crash handling
pub const crash = @import("crash.zig");
