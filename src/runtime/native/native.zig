//! Cot Native Function Registry
//!
//! Unified dispatch for function calls - both native (Zig) and Cot (bytecode).
//! This is the central registry for all callable functions in Cot.
//!
//! Terminology:
//! - Native function: Implemented in Zig (the runtime)
//! - Bytecode function: Compiled Cot code
//! - The 'xcall' keyword is supported for backwards compatibility but optional

const std = @import("std");
const bytecode = @import("../bytecode/bytecode.zig");
const debug = @import("../debug.zig");
const extension = @import("../extension.zig");
const handles_mod = @import("../handles/handles.zig");

// Re-export submodules
pub const linker = @import("linker.zig");
pub const stdlib = @import("stdlib.zig");

// Function category modules
pub const math = @import("math.zig");
pub const datetime = @import("datetime.zig");
pub const conversion = @import("conversion.zig");
pub const string = @import("string.zig");
pub const system = @import("system.zig");
pub const buffer = @import("buffer.zig");
pub const io_console = @import("io_console.zig");
pub const io_core = @import("io_core.zig");
pub const io_isam = @import("io_isam.zig");
pub const json = @import("json.zig");
pub const http_client = @import("http_client.zig");
pub const http_server = @import("http_server.zig");
pub const io_fs = @import("io_fs.zig");
pub const io_crypto = @import("io_crypto.zig");
pub const io_sql = @import("io_sql.zig");
pub const io_terminal = @import("io_terminal.zig");
// Note: symtable has moved to extensions/dbl/symtable.zig

pub const Linker = linker.Linker;
pub const Stdlib = stdlib.Stdlib;

// Unified handle management
pub const UnifiedHandleManager = handles_mod.UnifiedHandleManager;
pub const Handle = handles_mod.Handle;
pub const HandleType = handles_mod.HandleType;

/// Value type from NaN-boxed implementation
pub const Value = @import("../bytecode/value.zig").Value;

/// Native function errors
pub const NativeError = error{
    InvalidArgument,
    OutOfMemory,
    FileError,
    NotImplemented,
    SubroutineNotFound,
    ReloadRequested,
    EndOfFile,
    RecordNotFound,
    AssertionFailed,
    ClosureCallFailed,
    NoVMContext,
};

/// Native function signature
pub const NativeFn = *const fn (*NativeContext) NativeError!?Value;

/// Callable types
pub const CallableType = enum {
    native,
    bytecode,
};

/// Callable definition
pub const CallableDef = struct {
    name: []const u8,
    sub_type: CallableType,
    native_fn: ?NativeFn,
    module_index: ?u16,
    routine_index: ?u16,
};

/// Runtime value type for storing globals
pub const GlobalValue = union(enum) {
    null_val: void,
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    fixed_string: []u8,
    boolean: bool,
};

/// Closure invoker function type
/// Called to execute a Cot closure from native code.
/// vm_ptr: Opaque pointer to the VM instance
/// closure: The closure Value to invoke
/// call_args: Arguments to pass to the closure
/// Returns: The closure's return value, or null on void return
pub const ClosureInvoker = *const fn (
    vm_ptr: *anyopaque,
    closure: Value,
    call_args: []const Value,
) NativeError!?Value;

/// Native function execution context
pub const NativeContext = struct {
    allocator: std.mem.Allocator,
    args: []const Value,
    handles: ?*UnifiedHandleManager = null, // Unified handle manager for all I/O
    vm: ?*anyopaque = null, // Opaque VM pointer for closure callbacks
    closure_invoker: ?ClosureInvoker = null, // Function to invoke closures

    pub fn getArg(self: *const NativeContext, index: usize) ?Value {
        if (index >= self.args.len) return null;
        return self.args[index];
    }

    pub fn getArgString(self: *const NativeContext, index: usize) ![]const u8 {
        const val = self.getArg(index) orelse return NativeError.InvalidArgument;
        return val.toString();
    }

    pub fn getArgInt(self: *const NativeContext, index: usize) !i64 {
        const val = self.getArg(index) orelse return NativeError.InvalidArgument;
        return val.toInt();
    }

    /// Check if a closure value was passed at the given argument index
    pub fn getArgClosure(self: *const NativeContext, index: usize) ?Value {
        const val = self.getArg(index) orelse return null;
        if (val.isClosure()) return val;
        return null;
    }

    /// Call a Cot closure from native code.
    /// This enables callback patterns where native functions invoke Cot handlers.
    ///
    /// Example:
    ///   const handler = ctx.getArgClosure(1) orelse return error.InvalidArgument;
    ///   const result = try ctx.callClosure(handler, &.{arg1, arg2});
    ///
    /// Returns: The closure's return value, or null if void
    /// Error: NoVMContext if VM reference not available, ClosureCallFailed on execution error
    pub fn callClosure(self: *const NativeContext, closure: Value, call_args: []const Value) NativeError!?Value {
        const vm_ptr = self.vm orelse return NativeError.NoVMContext;
        const invoker = self.closure_invoker orelse return NativeError.NoVMContext;
        return invoker(vm_ptr, closure, call_args);
    }

    /// Check if closure invocation is supported
    pub fn canCallClosures(self: *const NativeContext) bool {
        return self.vm != null and self.closure_invoker != null;
    }
};

/// Native function registry
pub const NativeRegistry = struct {
    allocator: std.mem.Allocator,
    subroutines: std.StringHashMap(CallableDef),
    loaded_modules: std.ArrayListAligned(*bytecode.Module, null),
    module_globals: std.ArrayListAligned([]GlobalValue, null),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var registry = Self{
            .allocator = allocator,
            .subroutines = std.StringHashMap(CallableDef).init(allocator),
            .loaded_modules = .empty,
            .module_globals = .empty,
        };

        registry.registerAllNatives() catch {};

        return registry;
    }

    pub fn deinit(self: *Self) void {
        self.subroutines.deinit();
        for (self.loaded_modules.items) |mod| {
            mod.deinit();
            self.allocator.destroy(mod);
        }
        self.loaded_modules.deinit(self.allocator);

        for (self.module_globals.items) |globals| {
            for (globals) |gval| {
                switch (gval) {
                    .fixed_string => |a| self.allocator.free(a),
                    else => {},
                }
            }
            self.allocator.free(globals);
        }
        self.module_globals.deinit(self.allocator);
    }

    /// Register all native functions from submodules
    fn registerAllNatives(self: *Self) !void {
        try math.register(self);
        try datetime.register(self);
        try conversion.register(self);
        try string.register(self);
        try system.register(self);
        try buffer.register(self);
        try io_console.register(self);
        try io_core.register(self);
        try io_isam.register(self);
        try json.register(self);
        try http_client.register(self);
        try http_server.register(self);
        try io_fs.register(self);
        try io_crypto.register(self);
        try io_sql.register(self);
        try io_terminal.register(self);
        // Note: DBL channel-based functions are registered by the DBL extension
    }

    /// Register a native function
    pub fn registerNative(self: *Self, name: []const u8, func: NativeFn) !void {
        try self.subroutines.put(name, .{
            .name = name,
            .sub_type = .native,
            .native_fn = func,
            .module_index = null,
            .routine_index = null,
        });
    }

    /// Load a bytecode module
    pub fn loadModule(self: *Self, path: []const u8) !void {
        const mod_file = std.fs.cwd().openFile(path, .{}) catch {
            return NativeError.FileError;
        };
        defer mod_file.close();

        const bytes = mod_file.readToEndAlloc(self.allocator, 1024 * 1024 * 100) catch {
            return NativeError.FileError;
        };
        defer self.allocator.free(bytes);

        const mod_ptr = try self.allocator.create(bytecode.Module);
        errdefer self.allocator.destroy(mod_ptr);

        var fbs = std.io.fixedBufferStream(bytes);

        mod_ptr.* = bytecode.Module.deserialize(self.allocator, fbs.reader()) catch {
            return NativeError.FileError;
        };

        mod_ptr.calculateGlobalsCount();
        debug.print(.general, "Module globals_count: {d}", .{mod_ptr.globals_count});

        const module_index: u16 = @intCast(self.loaded_modules.items.len);
        try self.loaded_modules.append(self.allocator, mod_ptr);

        if (mod_ptr.globals_count > 0) {
            const globals = try self.allocator.alloc(GlobalValue, mod_ptr.globals_count);
            for (globals) |*g| {
                g.* = .{ .null_val = {} };
            }
            try self.module_globals.append(self.allocator, globals);
            debug.print(.general, "Allocated {d} global slots for module {d}", .{ mod_ptr.globals_count, module_index });
        } else {
            try self.module_globals.append(self.allocator, &[_]GlobalValue{});
        }

        debug.print(.general, "Module has {d} exports, {d} routines", .{ mod_ptr.exports.len, mod_ptr.routines.len });

        if (mod_ptr.exports.len == 0 and mod_ptr.routines.len > 0) {
            debug.print(.general, "No exports, registering all routines", .{});
            for (mod_ptr.routines, 0..) |routine, i| {
                const name = switch (mod_ptr.getConstant(routine.name_index).?) {
                    .identifier => |n| n,
                    else => continue,
                };
                if (std.mem.eql(u8, name, "main")) continue;

                debug.print(.general, "  Registering routine: '{s}' (module={d}, routine={d})", .{ name, module_index, i });
                try self.subroutines.put(name, .{
                    .name = name,
                    .sub_type = .bytecode,
                    .native_fn = null,
                    .module_index = module_index,
                    .routine_index = @intCast(i),
                });
            }
        }

        for (mod_ptr.exports) |export_entry| {
            if (export_entry.kind == .routine) {
                const name = switch (mod_ptr.getConstant(export_entry.name_index).?) {
                    .identifier => |n| n,
                    else => continue,
                };

                debug.print(.general, "  Registering subroutine: '{s}' (module={d}, routine={d})", .{ name, module_index, export_entry.index });
                try self.subroutines.put(name, .{
                    .name = name,
                    .sub_type = .bytecode,
                    .native_fn = null,
                    .module_index = module_index,
                    .routine_index = export_entry.index,
                });
            }
        }
    }

    /// Load a bytecode module with a package prefix for qualified function calls
    /// Functions will be registered as both "prefix.name" and "name"
    pub fn loadModuleWithPrefix(self: *Self, path: []const u8, package_prefix: []const u8) !void {
        debug.print(.general, "loadModuleWithPrefix: path={s} prefix={s}", .{ path, package_prefix });
        const mod_file = std.fs.cwd().openFile(path, .{}) catch |err| {
            debug.print(.general, "loadModuleWithPrefix: Failed to open file: {}", .{err});
            return NativeError.FileError;
        };
        defer mod_file.close();

        const bytes = mod_file.readToEndAlloc(self.allocator, 1024 * 1024 * 100) catch |err| {
            debug.print(.general, "loadModuleWithPrefix: Failed to read: {}", .{err});
            return NativeError.FileError;
        };
        defer self.allocator.free(bytes);
        debug.print(.general, "loadModuleWithPrefix: Read {d} bytes", .{bytes.len});

        const mod_ptr = try self.allocator.create(bytecode.Module);
        errdefer self.allocator.destroy(mod_ptr);

        // Check if this is a bundle (CBUNDLE header) or raw module (CBO1 header)
        const is_bundle = bytes.len >= 8 and std.mem.eql(u8, bytes[0..8], "CBUNDLE\x00");
        debug.print(.general, "loadModuleWithPrefix: is_bundle={}", .{is_bundle});

        const module_bytes = if (is_bundle) blk: {
            // Parse bundle header: "CBUNDLE\0" (8) + version (1) + module_count (4) = 13 bytes
            if (bytes.len < 13) {
                debug.print(.general, "loadModuleWithPrefix: Bundle too small", .{});
                return NativeError.FileError;
            }
            const module_count = std.mem.readInt(u32, bytes[9..13], .little);
            if (module_count == 0) {
                debug.print(.general, "loadModuleWithPrefix: No modules in bundle", .{});
                return NativeError.FileError;
            }

            // Parse module table: each entry is offset (4) + size (4) + type (1) = 9 bytes
            // Find the first module (type 0 = main, type 1 = library, etc.)
            const table_start: usize = 13;
            var module_offset: ?u32 = null;
            var module_size: ?u32 = null;
            for (0..module_count) |i| {
                const entry_start = table_start + (i * 9);
                if (entry_start + 9 > bytes.len) break;
                const offset = std.mem.readInt(u32, bytes[entry_start..][0..4], .little);
                const size = std.mem.readInt(u32, bytes[entry_start + 4 ..][0..4], .little);
                // Take first module regardless of type for library packages
                module_offset = offset;
                module_size = size;
                break;
            }

            if (module_offset == null or module_size == null) {
                debug.print(.general, "loadModuleWithPrefix: No module found in bundle", .{});
                return NativeError.FileError;
            }

            const start = module_offset.?;
            const end = start + module_size.?;
            if (end > bytes.len) {
                debug.print(.general, "loadModuleWithPrefix: Module extends beyond bundle", .{});
                return NativeError.FileError;
            }
            debug.print(.general, "loadModuleWithPrefix: Extracted module at offset {d} size {d}", .{ start, module_size.? });
            break :blk bytes[start..end];
        } else bytes;

        var fbs = std.io.fixedBufferStream(module_bytes);

        mod_ptr.* = bytecode.Module.deserialize(self.allocator, fbs.reader()) catch |err| {
            debug.print(.general, "loadModuleWithPrefix: Failed to deserialize: {}", .{err});
            return NativeError.FileError;
        };

        mod_ptr.calculateGlobalsCount();
        debug.print(.general, "Module globals_count: {d}", .{mod_ptr.globals_count});

        const module_index: u16 = @intCast(self.loaded_modules.items.len);
        try self.loaded_modules.append(self.allocator, mod_ptr);

        if (mod_ptr.globals_count > 0) {
            const globals = try self.allocator.alloc(GlobalValue, mod_ptr.globals_count);
            for (globals) |*g| {
                g.* = .{ .null_val = {} };
            }
            try self.module_globals.append(self.allocator, globals);
            debug.print(.general, "Allocated {d} global slots for module {d}", .{ mod_ptr.globals_count, module_index });
        } else {
            try self.module_globals.append(self.allocator, &[_]GlobalValue{});
        }

        debug.print(.general, "Module has {d} exports, {d} routines, prefix='{s}'", .{ mod_ptr.exports.len, mod_ptr.routines.len, package_prefix });

        // Helper to register a function with both qualified and unqualified names
        const RegisterHelper = struct {
            fn register(
                subroutines: *std.StringHashMap(CallableDef),
                alloc: std.mem.Allocator,
                unqual_name: []const u8,
                prefix: []const u8,
                mod_idx: u16,
                routine_idx: u16,
            ) !void {
                // Register with unqualified name
                try subroutines.put(unqual_name, .{
                    .name = unqual_name,
                    .sub_type = .bytecode,
                    .native_fn = null,
                    .module_index = mod_idx,
                    .routine_index = routine_idx,
                });

                // Register with qualified name if prefix is provided
                if (prefix.len > 0) {
                    const qualified_name = std.fmt.allocPrint(alloc, "{s}.{s}", .{ prefix, unqual_name }) catch return;
                    debug.print(.general, "  Registering qualified: '{s}' (module={d}, routine={d})", .{ qualified_name, mod_idx, routine_idx });
                    try subroutines.put(qualified_name, .{
                        .name = qualified_name,
                        .sub_type = .bytecode,
                        .native_fn = null,
                        .module_index = mod_idx,
                        .routine_index = routine_idx,
                    });
                }
            }
        };

        if (mod_ptr.exports.len == 0 and mod_ptr.routines.len > 0) {
            debug.print(.general, "No exports, registering all routines", .{});
            for (mod_ptr.routines, 0..) |routine, i| {
                const name = switch (mod_ptr.getConstant(routine.name_index).?) {
                    .identifier => |n| n,
                    else => continue,
                };
                if (std.mem.eql(u8, name, "main")) continue;

                debug.print(.general, "  Registering routine: '{s}' (module={d}, routine={d})", .{ name, module_index, i });
                try RegisterHelper.register(&self.subroutines, self.allocator, name, package_prefix, module_index, @intCast(i));
            }
        }

        for (mod_ptr.exports) |export_entry| {
            if (export_entry.kind == .routine) {
                const name = switch (mod_ptr.getConstant(export_entry.name_index).?) {
                    .identifier => |n| n,
                    else => continue,
                };

                debug.print(.general, "  Registering subroutine: '{s}' (module={d}, routine={d})", .{ name, module_index, export_entry.index });
                try RegisterHelper.register(&self.subroutines, self.allocator, name, package_prefix, module_index, export_entry.index);
            }
        }
    }

    /// Look up a callable by name
    pub fn lookup(self: *Self, name: []const u8) ?CallableDef {
        var lower_buf: [256]u8 = undefined;
        const lower_name = std.ascii.lowerString(&lower_buf, name);
        const lookup_name = lower_name[0..@min(name.len, 256)];

        if (self.subroutines.get(lookup_name)) |def| {
            return def;
        }

        if (extension.getRegistry()) |reg| {
            if (reg.getNative(lookup_name)) |handler| {
                return CallableDef{
                    .name = lookup_name,
                    .sub_type = .native,
                    .native_fn = handler,
                    .module_index = null,
                    .routine_index = null,
                };
            }
        }

        return null;
    }

    /// Get a loaded module by index
    pub fn getLoadedModule(self: *Self, index: u16) ?*bytecode.Module {
        if (index >= self.loaded_modules.items.len) return null;
        return self.loaded_modules.items[index];
    }

    /// Get globals storage for a module
    pub fn getModuleGlobals(self: *Self, module_index: u16) ?[]GlobalValue {
        if (module_index >= self.module_globals.items.len) return null;
        return self.module_globals.items[module_index];
    }

    /// Call a function by name
    pub fn call(self: *Self, name: []const u8, ctx: *NativeContext) NativeError!?Value {
        if (self.lookup(name)) |def| {
            switch (def.sub_type) {
                .native => {
                    if (def.native_fn) |func| {
                        return func(ctx);
                    }
                    return NativeError.NotImplemented;
                },
                .bytecode => {
                    return NativeError.NotImplemented;
                },
            }
        }

        if (extension.getRegistry()) |reg| {
            var lower_buf: [256]u8 = undefined;
            const lower_name = std.ascii.lowerString(&lower_buf, name);
            if (reg.getNative(lower_name[0..@min(name.len, 256)])) |handler| {
                return handler(ctx);
            }
        }

        return NativeError.SubroutineNotFound;
    }
};

// Re-export parseKeySpec from io_isam module
pub const parseKeySpec = io_isam.parseKeySpec;

test "registry init" {
    const allocator = std.testing.allocator;
    var registry = NativeRegistry.init(allocator);
    defer registry.deinit();

    const isamc = registry.lookup("isamc");
    try std.testing.expect(isamc != null);
    try std.testing.expectEqual(CallableType.native, isamc.?.sub_type);
}
