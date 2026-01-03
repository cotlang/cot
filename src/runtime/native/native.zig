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
const cotdb = @import("cotdb");
const debug = @import("../debug.zig");
const extension = @import("../extension.zig");
const channels_mod = @import("../channels.zig");

// Re-export submodules
pub const linker = @import("linker.zig");
pub const stdlib = @import("stdlib.zig");
pub const tui_runtime = @import("tui_runtime.zig");

// Function category modules
pub const math = @import("math.zig");
pub const datetime = @import("datetime.zig");
pub const conversion = @import("conversion.zig");
pub const string = @import("string.zig");
pub const io = @import("io.zig");
pub const system = @import("system.zig");
pub const db = @import("db.zig");
pub const buffer = @import("buffer.zig");
// Note: symtable has moved to extensions/dbl/symtable.zig

pub const Linker = linker.Linker;
pub const Stdlib = stdlib.Stdlib;

// Unified channel management (text + ISAM)
pub const ChannelManager = channels_mod.ChannelManager;
pub const Channel = channels_mod.Channel;
pub const ChannelType = channels_mod.ChannelType;

// Legacy cursor management from CotDB (still used internally)
pub const CursorManager = cotdb.CursorManager;
pub const Cursor = cotdb.Cursor;

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
    TuiError,
    AssertionFailed,
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

/// Native function execution context
pub const NativeContext = struct {
    allocator: std.mem.Allocator,
    args: []const Value,
    cursors: ?*CursorManager,           // Legacy ISAM-only cursors
    channels: ?*ChannelManager = null,  // Unified text + ISAM channels

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
        try io.register(self);
        try system.register(self);
        try db.register(self);
        try buffer.register(self);
        // Note: NSPC_* functions are registered by the DBL extension
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
        const file = std.fs.cwd().openFile(path, .{}) catch {
            return NativeError.FileError;
        };
        defer file.close();

        const bytes = file.readToEndAlloc(self.allocator, 1024 * 1024 * 100) catch {
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

// Re-export parseKeySpec from db module for backwards compatibility
pub const parseKeySpec = db.parseKeySpec;

test "registry init" {
    const allocator = std.testing.allocator;
    var registry = NativeRegistry.init(allocator);
    defer registry.deinit();

    const isamc = registry.lookup("isamc");
    try std.testing.expect(isamc != null);
    try std.testing.expectEqual(CallableType.native, isamc.?.sub_type);
}
