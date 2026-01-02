//! Cot Bytecode Module Format
//!
//! Defines the structure of compiled bytecode modules.
//!
//! File extensions:
//!   .cbo - compiled object file
//!   .clb - executable library
//!   .cbr - mainline application

const std = @import("std");
const Opcode = @import("opcodes.zig").Opcode;

/// Magic numbers for file identification
pub const MAGIC_MODULE = [4]u8{ 'C', 'B', 'O', '1' }; // .cbo object file
pub const MAGIC_LIBRARY = [4]u8{ 'C', 'L', 'B', '1' }; // .clb library
pub const MAGIC_EXECUTABLE = [4]u8{ 'C', 'B', 'R', '1' }; // .cbr executable

/// Current bytecode version
pub const VERSION_MAJOR: u16 = 0;
pub const VERSION_MINOR: u16 = 1;

/// Module flags
pub const ModuleFlags = packed struct(u32) {
    has_debug_info: bool = false,
    has_native_extensions: bool = false,
    requires_isam: bool = false,
    is_library: bool = false,
    _reserved: u28 = 0,
};

/// Section types
pub const SectionType = enum(u32) {
    constants = 0x0001,
    types = 0x0002,
    routines = 0x0003,
    code = 0x0004,
    exports = 0x0005,
    imports = 0x0006,
    debug = 0x0007,
    native = 0x0008,
};

/// Module header (32 bytes)
pub const ModuleHeader = extern struct {
    magic: [4]u8,
    version_major: u16,
    version_minor: u16,
    flags: ModuleFlags,
    section_count: u32,
    entry_point: u32, // 0xFFFFFFFF if library
    source_hash: u32,
    _reserved: [8]u8,

    pub fn init() ModuleHeader {
        return .{
            .magic = MAGIC_MODULE,
            .version_major = VERSION_MAJOR,
            .version_minor = VERSION_MINOR,
            .flags = .{},
            .section_count = 0,
            .entry_point = 0xFFFFFFFF,
            .source_hash = 0,
            ._reserved = [_]u8{0} ** 8,
        };
    }

    pub fn isValid(self: *const ModuleHeader) bool {
        return std.mem.eql(u8, &self.magic, &MAGIC_MODULE);
    }
};

/// Section table entry
pub const SectionEntry = extern struct {
    section_type: SectionType,
    offset: u32,
    size: u32,
    entry_count: u32,
};

/// Constant pool entry tags
pub const ConstantTag = enum(u8) {
    integer = 0x01,
    decimal = 0x02, // i64 + u8 precision
    string = 0x03,
    fixed_string = 0x04, // fixed-size, space-padded
    identifier = 0x05,
    record_ref = 0x06,
    routine_ref = 0x07,
};

/// Constant pool value
pub const Constant = union(ConstantTag) {
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    string: []const u8,
    fixed_string: struct { data: []const u8, size: u16 },
    identifier: []const u8,
    record_ref: u16,
    routine_ref: u16,

    /// Serialize constant to writer
    pub fn serialize(self: Constant, writer: anytype) !void {
        // Write tag
        try writer.writeByte(@intFromEnum(std.meta.activeTag(self)));

        switch (self) {
            .integer => |val| try writer.writeInt(i64, val, .little),
            .decimal => |d| {
                try writer.writeInt(i64, d.value, .little);
                try writer.writeByte(d.precision);
            },
            .string => |s| {
                try writer.writeInt(u32, @intCast(s.len), .little);
                try writer.writeAll(s);
            },
            .fixed_string => |a| {
                try writer.writeInt(u16, a.size, .little);
                try writer.writeAll(a.data[0..a.size]);
            },
            .identifier => |id| {
                try writer.writeInt(u32, @intCast(id.len), .little);
                try writer.writeAll(id);
            },
            .record_ref => |r| try writer.writeInt(u16, r, .little),
            .routine_ref => |r| try writer.writeInt(u16, r, .little),
        }
    }

    /// Deserialize constant from reader
    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !Constant {
        const tag = try reader.readByte();
        const tag_enum: ConstantTag = @enumFromInt(tag);

        return switch (tag_enum) {
            .integer => .{ .integer = try reader.readInt(i64, .little) },
            .decimal => .{
                .decimal = .{
                    .value = try reader.readInt(i64, .little),
                    .precision = try reader.readByte(),
                },
            },
            .string => blk: {
                const len = try reader.readInt(u32, .little);
                const str = try allocator.alloc(u8, len);
                try reader.readNoEof(str);
                break :blk .{ .string = str };
            },
            .fixed_string => blk: {
                const size = try reader.readInt(u16, .little);
                const data = try allocator.alloc(u8, size);
                try reader.readNoEof(data);
                break :blk .{ .fixed_string = .{ .data = data, .size = size } };
            },
            .identifier => blk: {
                const len = try reader.readInt(u32, .little);
                const id = try allocator.alloc(u8, len);
                try reader.readNoEof(id);
                break :blk .{ .identifier = id };
            },
            .record_ref => .{ .record_ref = try reader.readInt(u16, .little) },
            .routine_ref => .{ .routine_ref = try reader.readInt(u16, .little) },
        };
    }
};

/// Data type codes
pub const DataTypeCode = enum(u8) {
    string = 0x01,
    decimal = 0x02,
    fixed_point = 0x03,
    int8 = 0x04,
    int16 = 0x05,
    int32 = 0x06,
    int64 = 0x07,
    structure = 0x08,
    handle = 0x09,
    group = 0x0A,
};

/// Field definition
pub const FieldDef = struct {
    name_index: u16, // Index into constant pool
    data_type: DataTypeCode,
    flags: u8,
    offset: u16,
    size: u16,
    precision: u8,
    array_dims: []u16,
};

/// Record/type definition
pub const TypeDef = struct {
    type_id: u16,
    kind: TypeKind,
    flags: u8,
    name_index: u16,
    total_size: u32,
    fields: []FieldDef,

    pub const TypeKind = enum(u8) {
        record = 0,
        group = 1,
        alias = 2,
    };
};

/// Routine flags
pub const RoutineFlags = packed struct(u16) {
    is_public: bool = false,
    is_function: bool = false, // Has return value
    is_void: bool = false, // No return value (void)
    has_varargs: bool = false,
    is_entry_point: bool = false,
    _reserved: u11 = 0,
};

/// Parameter pass mode: by value or by reference
pub const ParamMode = enum(u8) {
    val = 0,  // by value (default for Cot, DBL class methods)
    ref = 1,  // by reference (default for DBL subroutines/functions)
};

/// Parameter definition
pub const ParamDef = struct {
    name_index: u16,
    data_type: DataTypeCode,
    mode: ParamMode,
    default_value: ?u16, // Constant pool index
};

/// Local variable definition (debug info)
pub const LocalDef = struct {
    name_index: u16, // Index into string table
    slot: u16, // Stack slot index
    data_type: DataTypeCode,
};

/// Routine definition
pub const RoutineDef = struct {
    name_index: u16,
    flags: RoutineFlags,
    code_offset: u32,
    code_length: u32,
    param_count: u16,
    local_count: u16,
    max_stack: u16,
    params: []ParamDef,
    locals: []LocalDef, // Debug info: local variable names
};

/// Export entry
pub const ExportEntry = struct {
    name_index: u16,
    kind: ExportKind,
    flags: u8,
    index: u16,

    pub const ExportKind = enum(u8) {
        routine = 0,
        record = 1,
        global = 2,
    };
};

/// Import entry
pub const ImportEntry = struct {
    name_index: u16,
    module_index: u16, // 0 = any module
    kind: ImportKind,
    flags: ImportFlags,

    pub const ImportKind = enum(u8) {
        routine = 0,
        record = 1,
        global = 2,
    };

    pub const ImportFlags = packed struct(u8) {
        required: bool = true,
        _reserved: u7 = 0,
    };
};

/// Debug line entry
pub const LineEntry = struct {
    code_offset: u32,
    line: u16,
    column: u16,
};

/// Debug local variable entry
pub const LocalVarEntry = struct {
    name_index: u16,
    slot: u16,
    scope_start: u32,
    scope_end: u32,
    data_type: DataTypeCode,
};

/// Complete module structure
pub const Module = struct {
    allocator: std.mem.Allocator,
    header: ModuleHeader,
    constants: []Constant,
    types: []TypeDef,
    routines: []RoutineDef,
    code: []u8,
    exports: []ExportEntry,
    imports: []ImportEntry,

    // Debug info (optional)
    source_file: ?[]const u8,
    line_table: ?[]LineEntry,
    local_vars: ?[]LocalVarEntry,

    // Globals metadata - calculated from types at load time
    // The first `globals_count` local slots in each routine are globals
    globals_count: u32,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .header = ModuleHeader.init(),
            .constants = &[_]Constant{},
            .types = &[_]TypeDef{},
            .routines = &[_]RoutineDef{},
            .code = &[_]u8{},
            .exports = &[_]ExportEntry{},
            .imports = &[_]ImportEntry{},
            .source_file = null,
            .line_table = null,
            .local_vars = null,
            .globals_count = 0,
        };
    }

    /// Calculate the number of global variable slots based on types
    /// This is the total number of fields across all global record types
    pub fn calculateGlobalsCount(self: *Self) void {
        var count: u32 = 0;
        for (self.types) |ty| {
            count += @intCast(ty.fields.len);
        }
        self.globals_count = count;
    }

    pub fn deinit(self: *Self) void {
        // Free allocated slices
        if (self.constants.len > 0) {
            for (self.constants) |c| {
                switch (c) {
                    .string => |s| self.allocator.free(s),
                    .fixed_string => |a| self.allocator.free(a.data),
                    .identifier => |s| self.allocator.free(s),
                    else => {},
                }
            }
            self.allocator.free(self.constants);
        }
        if (self.types.len > 0) {
            for (self.types) |ty| {
                if (ty.fields.len > 0) self.allocator.free(ty.fields);
            }
            self.allocator.free(self.types);
        }
        if (self.routines.len > 0) {
            // Free locals and params slices inside each routine
            for (self.routines) |routine| {
                if (routine.locals.len > 0) self.allocator.free(routine.locals);
                if (routine.params.len > 0) self.allocator.free(routine.params);
            }
            self.allocator.free(self.routines);
        }
        if (self.code.len > 0) self.allocator.free(self.code);
        if (self.exports.len > 0) self.allocator.free(self.exports);
        if (self.imports.len > 0) self.allocator.free(self.imports);
        if (self.line_table) |lt| self.allocator.free(lt);
        if (self.local_vars) |lv| self.allocator.free(lv);
        if (self.source_file) |sf| self.allocator.free(sf);
    }

    /// Serialize module to bytes
    pub fn serialize(self: *const Self, writer: anytype) !void {
        // Write header
        try writer.writeAll(std.mem.asBytes(&self.header));

        // Write constants section
        try writer.writeInt(u32, @intCast(self.constants.len), .little);
        for (self.constants) |constant| {
            try constant.serialize(writer);
        }

        // Write types section
        try writer.writeInt(u32, @intCast(self.types.len), .little);
        for (self.types) |type_def| {
            try writer.writeInt(u16, type_def.type_id, .little);
            try writer.writeByte(@intFromEnum(type_def.kind));
            try writer.writeByte(type_def.flags);
            try writer.writeInt(u16, type_def.name_index, .little);
            try writer.writeInt(u32, type_def.total_size, .little);
            // Write fields
            try writer.writeInt(u16, @intCast(type_def.fields.len), .little);
            for (type_def.fields) |field| {
                try writer.writeInt(u16, field.name_index, .little);
                try writer.writeByte(@intFromEnum(field.data_type));
                try writer.writeByte(field.flags);
                try writer.writeInt(u16, field.offset, .little);
                try writer.writeInt(u16, field.size, .little);
                try writer.writeByte(field.precision);
                // Skip array_dims for now (could add later)
            }
        }

        // Write routines section
        try writer.writeInt(u32, @intCast(self.routines.len), .little);
        for (self.routines) |routine| {
            try writer.writeInt(u16, routine.name_index, .little);
            try writer.writeAll(std.mem.asBytes(&routine.flags));
            try writer.writeInt(u32, routine.code_offset, .little);
            try writer.writeInt(u32, routine.code_length, .little);
            try writer.writeInt(u16, routine.param_count, .little);
            try writer.writeInt(u16, routine.local_count, .little);
            try writer.writeInt(u16, routine.max_stack, .little);

            // Write params (including mode for ref parameter handling)
            try writer.writeInt(u16, @intCast(routine.params.len), .little);
            for (routine.params) |param| {
                try writer.writeInt(u16, param.name_index, .little);
                try writer.writeByte(@intFromEnum(param.data_type));
                try writer.writeByte(@intFromEnum(param.mode));
                try writer.writeInt(u16, param.default_value orelse 0xFFFF, .little);
            }

            // Write locals debug info
            try writer.writeInt(u16, @intCast(routine.locals.len), .little);
            for (routine.locals) |local| {
                try writer.writeInt(u16, local.name_index, .little);
                try writer.writeInt(u16, local.slot, .little);
                try writer.writeByte(@intFromEnum(local.data_type));
            }
        }

        // Write code section
        try writer.writeInt(u32, @intCast(self.code.len), .little);
        try writer.writeAll(self.code);

        // Write exports section
        try writer.writeInt(u32, @intCast(self.exports.len), .little);
        for (self.exports) |exp| {
            try writer.writeInt(u16, exp.name_index, .little);
            try writer.writeByte(@intFromEnum(exp.kind));
            try writer.writeByte(exp.flags);
            try writer.writeInt(u16, exp.index, .little);
        }
    }

    /// Deserialize module from bytes
    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !Self {
        var module = Self.init(allocator);

        // Read header
        const header_bytes = try reader.readBytesNoEof(@sizeOf(ModuleHeader));
        module.header = std.mem.bytesToValue(ModuleHeader, &header_bytes);

        if (!module.header.isValid()) {
            return error.InvalidModule;
        }

        // Read constants section
        const const_count = try reader.readInt(u32, .little);
        if (const_count > 0) {
            module.constants = try allocator.alloc(Constant, const_count);
            for (0..const_count) |i| {
                module.constants[i] = try Constant.deserialize(allocator, reader);
            }
        }

        // Read types section
        const type_count = try reader.readInt(u32, .little);
        if (type_count > 0) {
            module.types = try allocator.alloc(TypeDef, type_count);
            for (0..type_count) |i| {
                module.types[i].type_id = try reader.readInt(u16, .little);
                module.types[i].kind = @enumFromInt(try reader.readByte());
                module.types[i].flags = try reader.readByte();
                module.types[i].name_index = try reader.readInt(u16, .little);
                module.types[i].total_size = try reader.readInt(u32, .little);

                // Read fields
                const field_count = try reader.readInt(u16, .little);
                if (field_count > 0) {
                    module.types[i].fields = try allocator.alloc(FieldDef, field_count);
                    for (0..field_count) |j| {
                        module.types[i].fields[j].name_index = try reader.readInt(u16, .little);
                        module.types[i].fields[j].data_type = @enumFromInt(try reader.readByte());
                        module.types[i].fields[j].flags = try reader.readByte();
                        module.types[i].fields[j].offset = try reader.readInt(u16, .little);
                        module.types[i].fields[j].size = try reader.readInt(u16, .little);
                        module.types[i].fields[j].precision = try reader.readByte();
                        module.types[i].fields[j].array_dims = &[_]u16{};
                    }
                } else {
                    module.types[i].fields = &[_]FieldDef{};
                }
            }
        }

        // Read routines section
        const routine_count = try reader.readInt(u32, .little);
        if (routine_count > 0) {
            module.routines = try allocator.alloc(RoutineDef, routine_count);
            for (0..routine_count) |i| {
                module.routines[i].name_index = try reader.readInt(u16, .little);
                const flags_bytes = try reader.readBytesNoEof(@sizeOf(RoutineFlags));
                module.routines[i].flags = std.mem.bytesToValue(RoutineFlags, &flags_bytes);
                module.routines[i].code_offset = try reader.readInt(u32, .little);
                module.routines[i].code_length = try reader.readInt(u32, .little);
                module.routines[i].param_count = try reader.readInt(u16, .little);
                module.routines[i].local_count = try reader.readInt(u16, .little);
                module.routines[i].max_stack = try reader.readInt(u16, .little);

                // Read params (including mode for ref parameter handling)
                const params_count = try reader.readInt(u16, .little);
                if (params_count > 0) {
                    module.routines[i].params = try allocator.alloc(ParamDef, params_count);
                    for (0..params_count) |j| {
                        module.routines[i].params[j].name_index = try reader.readInt(u16, .little);
                        module.routines[i].params[j].data_type = @enumFromInt(try reader.readByte());
                        module.routines[i].params[j].mode = @enumFromInt(try reader.readByte());
                        const default_val = try reader.readInt(u16, .little);
                        module.routines[i].params[j].default_value = if (default_val == 0xFFFF) null else default_val;
                    }
                } else {
                    module.routines[i].params = &[_]ParamDef{};
                }

                // Read locals debug info
                const locals_count = try reader.readInt(u16, .little);
                if (locals_count > 0) {
                    module.routines[i].locals = try allocator.alloc(LocalDef, locals_count);
                    for (0..locals_count) |j| {
                        module.routines[i].locals[j].name_index = try reader.readInt(u16, .little);
                        module.routines[i].locals[j].slot = try reader.readInt(u16, .little);
                        module.routines[i].locals[j].data_type = @enumFromInt(try reader.readByte());
                    }
                } else {
                    module.routines[i].locals = &[_]LocalDef{};
                }
            }
        }

        // Read code section
        const code_len = try reader.readInt(u32, .little);
        if (code_len > 0) {
            module.code = try allocator.alloc(u8, code_len);
            try reader.readNoEof(module.code);
        }

        // Read exports section
        const export_count = try reader.readInt(u32, .little);
        if (export_count > 0) {
            module.exports = try allocator.alloc(ExportEntry, export_count);
            for (0..export_count) |i| {
                module.exports[i].name_index = try reader.readInt(u16, .little);
                module.exports[i].kind = @enumFromInt(try reader.readByte());
                module.exports[i].flags = try reader.readByte();
                module.exports[i].index = try reader.readInt(u16, .little);
            }
        }

        return module;
    }

    /// Get a constant by index
    pub fn getConstant(self: *const Self, index: u16) ?Constant {
        if (index >= self.constants.len) return null;
        return self.constants[index];
    }

    /// Get a type definition by index
    pub fn getType(self: *const Self, index: u16) ?*const TypeDef {
        if (index >= self.types.len) return null;
        return &self.types[index];
    }

    /// Get a routine definition by index
    pub fn getRoutine(self: *const Self, index: u16) ?*const RoutineDef {
        if (index >= self.routines.len) return null;
        return &self.routines[index];
    }

    /// Find routine by name
    pub fn findRoutine(self: *const Self, name: []const u8) ?u16 {
        for (self.routines, 0..) |routine, i| {
            if (self.getConstant(routine.name_index)) |c| {
                switch (c) {
                    .identifier => |n| {
                        if (std.mem.eql(u8, n, name)) {
                            return @intCast(i);
                        }
                    },
                    else => {},
                }
            }
        }
        return null;
    }
};

/// Library bundle (collection of modules)
pub const Library = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    modules: []Module,
    master_exports: []MasterExport,

    pub const MasterExport = struct {
        name_index: u16,
        module_index: u16,
        local_index: u16,
    };

    pub fn init(allocator: std.mem.Allocator, name: []const u8) Library {
        return .{
            .allocator = allocator,
            .name = name,
            .modules = &[_]Module{},
            .master_exports = &[_]MasterExport{},
        };
    }

    pub fn deinit(self: *Library) void {
        for (self.modules) |*m| {
            m.deinit();
        }
        if (self.modules.len > 0) self.allocator.free(self.modules);
        if (self.master_exports.len > 0) self.allocator.free(self.master_exports);
    }
};

test "module header" {
    const header = ModuleHeader.init();
    try std.testing.expect(header.isValid());
    try std.testing.expectEqual(VERSION_MAJOR, header.version_major);
    try std.testing.expectEqual(VERSION_MINOR, header.version_minor);
}
