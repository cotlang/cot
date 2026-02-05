//! ObjectModule: Bridge between compiled code and object file writers.
//!
//! This module provides the interface for generating object files (Mach-O, ELF)
//! from compiled machine code. It is a port of Cranelift's ObjectModule from
//! `cranelift/object/src/backend.rs`.
//!
//! The ObjectModule:
//!   1. Collects function declarations (names, linkage)
//!   2. Accepts compiled code (MachBufferFinalized) for each function
//!   3. Converts relocations to object file format
//!   4. Writes the final object file
//!
//! Reference: ~/learning/wasmtime/cranelift/object/src/backend.rs

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import buffer types for relocation handling
const buffer_mod = @import("machinst/buffer.zig");
const FinalizedMachReloc = buffer_mod.FinalizedMachReloc;
const FinalizedRelocTarget = buffer_mod.FinalizedRelocTarget;
const MachBufferFinalized = buffer_mod.MachBufferFinalized;
const Reloc = buffer_mod.Reloc;
const ExternalName = buffer_mod.ExternalName;
const LibCall = buffer_mod.LibCall;
const KnownSymbol = buffer_mod.KnownSymbol;
const CodeOffset = buffer_mod.CodeOffset;

// Import compile types
const compile_mod = @import("compile.zig");
const CompiledCode = compile_mod.CompiledCode;

// Import object file writers
const macho = @import("macho.zig");
const elf = @import("elf.zig");

// =============================================================================
// FuncId / DataId - Function and Data identifiers
// =============================================================================

/// Function identifier.
/// Mirrors Cranelift's FuncId.
pub const FuncId = struct {
    index: u32,

    pub fn init(index: u32) FuncId {
        return .{ .index = index };
    }
};

/// Data object identifier.
/// Mirrors Cranelift's DataId.
pub const DataId = struct {
    index: u32,

    pub fn init(index: u32) DataId {
        return .{ .index = index };
    }
};

// =============================================================================
// Linkage - Symbol visibility
// =============================================================================

/// Symbol linkage (visibility).
/// Mirrors Cranelift's Linkage enum.
pub const Linkage = enum {
    /// Exported, visible from outside the module.
    Export,
    /// Local to the module, not visible externally.
    Local,
    /// Imported from another module.
    Import,
    /// Preemptible, can be overridden at runtime.
    Preemptible,
};

// =============================================================================
// TargetOS - Target operating system
// =============================================================================

/// Target operating system for object file format selection.
pub const TargetOS = enum {
    macos,
    linux,
    windows,

    /// Detect the current host OS.
    pub fn detectHost() TargetOS {
        const os = @import("builtin").os.tag;
        return switch (os) {
            .macos => .macos,
            .linux => .linux,
            .windows => .windows,
            else => .linux, // Default to ELF for unknown
        };
    }
};

// =============================================================================
// TargetArch - Target architecture
// =============================================================================

/// Target architecture for object file format selection.
pub const TargetArch = enum {
    aarch64,
    x86_64,

    /// Detect the current host architecture.
    pub fn detectHost() TargetArch {
        const arch = @import("builtin").cpu.arch;
        return switch (arch) {
            .aarch64, .aarch64_be => .aarch64,
            .x86_64 => .x86_64,
            else => .x86_64, // Default to x86_64 for unknown
        };
    }
};

// =============================================================================
// FunctionInfo - Declared function metadata
// =============================================================================

/// Information about a declared function.
const FunctionInfo = struct {
    /// Symbol name for the function.
    name: []const u8,
    /// Symbol linkage.
    linkage: Linkage,
    /// Whether the function has been defined.
    defined: bool,
    /// Code offset in the text section (once defined).
    code_offset: u32,
    /// Code size in bytes (once defined).
    code_size: u32,
};

// =============================================================================
// DataInfo - Declared data object metadata
// =============================================================================

/// Information about a declared data object.
const DataInfo = struct {
    /// Symbol name for the data object.
    name: []const u8,
    /// Symbol linkage.
    linkage: Linkage,
    /// Whether writable.
    writable: bool,
    /// Whether the data has been defined.
    defined: bool,
    /// Offset in the data section (once defined).
    data_offset: u32,
    /// Size in bytes.
    size: u32,
};

// =============================================================================
// ObjectModule - Main interface
// =============================================================================

/// Object file module builder.
/// Collects function definitions and produces an object file.
///
/// Mirrors Cranelift's ObjectModule from backend.rs.
pub const ObjectModule = struct {
    const Self = @This();

    /// Allocator for dynamic memory.
    allocator: Allocator,

    /// Target operating system.
    target_os: TargetOS,

    /// Target architecture.
    target_arch: TargetArch,

    /// Declared functions.
    functions: std.ArrayListUnmanaged(FunctionInfo),

    /// Declared data objects.
    data_objects: std.ArrayListUnmanaged(DataInfo),

    /// External name table (maps UserExternalNameRef indices to names).
    external_names: std.ArrayListUnmanaged([]const u8),

    /// Accumulated code bytes.
    text_data: std.ArrayListUnmanaged(u8),

    /// Accumulated data bytes.
    data_section: std.ArrayListUnmanaged(u8),

    /// Accumulated relocations (for object file).
    relocations: std.ArrayListUnmanaged(ObjectReloc),

    /// Accumulated data relocations.
    data_relocations: std.ArrayListUnmanaged(ObjectReloc),

    /// Create a new ObjectModule.
    pub fn init(allocator: Allocator) Self {
        return initWithTarget(allocator, TargetOS.detectHost(), TargetArch.detectHost());
    }

    /// Create a new ObjectModule for a specific target.
    pub fn initWithTarget(allocator: Allocator, os: TargetOS, arch: TargetArch) Self {
        return .{
            .allocator = allocator,
            .target_os = os,
            .target_arch = arch,
            .functions = .{},
            .data_objects = .{},
            .external_names = .{},
            .text_data = .{},
            .data_section = .{},
            .relocations = .{},
            .data_relocations = .{},
        };
    }

    /// Free all resources.
    pub fn deinit(self: *Self) void {
        for (self.functions.items) |func| {
            self.allocator.free(func.name);
        }
        self.functions.deinit(self.allocator);

        for (self.data_objects.items) |data| {
            self.allocator.free(data.name);
        }
        self.data_objects.deinit(self.allocator);

        for (self.external_names.items) |name| {
            self.allocator.free(name);
        }
        self.external_names.deinit(self.allocator);

        self.text_data.deinit(self.allocator);
        self.data_section.deinit(self.allocator);
        self.relocations.deinit(self.allocator);
        self.data_relocations.deinit(self.allocator);
    }

    // =========================================================================
    // Declaration API
    // =========================================================================

    /// Declare a function with the given name and linkage.
    /// Returns a FuncId that can be used to define the function later.
    ///
    /// Mirrors Cranelift's ObjectModule::declare_function.
    pub fn declareFunction(self: *Self, name: []const u8, linkage: Linkage) !FuncId {
        const name_copy = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_copy);

        const id = FuncId.init(@intCast(self.functions.items.len));
        try self.functions.append(self.allocator, .{
            .name = name_copy,
            .linkage = linkage,
            .defined = false,
            .code_offset = 0,
            .code_size = 0,
        });
        return id;
    }

    /// Declare a data object with the given name, linkage, and mutability.
    /// Returns a DataId that can be used to define the data later.
    pub fn declareData(self: *Self, name: []const u8, linkage: Linkage, writable: bool) !DataId {
        const name_copy = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_copy);

        const id = DataId.init(@intCast(self.data_objects.items.len));
        try self.data_objects.append(self.allocator, .{
            .name = name_copy,
            .linkage = linkage,
            .writable = writable,
            .defined = false,
            .data_offset = 0,
            .size = 0,
        });
        return id;
    }

    /// Register an external name for relocation resolution.
    /// The index is used in UserExternalNameRef.
    pub fn declareExternalName(self: *Self, index: u32, name: []const u8) !void {
        // Ensure the array is large enough
        while (self.external_names.items.len <= index) {
            try self.external_names.append(self.allocator, "");
        }

        const name_copy = try self.allocator.dupe(u8, name);
        if (self.external_names.items[index].len > 0) {
            self.allocator.free(self.external_names.items[index]);
        }
        self.external_names.items[index] = name_copy;
    }

    // =========================================================================
    // Definition API
    // =========================================================================

    /// Define a function with the given compiled code.
    ///
    /// Mirrors Cranelift's ObjectModule::define_function_bytes.
    pub fn defineFunction(self: *Self, func_id: FuncId, compiled: *const CompiledCode) !void {
        var func = &self.functions.items[func_id.index];

        // Align to 16 bytes for function entry
        try self.alignTextTo(16);

        // Record the code offset
        func.code_offset = @intCast(self.text_data.items.len);
        func.code_size = compiled.codeSize();

        // Append the code bytes
        const code = compiled.code();
        try self.text_data.appendSlice(self.allocator, code);

        // Process relocations
        for (compiled.relocations()) |reloc| {
            try self.processRelocation(func.code_offset, &reloc);
        }

        func.defined = true;
    }

    /// Define a function with raw bytes and relocations.
    /// This is useful when the code doesn't come from CompiledCode.
    pub fn defineFunctionBytes(
        self: *Self,
        func_id: FuncId,
        code: []const u8,
        relocs: []const FinalizedMachReloc,
    ) !void {
        var func = &self.functions.items[func_id.index];

        // Align to 16 bytes for function entry
        try self.alignTextTo(16);

        // Record the code offset
        func.code_offset = @intCast(self.text_data.items.len);
        func.code_size = @intCast(code.len);

        // Append the code bytes
        try self.text_data.appendSlice(self.allocator, code);

        // Process relocations
        for (relocs) |reloc| {
            try self.processRelocation(func.code_offset, &reloc);
        }

        func.defined = true;
    }

    /// Define a data object with the given bytes.
    pub fn defineData(self: *Self, data_id: DataId, bytes: []const u8) !void {
        var data = &self.data_objects.items[data_id.index];

        // Align to 8 bytes
        try self.alignDataTo(8);

        data.data_offset = @intCast(self.data_section.items.len);
        data.size = @intCast(bytes.len);

        try self.data_section.appendSlice(self.allocator, bytes);

        data.defined = true;
    }

    // =========================================================================
    // Relocation processing
    // =========================================================================

    /// Process a relocation from MachBufferFinalized.
    fn processRelocation(self: *Self, code_base: u32, reloc: *const FinalizedMachReloc) !void {
        const target_name = try self.resolveRelocTarget(reloc.target);

        try self.relocations.append(self.allocator, .{
            .offset = code_base + reloc.offset,
            .target = target_name,
            .kind = reloc.kind,
            .addend = reloc.addend,
        });
    }

    /// Resolve a relocation target to a symbol name.
    fn resolveRelocTarget(self: *Self, target: FinalizedRelocTarget) ![]const u8 {
        return switch (target) {
            .ExternalName => |name| switch (name) {
                .User => |ref| blk: {
                    if (ref.index < self.external_names.items.len) {
                        break :blk self.external_names.items[ref.index];
                    }
                    // Fallback for unregistered external names
                    break :blk try std.fmt.allocPrint(self.allocator, "_func_{d}", .{ref.index});
                },
                .LibCall => |lc| libCallName(lc),
                .KnownSymbol => |ks| knownSymbolName(ks),
                .TestCase => "_test",
            },
            .Func => |offset| blk: {
                // Find which function contains this offset
                for (self.functions.items) |func| {
                    if (func.defined and offset >= func.code_offset and
                        offset < func.code_offset + func.code_size)
                    {
                        break :blk func.name;
                    }
                }
                // Fallback
                break :blk try std.fmt.allocPrint(self.allocator, "_internal_{d}", .{offset});
            },
        };
    }

    // =========================================================================
    // Alignment helpers
    // =========================================================================

    fn alignTextTo(self: *Self, alignment: u32) !void {
        const current = @as(u32, @intCast(self.text_data.items.len));
        const aligned = (current + alignment - 1) & ~(alignment - 1);
        const padding = aligned - current;
        for (0..padding) |_| {
            try self.text_data.append(self.allocator, 0x90); // NOP for x86, harmless for ARM
        }
    }

    fn alignDataTo(self: *Self, alignment: u32) !void {
        const current = @as(u32, @intCast(self.data_section.items.len));
        const aligned = (current + alignment - 1) & ~(alignment - 1);
        const padding = aligned - current;
        for (0..padding) |_| {
            try self.data_section.append(self.allocator, 0);
        }
    }

    // =========================================================================
    // Object file generation
    // =========================================================================

    /// Finish and write the object file to a writer.
    pub fn finish(self: *Self, writer: anytype) !void {
        switch (self.target_os) {
            .macos => try self.writeMachO(writer),
            .linux, .windows => try self.writeELF(writer),
        }
    }

    /// Finish and write the object file to a path.
    pub fn finishToFile(self: *Self, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        try self.finish(file.writer());
    }

    /// Write Mach-O format object file.
    fn writeMachO(self: *Self, writer: anytype) !void {
        var macho_writer = macho.MachOWriter.init(self.allocator);
        defer macho_writer.deinit();

        // Add code section
        try macho_writer.addCode(self.text_data.items);

        // Add data section
        if (self.data_section.items.len > 0) {
            try macho_writer.addData(self.data_section.items);
        }

        // Add function symbols
        for (self.functions.items) |func| {
            if (func.defined) {
                try macho_writer.addSymbol(
                    func.name,
                    func.code_offset,
                    1, // section 1 = __text
                    func.linkage == .Export,
                );
            }
        }

        // Add data symbols
        for (self.data_objects.items) |data| {
            if (data.defined) {
                try macho_writer.addSymbol(
                    data.name,
                    data.data_offset,
                    2, // section 2 = __data
                    data.linkage == .Export,
                );
            }
        }

        // Add relocations (convert from internal format)
        for (self.relocations.items) |reloc| {
            const mach_reloc_type = machORelocType(reloc.kind, self.target_arch);
            if (mach_reloc_type == macho.ARM64_RELOC_BRANCH26) {
                try macho_writer.addRelocation(reloc.offset, reloc.target);
            } else {
                try macho_writer.addDataRelocation(reloc.offset, reloc.target, mach_reloc_type);
            }
        }

        try macho_writer.write(writer);
    }

    /// Write ELF format object file.
    fn writeELF(self: *Self, writer: anytype) !void {
        var elf_writer = elf.ElfWriter.init(self.allocator);
        defer elf_writer.deinit();

        // Add code section
        try elf_writer.addCode(self.text_data.items);

        // Add data section
        if (self.data_section.items.len > 0) {
            try elf_writer.addData(self.data_section.items);
        }

        // Add function symbols
        for (self.functions.items) |func| {
            if (func.defined) {
                try elf_writer.addSymbol(
                    func.name,
                    func.code_offset,
                    1, // section 1 = .text
                    func.linkage == .Export,
                );
            }
        }

        // Add data symbols
        for (self.data_objects.items) |data| {
            if (data.defined) {
                try elf_writer.addSymbol(
                    data.name,
                    data.data_offset,
                    2, // section 2 = .data
                    data.linkage == .Export,
                );
            }
        }

        // Add relocations (convert from internal format)
        for (self.relocations.items) |reloc| {
            // Convert relocation kind to ELF relocation type
            const elf_rel_type = elfRelocType(reloc.kind, self.target_arch);
            try elf_writer.addRelocationWithType(reloc.offset, reloc.target, elf_rel_type, reloc.addend);
        }

        try elf_writer.write(writer);
    }
};

// =============================================================================
// ObjectReloc - Internal relocation format
// =============================================================================

/// Internal relocation record.
const ObjectReloc = struct {
    /// Offset in the text section.
    offset: u32,
    /// Target symbol name.
    target: []const u8,
    /// Relocation kind (from MachBuffer).
    kind: Reloc,
    /// Addend.
    addend: i64,
};

// =============================================================================
// Relocation type conversion
// =============================================================================

/// Convert MachBuffer Reloc to Mach-O relocation type.
fn machORelocType(reloc: Reloc, arch: TargetArch) u4 {
    return switch (arch) {
        .aarch64 => switch (reloc) {
            .Arm64Call => macho.ARM64_RELOC_BRANCH26,
            .Aarch64AdrPrelPgHi21, .Aarch64AdrPrel21, .Arm64AdrGotPage21 => macho.ARM64_RELOC_PAGE21,
            .Aarch64AddAbsLo12Nc, .Aarch64Ldst64AbsLo12Nc, .Aarch64Ldst32AbsLo12Nc,
            .Aarch64Ldst16AbsLo12Nc, .Aarch64Ldst8AbsLo12Nc, .Aarch64Ldst128AbsLo12Nc,
            => macho.ARM64_RELOC_PAGEOFF12,
            .Abs8 => macho.ARM64_RELOC_UNSIGNED,
            else => macho.ARM64_RELOC_UNSIGNED,
        },
        .x86_64 => macho.ARM64_RELOC_UNSIGNED, // x64 uses different constants
    };
}

/// Convert MachBuffer Reloc to ELF relocation type.
fn elfRelocType(reloc: Reloc, arch: TargetArch) u32 {
    return switch (arch) {
        .x86_64 => switch (reloc) {
            .X86PCRel4, .X86PCRelRodata4 => elf.R_X86_64_PC32,
            .X86CallPCRel4, .X86CallPLTRel4 => elf.R_X86_64_PLT32,
            else => elf.R_X86_64_PC32,
        },
        .aarch64 => elf.R_X86_64_PC32, // ARM64 ELF uses different constants
    };
}

// =============================================================================
// LibCall and KnownSymbol name resolution
// =============================================================================

/// Get the symbol name for a library call.
fn libCallName(lc: LibCall) []const u8 {
    return switch (lc) {
        .Probestack => "__probestack",
        .CeilF32 => "ceilf",
        .CeilF64 => "ceil",
        .FloorF32 => "floorf",
        .FloorF64 => "floor",
        .TruncF32 => "truncf",
        .TruncF64 => "trunc",
        .NearestF32 => "nearbyintf",
        .NearestF64 => "nearbyint",
        .FmaF32 => "fmaf",
        .FmaF64 => "fma",
        .Memcpy => "memcpy",
        .Memset => "memset",
        .Memmove => "memmove",
        .Memcmp => "memcmp",
        .ElfTlsGetAddr => "__tls_get_addr",
        .ElfTlsGetOffset => "__tls_get_offset",
        .PltGetAddr => "__plt_get_addr",
        .StackLimit => "__stack_chk_guard",
    };
}

/// Get the symbol name for a known symbol.
fn knownSymbolName(ks: KnownSymbol) []const u8 {
    return switch (ks) {
        .ElfGlobalOffsetTable => "_GLOBAL_OFFSET_TABLE_",
        .CoffTlsIndex => "_tls_index",
    };
}

// =============================================================================
// Tests
// =============================================================================

test "ObjectModule init and deinit" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    try std.testing.expect(module.functions.items.len == 0);
    try std.testing.expect(module.data_objects.items.len == 0);
}

test "ObjectModule declareFunction" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    const func_id = try module.declareFunction("_main", .Export);
    try std.testing.expectEqual(@as(u32, 0), func_id.index);

    const func2_id = try module.declareFunction("_helper", .Local);
    try std.testing.expectEqual(@as(u32, 1), func2_id.index);

    try std.testing.expectEqualStrings("_main", module.functions.items[0].name);
    try std.testing.expectEqual(Linkage.Export, module.functions.items[0].linkage);
}

test "ObjectModule declareData" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    const data_id = try module.declareData("my_data", .Export, true);
    try std.testing.expectEqual(@as(u32, 0), data_id.index);

    try std.testing.expectEqualStrings("my_data", module.data_objects.items[0].name);
    try std.testing.expect(module.data_objects.items[0].writable);
}

test "ObjectModule declareExternalName" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    try module.declareExternalName(0, "_printf");
    try module.declareExternalName(1, "_exit");

    try std.testing.expectEqual(@as(usize, 2), module.external_names.items.len);
    try std.testing.expectEqualStrings("_printf", module.external_names.items[0]);
    try std.testing.expectEqualStrings("_exit", module.external_names.items[1]);
}

test "ObjectModule defineFunctionBytes" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    const func_id = try module.declareFunction("_main", .Export);

    // Simple ARM64 NOP instruction
    const code = [_]u8{ 0x1F, 0x20, 0x03, 0xD5 };
    try module.defineFunctionBytes(func_id, &code, &.{});

    try std.testing.expect(module.functions.items[0].defined);
    try std.testing.expectEqual(@as(u32, 4), module.functions.items[0].code_size);
}

test "ObjectModule defineData" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    const data_id = try module.declareData("my_data", .Export, false);
    try module.defineData(data_id, "Hello, World!");

    try std.testing.expect(module.data_objects.items[0].defined);
}

test "libCallName" {
    try std.testing.expectEqualStrings("memcpy", libCallName(.Memcpy));
    try std.testing.expectEqualStrings("ceilf", libCallName(.CeilF32));
}

test "knownSymbolName" {
    try std.testing.expectEqualStrings("_GLOBAL_OFFSET_TABLE_", knownSymbolName(.ElfGlobalOffsetTable));
}

test "TargetOS detectHost" {
    const os = TargetOS.detectHost();
    const builtin_os = @import("builtin").os.tag;
    switch (builtin_os) {
        .macos => try std.testing.expect(os == .macos),
        .linux => try std.testing.expect(os == .linux),
        .windows => try std.testing.expect(os == .windows),
        else => {},
    }
}

test "TargetArch detectHost" {
    const arch = TargetArch.detectHost();
    const builtin_arch = @import("builtin").cpu.arch;
    switch (builtin_arch) {
        .aarch64, .aarch64_be => try std.testing.expect(arch == .aarch64),
        .x86_64 => try std.testing.expect(arch == .x86_64),
        else => {},
    }
}

test "ObjectModule full workflow" {
    const allocator = std.testing.allocator;
    var module = ObjectModule.init(allocator);
    defer module.deinit();

    // Declare and define a function
    const func_id = try module.declareFunction("_main", .Export);
    const code = [_]u8{ 0x1F, 0x20, 0x03, 0xD5, 0xC0, 0x03, 0x5F, 0xD6 }; // NOP + RET
    try module.defineFunctionBytes(func_id, &code, &.{});

    // Write to memory
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try module.finish(output.writer(allocator));

    // Verify something was written
    try std.testing.expect(output.items.len > 0);
}
