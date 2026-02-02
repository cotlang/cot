//! MachBuffer: Machine code buffer with labels, fixups, and branch optimization.
//!
//! This is a faithful port of Cranelift's `machinst/buffer.rs`.
//! The buffer accumulates machine code bytes and metadata (relocations, traps,
//! call sites, source locations) during instruction emission, then produces
//! a finalized buffer with all labels resolved.
//!
//! Key features:
//! - Label-based forward references resolved via fixups
//! - Branch optimization (elision, redirection, condition flipping)
//! - Island/veneer emission for out-of-range jumps
//! - Constant pool management
//!
//! Reference: ~/learning/wasmtime/cranelift/codegen/src/machinst/buffer.rs

const std = @import("std");
const Allocator = std.mem.Allocator;
const inst = @import("inst.zig");
const reg = @import("reg.zig");

/// Code offset type (u32 like Cranelift).
pub const CodeOffset = u32;

/// Addend type for relocations.
pub const Addend = i64;

// =============================================================================
// External types (stubs - these would be defined elsewhere in a full port)
// =============================================================================

/// Relocation kind.
pub const Reloc = enum {
    Abs4,
    Abs8,
    X86PCRel4,
    X86PCRelRodata4,
    X86CallPCRel4,
    X86CallPLTRel4,
    X86GOTPCRel4,
    X86SecRel,
    Arm64Call,
    Arm64AdrGotPage21,
    Arm64Ld64GotLo12Nc,
    RiscvCallPlt,
    RiscvPCRelHi20,
    RiscvPCRelLo12I,
    Aarch64AdrPrel21,
    Aarch64AdrPrelPgHi21,
    Aarch64AddAbsLo12Nc,
    Aarch64Ldst128AbsLo12Nc,
    Aarch64Ldst64AbsLo12Nc,
    Aarch64Ldst32AbsLo12Nc,
    Aarch64Ldst16AbsLo12Nc,
    Aarch64Ldst8AbsLo12Nc,
    MachOX86_64Tlv,
    MachOAarch64TlvPage21,
    MachOAarch64TlvPageOff12,
    Elf_X86_64_TlsGd,
    Elf_X86_64_GottpOff,
    Aarch64TlsGdAdrPage21,
    Aarch64TlsGdAddLo12Nc,
    Aarch64TlsDescAdrPage21,
    Aarch64TlsDescLd64Lo12,
    Aarch64TlsDescAddLo12,
    Aarch64TlsDescCall,
    S390xPCRel32Dbl,
    S390xPltRel32Dbl,
    S390xGotPCRel32Dbl,
    S390xGotEntPCRel32Dbl,
    S390xTlsGd64,
    S390xTlsGdCall,
};

/// Trap code.
pub const TrapCode = enum(u8) {
    STACK_OVERFLOW = 0,
    HEAP_OUT_OF_BOUNDS = 1,
    HEAP_MISALIGNED = 2,
    TABLE_OUT_OF_BOUNDS = 3,
    INDIRECT_CALL_TO_NULL = 4,
    BAD_SIGNATURE = 5,
    INTEGER_OVERFLOW = 6,
    INTEGER_DIVISION_BY_ZERO = 7,
    BAD_CONVERSION_TO_INTEGER = 8,
    UNREACHABLE_CODE_REACHED = 9,
    INTERRUPT = 10,
    NULL_REFERENCE = 11,
    NULL_I31REF = 12,
    ARRAY_OUT_OF_BOUNDS = 13,
    ALLOCATION_FAILED = 14,
    CAST_FAILURE = 15,
    User = 16,
};

/// Exception tag.
pub const ExceptionTag = struct {
    value: u32,

    pub fn init(value: u32) ExceptionTag {
        return .{ .value = value };
    }
};

/// Source location (relative).
pub const RelSourceLoc = struct {
    offset: u32,

    pub fn expand(self: RelSourceLoc, base: SourceLoc) SourceLoc {
        return .{ .offset = base.offset +% self.offset };
    }
};

/// Source location (absolute).
pub const SourceLoc = struct {
    offset: u32,

    pub const default: SourceLoc = .{ .offset = 0 };
};

/// External name reference.
pub const ExternalName = union(enum) {
    User: UserExternalNameRef,
    TestCase: struct { length: u32 },
    LibCall: LibCall,
    KnownSymbol: KnownSymbol,
};

/// User external name reference.
/// Matches Cranelift's UserExternalName in ir/extname.rs
pub const UserExternalNameRef = struct {
    namespace: u32,
    index: u32,

    pub fn init(index: u32) UserExternalNameRef {
        return .{ .namespace = 0, .index = index };
    }

    pub fn initFull(namespace: u32, index: u32) UserExternalNameRef {
        return .{ .namespace = namespace, .index = index };
    }
};

/// Library call.
pub const LibCall = enum {
    Probestack,
    CeilF32,
    CeilF64,
    FloorF32,
    FloorF64,
    TruncF32,
    TruncF64,
    NearestF32,
    NearestF64,
    FmaF32,
    FmaF64,
    Memcpy,
    Memset,
    Memmove,
    Memcmp,
    ElfTlsGetAddr,
    ElfTlsGetOffset,
    PltGetAddr,
    StackLimit,
};

/// Known symbol.
pub const KnownSymbol = enum {
    ElfGlobalOffsetTable,
    CoffTlsIndex,
};

/// Block index - use the unified type from inst.zig.
pub const BlockIndex = inst.BlockIndex;

// =============================================================================
// MachLabel
// =============================================================================

/// A label in the machine code buffer.
/// Use the unified type from inst.zig.
pub const MachLabel = inst.MachLabel;

// =============================================================================
// LabelUse trait interface
// =============================================================================

/// Interface for label use types. Each architecture defines its own LabelUse
/// type describing how labels are used in branch instructions.
pub fn LabelUse(comptime T: type) type {
    return struct {
        /// The maximum positive range for this label use.
        pub fn maxPosRange(label_use: T) CodeOffset {
            return label_use.max_pos_range;
        }

        /// The maximum negative range for this label use.
        pub fn maxNegRange(label_use: T) CodeOffset {
            return label_use.max_neg_range;
        }

        /// The size of the patch in bytes.
        pub fn patchSize(label_use: T) usize {
            return label_use.patch_size;
        }

        /// Whether this label use supports veneers.
        pub fn supportsVeneer(label_use: T) bool {
            return label_use.supports_veneer;
        }

        /// Patch the instruction at the given offset.
        pub fn patch(label_use: T, buffer: []u8, use_offset: CodeOffset, label_offset: CodeOffset) void {
            label_use.patchFn(buffer, use_offset, label_offset);
        }

        /// Generate a veneer for out-of-range jumps.
        pub fn generateVeneer(label_use: T, buffer: *MachBuffer(T), veneer_offset: CodeOffset, label_offset: CodeOffset) ?T {
            if (label_use.generateVeneerFn) |gen_fn| {
                return gen_fn(buffer, veneer_offset, label_offset);
            }
            return null;
        }
    };
}

// =============================================================================
// MachBranch - branch record for optimization
// =============================================================================

/// Record of a branch instruction in the buffer, to facilitate editing.
pub fn MachBranch(comptime LabelUseType: type) type {
    _ = LabelUseType;
    return struct {
        const Self = @This();

        /// Start offset of the branch instruction.
        start: CodeOffset,
        /// End offset of the branch instruction.
        end: CodeOffset,
        /// Target label.
        target: MachLabel,
        /// Index into the fixups list.
        fixup: usize,
        /// Inverted bytes (for conditional branches that can be flipped).
        /// If null, this is an unconditional branch.
        inverted: ?[8]u8,
        inverted_len: u8,
        /// Labels at this branch (for redirecting).
        labels_at_this_branch: std.ArrayListUnmanaged(MachLabel),

        pub fn isCond(self: *const Self) bool {
            return self.inverted != null;
        }

        pub fn isUncond(self: *const Self) bool {
            return self.inverted == null;
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.labels_at_this_branch.deinit(allocator);
        }
    };
}

// =============================================================================
// MachLabelFixup - fixup record
// =============================================================================

/// A fixup to perform on the buffer once code is emitted.
/// Fixups refer to labels and patch the code based on label offsets.
pub fn MachLabelFixup(comptime LabelUseType: type) type {
    return struct {
        const Self = @This();

        /// The label whose offset controls this fixup.
        label: MachLabel,
        /// The offset to fix up / patch.
        offset: CodeOffset,
        /// The kind of fixup (architecture-specific).
        kind: LabelUseType,

        /// Calculate the deadline for this fixup.
        pub fn deadline(self: Self) CodeOffset {
            return self.offset +| self.kind.max_pos_range;
        }

        /// Compare by deadline (for priority queue - earlier deadlines first).
        pub fn compare(a: Self, b: Self) std.math.Order {
            // Note: reversed order for min-heap behavior (earliest deadline first)
            return std.math.order(b.deadline(), a.deadline());
        }
    };
}

// =============================================================================
// MachBufferConstant - constant metadata
// =============================================================================

/// Metadata about a constant in the constant pool.
const MachBufferConstant = struct {
    /// A label for this constant (lazily created).
    upcoming_label: ?MachLabel,
    /// Required alignment.
    alignment: CodeOffset,
    /// Size in bytes.
    size: usize,
};

// =============================================================================
// MachLabelTrap - deferred trap
// =============================================================================

/// A trap that is deferred to the next island emission.
const MachLabelTrap = struct {
    /// The label that will refer to the trap's offset.
    label: MachLabel,
    /// The trap code.
    code: TrapCode,
    /// Optional source location.
    loc: ?RelSourceLoc,
};

// =============================================================================
// Relocation records
// =============================================================================

/// Relocation target (before finalization).
pub const RelocTarget = union(enum) {
    /// Points to an external name.
    ExternalName: ExternalName,
    /// Points to a label inside this function.
    Label: MachLabel,
};

/// Relocation target (after finalization).
pub const FinalizedRelocTarget = union(enum) {
    /// Points to an external name.
    ExternalName: ExternalName,
    /// Points to a code offset within this function.
    Func: CodeOffset,
};

/// A relocation record (before finalization).
pub const MachReloc = struct {
    /// Offset at which the relocation applies.
    offset: CodeOffset,
    /// Kind of relocation.
    kind: Reloc,
    /// Target of the relocation.
    target: RelocTarget,
    /// Addend.
    addend: Addend,
};

/// A relocation record (after finalization).
pub const FinalizedMachReloc = struct {
    /// Offset at which the relocation applies.
    offset: CodeOffset,
    /// Kind of relocation.
    kind: Reloc,
    /// Target of the relocation.
    target: FinalizedRelocTarget,
    /// Addend.
    addend: Addend,
};

// =============================================================================
// Trap records
// =============================================================================

/// A trap record.
pub const MachTrap = struct {
    /// Offset of the trap instruction.
    offset: CodeOffset,
    /// Trap code.
    code: TrapCode,
};

// =============================================================================
// Call site records
// =============================================================================

/// Exception handler (before finalization).
pub const MachExceptionHandler = union(enum) {
    /// A specific tag should be handled at the given label.
    Tag: struct { tag: ExceptionTag, label: MachLabel },
    /// All exceptions should be handled at the given label.
    Default: MachLabel,
    /// Update the dynamic context.
    Context: ExceptionContextLoc,

    pub fn finalize(self: MachExceptionHandler, labelToOffset: fn (MachLabel) CodeOffset) FinalizedMachExceptionHandler {
        return switch (self) {
            .Tag => |t| .{ .Tag = .{ .tag = t.tag, .offset = labelToOffset(t.label) } },
            .Default => |label| .{ .Default = labelToOffset(label) },
            .Context => |loc| .{ .Context = loc },
        };
    }
};

/// Exception handler (after finalization).
pub const FinalizedMachExceptionHandler = union(enum) {
    /// A specific tag should be handled at the given offset.
    Tag: struct { tag: ExceptionTag, offset: CodeOffset },
    /// All exceptions should be handled at the given offset.
    Default: CodeOffset,
    /// Update the dynamic context.
    Context: ExceptionContextLoc,
};

/// Exception context location.
pub const ExceptionContextLoc = union(enum) {
    /// Offset from SP at the callsite.
    SPOffset: u32,
    /// A GPR at the callsite.
    GPR: u8,
};

/// A call site record (before finalization).
pub const MachCallSite = struct {
    /// Offset of the call's return address.
    ret_addr: CodeOffset,
    /// Frame offset from FP to SP, if known.
    frame_offset: ?u32,
    /// Range in exception_handlers.
    exception_handler_start: u32,
    exception_handler_end: u32,
};

/// A call site record (after finalization).
pub const FinalizedMachCallSite = struct {
    /// Offset of the call's return address.
    ret_addr: CodeOffset,
    /// Frame offset from FP to SP, if known.
    frame_offset: ?u32,
    /// Exception handlers.
    exception_handlers: []const FinalizedMachExceptionHandler,
};

/// A patchable call site.
pub const MachPatchableCallSite = struct {
    /// Offset of the call's return address.
    ret_addr: CodeOffset,
    /// Length of the patchable region.
    len: u32,
};

// =============================================================================
// Source location records
// =============================================================================

/// Source location mapping (generic over compile phase).
pub fn MachSrcLoc(comptime T: type) type {
    return struct {
        /// Start of the code region.
        start: CodeOffset,
        /// End of the code region.
        end: CodeOffset,
        /// Source location.
        loc: T,
    };
}

// =============================================================================
// Frame layout
// =============================================================================

/// Stack slot descriptor.
pub const MachBufferStackSlot = struct {
    /// Offset from bottom of frame.
    offset: u32,
    /// User-provided key.
    key: ?u32,
};

/// Frame layout information.
pub const MachBufferFrameLayout = struct {
    /// Offset from bottom of frame to FP.
    frame_to_fp_offset: u32,
    /// Stack slots.
    stackslots: std.ArrayListUnmanaged(MachBufferStackSlot),

    pub fn deinit(self: *MachBufferFrameLayout, allocator: Allocator) void {
        self.stackslots.deinit(allocator);
    }
};

// =============================================================================
// Debug tags
// =============================================================================

/// Debug tag position.
pub const MachDebugTagPos = enum {
    /// Tags attached after the instruction (for call return points).
    Post,
    /// Tags attached before the instruction (for traps).
    Pre,
};

/// Debug tag.
pub const DebugTag = struct {
    value: u32,
};

/// Debug tags at an offset.
pub const MachDebugTags = struct {
    offset: CodeOffset,
    pos: MachDebugTagPos,
    range_start: u32,
    range_end: u32,
};

// =============================================================================
// Veneer forcing
// =============================================================================

/// Whether to force veneer emission.
pub const ForceVeneers = enum {
    No,
    Yes,
};

// =============================================================================
// MachBuffer - the main buffer
// =============================================================================

/// Machine code buffer with labels, fixups, and branch optimization.
pub fn MachBuffer(comptime LabelUseType: type) type {
    return struct {
        const Self = @This();
        const BranchType = MachBranch(LabelUseType);
        const FixupType = MachLabelFixup(LabelUseType);

        /// Allocator for dynamic allocations.
        allocator: Allocator,

        /// The machine code bytes.
        data: std.ArrayListUnmanaged(u8),

        /// Relocations.
        relocs: std.ArrayListUnmanaged(MachReloc),

        /// Traps.
        traps: std.ArrayListUnmanaged(MachTrap),

        /// Call sites.
        call_sites: std.ArrayListUnmanaged(MachCallSite),

        /// Exception handlers (shared storage).
        exception_handlers: std.ArrayListUnmanaged(MachExceptionHandler),

        /// Patchable call sites.
        patchable_call_sites: std.ArrayListUnmanaged(MachPatchableCallSite),

        /// Source locations (stencil form).
        srclocs: std.ArrayListUnmanaged(MachSrcLoc(RelSourceLoc)),

        /// Debug tags.
        debug_tags: std.ArrayListUnmanaged(MachDebugTags),

        /// Debug tag pool.
        debug_tag_pool: std.ArrayListUnmanaged(DebugTag),

        /// Label offsets (UNKNOWN if not yet bound).
        label_offsets: std.ArrayListUnmanaged(CodeOffset),

        /// Label aliases (for redirecting labels).
        label_aliases: std.ArrayListUnmanaged(MachLabel),

        /// Pending constants.
        pending_constants: std.ArrayListUnmanaged(struct { label: MachLabel, constant: MachBufferConstant }),

        /// Pending traps.
        pending_traps: std.ArrayListUnmanaged(MachLabelTrap),

        /// Pending fixups (priority queue by deadline).
        pending_fixups: std.ArrayListUnmanaged(FixupType),

        /// Fixup records for island emission.
        fixup_records: std.ArrayListUnmanaged(FixupType),

        /// Branches for optimization.
        branches: std.ArrayListUnmanaged(BranchType),

        /// Current source location.
        cur_srcloc: ?RelSourceLoc,

        /// Start offset for current source location.
        cur_srcloc_start: CodeOffset,

        /// Latest branches at current offset (for optimization).
        latest_branches: std.ArrayListUnmanaged(usize),

        /// Labels at current offset.
        labels_at_tail: std.ArrayListUnmanaged(MachLabel),

        /// Constant data pool.
        constant_data: std.ArrayListUnmanaged(u8),

        /// Total constants alignment.
        constants_alignment: CodeOffset,

        /// Unknown offset sentinel.
        pub const UNKNOWN_OFFSET: CodeOffset = std.math.maxInt(CodeOffset);

        /// Create a new machine buffer.
        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .data = .{},
                .relocs = .{},
                .traps = .{},
                .call_sites = .{},
                .exception_handlers = .{},
                .patchable_call_sites = .{},
                .srclocs = .{},
                .debug_tags = .{},
                .debug_tag_pool = .{},
                .label_offsets = .{},
                .label_aliases = .{},
                .pending_constants = .{},
                .pending_traps = .{},
                .pending_fixups = .{},
                .fixup_records = .{},
                .branches = .{},
                .cur_srcloc = null,
                .cur_srcloc_start = 0,
                .latest_branches = .{},
                .labels_at_tail = .{},
                .constant_data = .{},
                .constants_alignment = 1,
            };
        }

        /// Free all resources.
        pub fn deinit(self: *Self) void {
            self.data.deinit(self.allocator);
            self.relocs.deinit(self.allocator);
            self.traps.deinit(self.allocator);
            self.call_sites.deinit(self.allocator);
            self.exception_handlers.deinit(self.allocator);
            self.patchable_call_sites.deinit(self.allocator);
            self.srclocs.deinit(self.allocator);
            self.debug_tags.deinit(self.allocator);
            self.debug_tag_pool.deinit(self.allocator);
            self.label_offsets.deinit(self.allocator);
            self.label_aliases.deinit(self.allocator);
            self.pending_constants.deinit(self.allocator);
            self.pending_traps.deinit(self.allocator);
            self.pending_fixups.deinit(self.allocator);
            self.fixup_records.deinit(self.allocator);
            for (self.branches.items) |*branch| {
                branch.deinit(self.allocator);
            }
            self.branches.deinit(self.allocator);
            self.latest_branches.deinit(self.allocator);
            self.labels_at_tail.deinit(self.allocator);
            self.constant_data.deinit(self.allocator);
        }

        // =====================================================================
        // Basic operations
        // =====================================================================

        /// Get the current offset in the buffer.
        pub fn curOffset(self: *const Self) CodeOffset {
            return @intCast(self.data.items.len);
        }

        /// Get the data slice.
        pub fn getData(self: *const Self) []const u8 {
            return self.data.items;
        }

        /// Get the data slice (mutable).
        pub fn getDataMut(self: *Self) []u8 {
            return self.data.items;
        }

        /// Emit a single byte.
        pub fn put1(self: *Self, byte: u8) !void {
            try self.data.append(self.allocator, byte);
        }

        /// Emit a 16-bit value (little-endian).
        pub fn put2(self: *Self, value: u16) !void {
            try self.data.appendSlice(self.allocator, &std.mem.toBytes(std.mem.nativeToLittle(u16, value)));
        }

        /// Emit a 32-bit value (little-endian).
        pub fn put4(self: *Self, value: u32) !void {
            try self.data.appendSlice(self.allocator, &std.mem.toBytes(std.mem.nativeToLittle(u32, value)));
        }

        /// Emit a 64-bit value (little-endian).
        pub fn put8(self: *Self, value: u64) !void {
            try self.data.appendSlice(self.allocator, &std.mem.toBytes(std.mem.nativeToLittle(u64, value)));
        }

        /// Emit a signed 32-bit value (little-endian).
        pub fn putSimm32(self: *Self, value: i32) !void {
            try self.put4(@bitCast(value));
        }

        /// Emit data bytes.
        pub fn putData(self: *Self, bytes: []const u8) !void {
            try self.data.appendSlice(self.allocator, bytes);
        }

        /// Reset the buffer for reuse (primarily for testing).
        pub fn reset(self: *Self) void {
            self.data.clearRetainingCapacity();
            // curOffset() derives from data.items.len, so clearing data is enough
        }

        /// Align the buffer to a given alignment.
        pub fn alignTo(self: *Self, alignment: u32) !void {
            const cur = self.curOffset();
            const aligned = (cur + alignment - 1) & ~(alignment - 1);
            const padding = aligned - cur;
            for (0..padding) |_| {
                try self.put1(0);
            }
        }

        // =====================================================================
        // Label management
        // =====================================================================

        /// Reserve labels for the given number of blocks.
        pub fn reserveLabelsForBlocks(self: *Self, num_blocks: usize) !void {
            try self.label_offsets.resize(self.allocator, num_blocks);
            try self.label_aliases.resize(self.allocator, num_blocks);
            for (0..num_blocks) |i| {
                self.label_offsets.items[i] = UNKNOWN_OFFSET;
                self.label_aliases.items[i] = MachLabel.init(@intCast(i));
            }
        }

        /// Get a new label.
        pub fn getLabel(self: *Self) !MachLabel {
            const label = MachLabel.init(@intCast(self.label_offsets.items.len));
            try self.label_offsets.append(self.allocator, UNKNOWN_OFFSET);
            try self.label_aliases.append(self.allocator, label);
            return label;
        }

        /// Bind a label to the current offset.
        pub fn bindLabel(self: *Self, label: MachLabel) !void {
            const offset = self.curOffset();
            self.label_offsets.items[label.index] = offset;

            // Add to labels at tail.
            try self.labels_at_tail.append(self.allocator, label);
        }

        /// Resolve a label to its final alias.
        pub fn resolveLabel(self: *const Self, label: MachLabel) MachLabel {
            var current = label;
            while (true) {
                const alias = self.label_aliases.items[current.index];
                if (alias.eql(current)) {
                    return current;
                }
                current = alias;
            }
        }

        /// Get the offset for a bound label (or UNKNOWN_OFFSET).
        pub fn labelOffset(self: *const Self, label: MachLabel) CodeOffset {
            const resolved = self.resolveLabel(label);
            return self.label_offsets.items[resolved.index];
        }

        /// Use a label at a specific offset, creating a fixup if needed.
        pub fn useLabelAtOffset(self: *Self, offset: CodeOffset, label: MachLabel, kind: LabelUseType) !void {
            const label_offset = self.labelOffset(label);

            if (label_offset != UNKNOWN_OFFSET) {
                // Label is already bound - patch immediately.
                kind.patch(self.data.items, offset, label_offset);
            } else {
                // Forward reference - create a fixup.
                try self.pending_fixups.append(self.allocator, .{
                    .label = label,
                    .offset = offset,
                    .kind = kind,
                });
            }
        }

        // =====================================================================
        // Metadata records
        // =====================================================================

        /// Add a relocation.
        pub fn addReloc(self: *Self, kind: Reloc, target: RelocTarget, addend: Addend) !void {
            try self.relocs.append(self.allocator, .{
                .offset = self.curOffset(),
                .kind = kind,
                .target = target,
                .addend = addend,
            });
        }

        /// Add a relocation to an external name.
        pub fn addRelocExternalName(self: *Self, kind: Reloc, name: ExternalName, addend: Addend) !void {
            try self.addReloc(kind, .{ .ExternalName = name }, addend);
        }

        /// Add a trap.
        pub fn addTrap(self: *Self, code: TrapCode) !void {
            try self.traps.append(self.allocator, .{
                .offset = self.curOffset(),
                .code = code,
            });
        }

        /// Add a call site.
        pub fn addCallSite(self: *Self, frame_offset: ?u32) !void {
            try self.call_sites.append(self.allocator, .{
                .ret_addr = self.curOffset(),
                .frame_offset = frame_offset,
                .exception_handler_start = @intCast(self.exception_handlers.items.len),
                .exception_handler_end = @intCast(self.exception_handlers.items.len),
            });
        }

        /// Add a try-call site with exception handlers.
        pub fn addTryCallSite(self: *Self, frame_offset: ?u32, handlers: []const MachExceptionHandler) !void {
            const start = @as(u32, @intCast(self.exception_handlers.items.len));
            try self.exception_handlers.appendSlice(self.allocator, handlers);
            const end = @as(u32, @intCast(self.exception_handlers.items.len));

            try self.call_sites.append(self.allocator, .{
                .ret_addr = self.curOffset(),
                .frame_offset = frame_offset,
                .exception_handler_start = start,
                .exception_handler_end = end,
            });
        }

        /// Add a patchable call site.
        pub fn addPatchableCallSite(self: *Self, len: u32) !void {
            try self.patchable_call_sites.append(self.allocator, .{
                .ret_addr = self.curOffset(),
                .len = len,
            });
        }

        // =====================================================================
        // Source locations
        // =====================================================================

        /// Start tracking a source location.
        pub fn startSrcloc(self: *Self, loc: RelSourceLoc) void {
            self.cur_srcloc = loc;
            self.cur_srcloc_start = self.curOffset();
        }

        /// End tracking the current source location.
        pub fn endSrcloc(self: *Self) !void {
            if (self.cur_srcloc) |loc| {
                const end = self.curOffset();
                if (end > self.cur_srcloc_start) {
                    try self.srclocs.append(self.allocator, .{
                        .start = self.cur_srcloc_start,
                        .end = end,
                        .loc = loc,
                    });
                }
                self.cur_srcloc = null;
            }
        }

        // =====================================================================
        // Branch tracking
        // =====================================================================

        /// Add an unconditional branch.
        pub fn addUncondBranch(self: *Self, start: CodeOffset, end: CodeOffset, target: MachLabel, fixup_index: usize) !void {
            var branch = BranchType{
                .start = start,
                .end = end,
                .target = target,
                .fixup = fixup_index,
                .inverted = null,
                .inverted_len = 0,
                .labels_at_this_branch = .{},
            };

            // Copy labels at tail.
            for (self.labels_at_tail.items) |label| {
                try branch.labels_at_this_branch.append(self.allocator, label);
            }

            try self.branches.append(self.allocator, branch);
            try self.latest_branches.append(self.allocator, self.branches.items.len - 1);
        }

        /// Add a conditional branch.
        pub fn addCondBranch(
            self: *Self,
            start: CodeOffset,
            end: CodeOffset,
            target: MachLabel,
            fixup_index: usize,
            inverted: []const u8,
        ) !void {
            var branch = BranchType{
                .start = start,
                .end = end,
                .target = target,
                .fixup = fixup_index,
                .inverted = undefined,
                .inverted_len = @intCast(inverted.len),
                .labels_at_this_branch = .{},
            };
            // Copy inverted bytes.
            @memcpy(branch.inverted.?[0..inverted.len], inverted);

            // Copy labels at tail.
            for (self.labels_at_tail.items) |label| {
                try branch.labels_at_this_branch.append(self.allocator, label);
            }

            try self.branches.append(self.allocator, branch);
            try self.latest_branches.append(self.allocator, self.branches.items.len - 1);
        }

        /// Clear latest branches (called when non-branch code is emitted).
        pub fn clearLatestBranches(self: *Self) void {
            self.latest_branches.clearRetainingCapacity();
            self.labels_at_tail.clearRetainingCapacity();
        }

        // =====================================================================
        // Island/veneer emission
        // =====================================================================

        /// Check if an island is needed for the given amount of code.
        pub fn islandNeeded(self: *const Self, distance: CodeOffset) bool {
            if (self.pending_fixups.items.len == 0 and self.pending_constants.items.len == 0) {
                return false;
            }

            // Find earliest deadline.
            var earliest_deadline: CodeOffset = std.math.maxInt(CodeOffset);
            for (self.pending_fixups.items) |fixup| {
                const deadline = fixup.deadline();
                if (deadline < earliest_deadline) {
                    earliest_deadline = deadline;
                }
            }

            // Check if we'll exceed the deadline.
            const future_offset = self.curOffset() +| distance;
            return future_offset >= earliest_deadline;
        }

        /// Emit an island if needed.
        pub fn emitIsland(self: *Self, distance: CodeOffset) !void {
            if (!self.islandNeeded(distance)) {
                return;
            }
            try self.emitIslandForced(distance);
        }

        /// Force island emission.
        pub fn emitIslandForced(self: *Self, distance: CodeOffset) !void {
            _ = distance;

            // Process all fixups that need veneers.
            var i: usize = 0;
            while (i < self.pending_fixups.items.len) {
                const fixup = self.pending_fixups.items[i];
                const label_offset = self.labelOffset(fixup.label);

                if (label_offset != UNKNOWN_OFFSET) {
                    // Label is bound - patch and remove.
                    fixup.kind.patch(self.data.items, fixup.offset, label_offset);
                    _ = self.pending_fixups.orderedRemove(i);
                } else {
                    // Check if we need a veneer.
                    const deadline = fixup.deadline();
                    if (self.curOffset() >= deadline) {
                        // Need a veneer.
                        // For now, just keep the fixup - actual veneer emission is architecture-specific.
                        i += 1;
                    } else {
                        i += 1;
                    }
                }
            }

            // Emit pending traps.
            for (self.pending_traps.items) |trap| {
                self.label_offsets.items[trap.label.index] = self.curOffset();
                try self.traps.append(self.allocator, .{
                    .offset = self.curOffset(),
                    .code = trap.code,
                });
                // Emit a trap instruction (architecture-specific - placeholder).
            }
            self.pending_traps.clearRetainingCapacity();
        }

        // =====================================================================
        // Finalization
        // =====================================================================

        /// Finish emission and return the finalized buffer.
        pub fn finish(self: *Self) !MachBufferFinalized {
            // Resolve remaining fixups.
            for (self.pending_fixups.items) |fixup| {
                const label_offset = self.labelOffset(fixup.label);
                if (label_offset != UNKNOWN_OFFSET) {
                    fixup.kind.patch(self.data.items, fixup.offset, label_offset);
                } else {
                    // Unresolved fixup - this is an error in a complete implementation.
                    return error.UnresolvedLabel;
                }
            }
            self.pending_fixups.clearRetainingCapacity();

            // Finalize relocations.
            var finalized_relocs = std.ArrayListUnmanaged(FinalizedMachReloc){};
            for (self.relocs.items) |reloc| {
                const finalized_target: FinalizedRelocTarget = switch (reloc.target) {
                    .ExternalName => |name| .{ .ExternalName = name },
                    .Label => |label| .{ .Func = self.labelOffset(label) },
                };
                try finalized_relocs.append(self.allocator, .{
                    .offset = reloc.offset,
                    .kind = reloc.kind,
                    .target = finalized_target,
                    .addend = reloc.addend,
                });
            }

            // Finalize exception handlers.
            var finalized_handlers = std.ArrayListUnmanaged(FinalizedMachExceptionHandler){};
            for (self.exception_handlers.items) |handler| {
                const finalized: FinalizedMachExceptionHandler = switch (handler) {
                    .Tag => |t| .{ .Tag = .{ .tag = t.tag, .offset = self.labelOffset(t.label) } },
                    .Default => |label| .{ .Default = self.labelOffset(label) },
                    .Context => |loc| .{ .Context = loc },
                };
                try finalized_handlers.append(self.allocator, finalized);
            }

            // Create owned copy of data.
            var data_copy = std.ArrayListUnmanaged(u8){};
            try data_copy.appendSlice(self.allocator, self.data.items);

            // Create owned copies of metadata.
            var traps_copy = std.ArrayListUnmanaged(MachTrap){};
            try traps_copy.appendSlice(self.allocator, self.traps.items);

            var call_sites_copy = std.ArrayListUnmanaged(MachCallSite){};
            try call_sites_copy.appendSlice(self.allocator, self.call_sites.items);

            var srclocs_copy = std.ArrayListUnmanaged(MachSrcLoc(RelSourceLoc)){};
            try srclocs_copy.appendSlice(self.allocator, self.srclocs.items);

            var patchable_copy = std.ArrayListUnmanaged(MachPatchableCallSite){};
            try patchable_copy.appendSlice(self.allocator, self.patchable_call_sites.items);

            return .{
                .allocator = self.allocator,
                .data = data_copy,
                .relocs = finalized_relocs,
                .traps = traps_copy,
                .call_sites = call_sites_copy,
                .exception_handlers = finalized_handlers,
                .srclocs = srclocs_copy,
                .patchable_call_sites = patchable_copy,
                .alignment = 1,
            };
        }

        /// Get the total size of the buffer.
        pub fn totalSize(self: *const Self) CodeOffset {
            return self.curOffset();
        }
    };
}

// =============================================================================
// MachBufferFinalized - finalized buffer
// =============================================================================

/// Finalized machine code buffer.
pub const MachBufferFinalized = struct {
    allocator: Allocator,

    /// The machine code bytes.
    data: std.ArrayListUnmanaged(u8),

    /// Relocations.
    relocs: std.ArrayListUnmanaged(FinalizedMachReloc),

    /// Traps.
    traps: std.ArrayListUnmanaged(MachTrap),

    /// Call sites.
    call_sites: std.ArrayListUnmanaged(MachCallSite),

    /// Exception handlers.
    exception_handlers: std.ArrayListUnmanaged(FinalizedMachExceptionHandler),

    /// Source locations.
    srclocs: std.ArrayListUnmanaged(MachSrcLoc(RelSourceLoc)),

    /// Patchable call sites.
    patchable_call_sites: std.ArrayListUnmanaged(MachPatchableCallSite),

    /// Required alignment.
    alignment: u32,

    /// Free all resources.
    pub fn deinit(self: *MachBufferFinalized) void {
        self.data.deinit(self.allocator);
        self.relocs.deinit(self.allocator);
        self.traps.deinit(self.allocator);
        self.call_sites.deinit(self.allocator);
        self.exception_handlers.deinit(self.allocator);
        self.srclocs.deinit(self.allocator);
        self.patchable_call_sites.deinit(self.allocator);
    }

    /// Get the data.
    pub fn getData(self: *const MachBufferFinalized) []const u8 {
        return self.data.items;
    }

    /// Get the total size.
    pub fn totalSize(self: *const MachBufferFinalized) CodeOffset {
        return @intCast(self.data.items.len);
    }

    /// Get the relocations.
    pub fn getRelocs(self: *const MachBufferFinalized) []const FinalizedMachReloc {
        return self.relocs.items;
    }

    /// Get the traps.
    pub fn getTraps(self: *const MachBufferFinalized) []const MachTrap {
        return self.traps.items;
    }
};

// =============================================================================
// MachTextSectionBuilder
// =============================================================================

/// Builder for entire text sections (multiple functions).
pub fn MachTextSectionBuilder(comptime LabelUseType: type) type {
    return struct {
        const Self = @This();
        const BufferType = MachBuffer(LabelUseType);

        buf: BufferType,
        next_func: usize,
        force_veneers: ForceVeneers,

        /// Create a new text section builder.
        pub fn init(allocator: Allocator, num_funcs: usize) !Self {
            var buf = BufferType.init(allocator);
            try buf.reserveLabelsForBlocks(num_funcs);
            return .{
                .buf = buf,
                .next_func = 0,
                .force_veneers = .No,
            };
        }

        /// Free resources.
        pub fn deinit(self: *Self) void {
            self.buf.deinit();
        }

        /// Append a function to the text section.
        pub fn append(self: *Self, labeled: bool, func_data: []const u8, alignment: u32) !u64 {
            // Emit island if needed.
            const size: u32 = @intCast(func_data.len);
            if (self.force_veneers == .Yes or self.buf.islandNeeded(size)) {
                try self.buf.emitIslandForced(size);
            }

            try self.buf.alignTo(alignment);
            const pos = self.buf.curOffset();

            if (labeled) {
                try self.buf.bindLabel(MachLabel.fromBlock(BlockIndex.init(self.next_func)));
                self.next_func += 1;
            }

            try self.buf.putData(func_data);
            return pos;
        }

        /// Force veneer generation.
        pub fn forceVeneers(self: *Self) void {
            self.force_veneers = .Yes;
        }

        /// Write data at a specific offset.
        pub fn write(self: *Self, offset: u64, data: []const u8) void {
            const start: usize = @intCast(offset);
            @memcpy(self.buf.data.items[start..][0..data.len], data);
        }

        /// Finish and return the raw data.
        pub fn finish(self: *Self) ![]u8 {
            // Verify all functions were pushed.
            std.debug.assert(self.next_func == self.buf.label_offsets.items.len);

            // Emit final island if needed.
            try self.buf.emitIslandForced(0);

            // Take ownership of data.
            const data = self.buf.data.items;
            self.buf.data = .{};
            return data;
        }
    };
}

// =============================================================================
// Tests
// =============================================================================

test "MachLabel basic operations" {
    const label = MachLabel.init(42);
    try std.testing.expectEqual(@as(u32, 42), label.index);
    try std.testing.expect(label.isValid());
    try std.testing.expect(!MachLabel.INVALID.isValid());
}

test "MachLabel from block" {
    const block = BlockIndex.init(5);
    const label = MachLabel.fromBlock(block);
    try std.testing.expectEqual(@as(u32, 5), label.index);
    try std.testing.expectEqual(@as(usize, 5), label.toBlock().index());
}

// Stub LabelUse type for testing.
const TestLabelUse = struct {
    max_pos_range: CodeOffset,
    max_neg_range: CodeOffset,
    patch_size: usize,
    supports_veneer: bool,

    fn patch(_: TestLabelUse, buffer: []u8, use_offset: CodeOffset, label_offset: CodeOffset) void {
        // Simple 4-byte relative offset patch.
        const rel_offset = @as(i32, @intCast(@as(i64, label_offset) - @as(i64, use_offset)));
        const bytes = std.mem.toBytes(std.mem.nativeToLittle(i32, rel_offset));
        @memcpy(buffer[use_offset..][0..4], &bytes);
    }
};

test "MachBuffer basic emit" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.put1(0x48);
    try buf.put1(0x89);
    try buf.put1(0xc0);

    try std.testing.expectEqual(@as(CodeOffset, 3), buf.curOffset());
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x48, 0x89, 0xc0 }, buf.getData());
}

test "MachBuffer put2/put4/put8" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.put2(0x1234);
    try buf.put4(0x56789ABC);
    try buf.put8(0xDEF0123456789ABC);

    try std.testing.expectEqual(@as(CodeOffset, 14), buf.curOffset());
}

test "MachBuffer label binding" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.reserveLabelsForBlocks(3);

    const label0 = MachLabel.fromBlock(BlockIndex.init(0));
    const label1 = MachLabel.fromBlock(BlockIndex.init(1));

    try buf.bindLabel(label0);
    try buf.put4(0);
    try buf.put4(0);
    try buf.bindLabel(label1);

    try std.testing.expectEqual(@as(CodeOffset, 0), buf.labelOffset(label0));
    try std.testing.expectEqual(@as(CodeOffset, 8), buf.labelOffset(label1));
}

test "MachBuffer alignment" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.put1(0x90);
    try std.testing.expectEqual(@as(CodeOffset, 1), buf.curOffset());

    try buf.alignTo(4);
    try std.testing.expectEqual(@as(CodeOffset, 4), buf.curOffset());

    try buf.alignTo(8);
    try std.testing.expectEqual(@as(CodeOffset, 8), buf.curOffset());
}

test "MachBuffer traps" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.put4(0);
    try buf.addTrap(.HEAP_OUT_OF_BOUNDS);
    try buf.put4(0);
    try buf.addTrap(.INTEGER_OVERFLOW);

    try std.testing.expectEqual(@as(usize, 2), buf.traps.items.len);
    try std.testing.expectEqual(@as(CodeOffset, 4), buf.traps.items[0].offset);
    try std.testing.expectEqual(TrapCode.HEAP_OUT_OF_BOUNDS, buf.traps.items[0].code);
    try std.testing.expectEqual(@as(CodeOffset, 8), buf.traps.items[1].offset);
    try std.testing.expectEqual(TrapCode.INTEGER_OVERFLOW, buf.traps.items[1].code);
}

test "MachBuffer relocations" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.put4(0);
    try buf.addRelocExternalName(.Abs4, .{ .User = UserExternalNameRef.init(0) }, 0);
    try buf.put4(0);
    try buf.addRelocExternalName(.Abs8, .{ .User = UserExternalNameRef.init(1) }, 8);

    try std.testing.expectEqual(@as(usize, 2), buf.relocs.items.len);
    try std.testing.expectEqual(@as(CodeOffset, 4), buf.relocs.items[0].offset);
    try std.testing.expectEqual(Reloc.Abs4, buf.relocs.items[0].kind);
    try std.testing.expectEqual(@as(CodeOffset, 8), buf.relocs.items[1].offset);
    try std.testing.expectEqual(Reloc.Abs8, buf.relocs.items[1].kind);
}

test "MachBuffer source locations" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    buf.startSrcloc(.{ .offset = 100 });
    try buf.put4(0);
    try buf.put4(0);
    try buf.endSrcloc();

    try std.testing.expectEqual(@as(usize, 1), buf.srclocs.items.len);
    try std.testing.expectEqual(@as(CodeOffset, 0), buf.srclocs.items[0].start);
    try std.testing.expectEqual(@as(CodeOffset, 8), buf.srclocs.items[0].end);
    try std.testing.expectEqual(@as(u32, 100), buf.srclocs.items[0].loc.offset);
}

test "MachLabelFixup deadline" {
    const fixup = MachLabelFixup(TestLabelUse){
        .label = MachLabel.init(0),
        .offset = 100,
        .kind = .{
            .max_pos_range = 1000,
            .max_neg_range = 1000,
            .patch_size = 4,
            .supports_veneer = true,
        },
    };

    try std.testing.expectEqual(@as(CodeOffset, 1100), fixup.deadline());
}

test "MachBufferFinalized basic" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    try buf.put1(0x90);
    try buf.put1(0x90);
    try buf.put1(0x90);

    var finalized = try buf.finish();
    defer finalized.deinit();

    try std.testing.expectEqual(@as(CodeOffset, 3), finalized.totalSize());
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x90, 0x90, 0x90 }, finalized.getData());
}

test "MachTextSectionBuilder basic" {
    const allocator = std.testing.allocator;
    var builder = try MachTextSectionBuilder(TestLabelUse).init(allocator, 2);
    defer builder.deinit();

    const func1 = [_]u8{ 0x48, 0x89, 0xc0, 0xc3 };
    const func2 = [_]u8{ 0x48, 0x31, 0xc0, 0xc3 };

    const pos1 = try builder.append(true, &func1, 4);
    const pos2 = try builder.append(true, &func2, 4);

    try std.testing.expectEqual(@as(u64, 0), pos1);
    try std.testing.expectEqual(@as(u64, 4), pos2);
}

test "MachBuffer get new label" {
    const allocator = std.testing.allocator;
    var buf = MachBuffer(TestLabelUse).init(allocator);
    defer buf.deinit();

    const label0 = try buf.getLabel();
    const label1 = try buf.getLabel();
    const label2 = try buf.getLabel();

    try std.testing.expectEqual(@as(u32, 0), label0.index);
    try std.testing.expectEqual(@as(u32, 1), label1.index);
    try std.testing.expectEqual(@as(u32, 2), label2.index);
}
