//! Strongly Typed Intermediate Representation

const std = @import("std");
const types = @import("types.zig");
const source = @import("source.zig");

const TypeIndex = types.TypeIndex;
const TypeRegistry = types.TypeRegistry;
const Span = source.Span;
const Pos = source.Pos;
const Allocator = std.mem.Allocator;

// Distinct index types
pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const LocalIdx = u32;
pub const null_local: LocalIdx = std.math.maxInt(LocalIdx);
pub const BlockIndex = u32;
pub const null_block: BlockIndex = std.math.maxInt(BlockIndex);
pub const ParamIdx = u32;
pub const StringIdx = u32;
pub const GlobalIdx = u32;

pub const BinaryOp = enum(u8) {
    add, sub, mul, div, mod, eq, ne, lt, le, gt, ge, @"and", @"or", bit_and, bit_or, bit_xor, shl, shr,
    fmin, fmax,

    pub fn isComparison(self: BinaryOp) bool { return switch (self) { .eq, .ne, .lt, .le, .gt, .ge => true, else => false }; }
    pub fn isArithmetic(self: BinaryOp) bool { return switch (self) { .add, .sub, .mul, .div, .mod => true, else => false }; }
    pub fn isLogical(self: BinaryOp) bool { return switch (self) { .@"and", .@"or" => true, else => false }; }
    pub fn isBitwise(self: BinaryOp) bool { return switch (self) { .bit_and, .bit_or, .bit_xor, .shl, .shr => true, else => false }; }
};

pub const UnaryOp = enum(u8) { neg, not, bit_not, optional_unwrap, abs, ceil, floor, trunc_float, nearest, sqrt, f64_reinterpret_i64, i64_reinterpret_f64 };

// Typed operation payloads
pub const ConstInt = struct { value: i64 };
pub const ConstFloat = struct { value: f64 };
pub const ConstBool = struct { value: bool };
pub const ConstSlice = struct { string_index: StringIdx };
pub const LocalRef = struct { local_idx: LocalIdx };
pub const GlobalRef = struct { global_idx: GlobalIdx, name: []const u8 };
pub const GlobalStore = struct { global_idx: GlobalIdx, name: []const u8, value: NodeIndex };
pub const FuncAddr = struct { name: []const u8 };
pub const Binary = struct { op: BinaryOp, left: NodeIndex, right: NodeIndex };
pub const Unary = struct { op: UnaryOp, operand: NodeIndex };
pub const StoreLocal = struct { local_idx: LocalIdx, value: NodeIndex };
pub const FieldLocal = struct { local_idx: LocalIdx, field_idx: u32, offset: i64 };
pub const StoreLocalField = struct { local_idx: LocalIdx, field_idx: u32, offset: i64, value: NodeIndex };
pub const StoreField = struct { base: NodeIndex, field_idx: u32, offset: i64, value: NodeIndex };
pub const FieldValue = struct { base: NodeIndex, field_idx: u32, offset: i64 };
pub const IndexLocal = struct { local_idx: LocalIdx, index: NodeIndex, elem_size: u32 };
pub const IndexValue = struct { base: NodeIndex, index: NodeIndex, elem_size: u32 };
pub const StoreIndexLocal = struct { local_idx: LocalIdx, index: NodeIndex, value: NodeIndex, elem_size: u32 };
pub const StoreIndexValue = struct { base: NodeIndex, index: NodeIndex, value: NodeIndex, elem_size: u32 };
pub const SliceLocal = struct { local_idx: LocalIdx, start: ?NodeIndex, end: ?NodeIndex, elem_size: u32 };
pub const SliceValue = struct { base: NodeIndex, start: ?NodeIndex, end: ?NodeIndex, elem_size: u32 };
pub const SlicePtr = struct { slice: NodeIndex };
pub const SliceLen = struct { slice: NodeIndex };
pub const SliceCap = struct { slice: NodeIndex };  // Go's slice.cap field
pub const PtrLoad = struct { ptr_local: LocalIdx };
pub const PtrStore = struct { ptr_local: LocalIdx, value: NodeIndex };
pub const PtrField = struct { ptr_local: LocalIdx, field_idx: u32, offset: i64 };
pub const PtrFieldStore = struct { ptr_local: LocalIdx, field_idx: u32, offset: i64, value: NodeIndex };
pub const PtrLoadValue = struct { ptr: NodeIndex };
pub const PtrStoreValue = struct { ptr: NodeIndex, value: NodeIndex };
pub const AddrLocal = struct { local_idx: LocalIdx };
pub const AddrOffset = struct { base: NodeIndex, offset: i64 };
pub const AddrIndex = struct { base: NodeIndex, index: NodeIndex, elem_size: u32 };
pub const Call = struct { func_name: []const u8, args: []const NodeIndex, is_builtin: bool };
pub const CallIndirect = struct { callee: NodeIndex, args: []const NodeIndex };
pub const ClosureCall = struct { callee: NodeIndex, context: NodeIndex, args: []const NodeIndex };
pub const Return = struct { value: ?NodeIndex };
pub const Jump = struct { target: BlockIndex };
pub const Branch = struct { condition: NodeIndex, then_block: BlockIndex, else_block: BlockIndex };
pub const PhiSource = struct { block: BlockIndex, value: NodeIndex };
pub const Phi = struct { sources: []const PhiSource };
pub const Select = struct { condition: NodeIndex, then_value: NodeIndex, else_value: NodeIndex };
pub const Convert = struct { operand: NodeIndex, from_type: TypeIndex, to_type: TypeIndex };
pub const ListNew = struct { elem_type: TypeIndex };
pub const ListPush = struct { handle: NodeIndex, value: NodeIndex };
pub const ListGet = struct { handle: NodeIndex, index: NodeIndex };
pub const ListSet = struct { handle: NodeIndex, index: NodeIndex, value: NodeIndex };
pub const ListLen = struct { handle: NodeIndex };
pub const MapNew = struct { key_type: TypeIndex, value_type: TypeIndex };
pub const MapSet = struct { handle: NodeIndex, key: NodeIndex, value: NodeIndex };
pub const MapGet = struct { handle: NodeIndex, key: NodeIndex };
pub const MapHas = struct { handle: NodeIndex, key: NodeIndex };
pub const StrConcat = struct { left: NodeIndex, right: NodeIndex };
pub const StringHeader = struct { ptr: NodeIndex, len: NodeIndex };
pub const SliceHeader = struct { ptr: NodeIndex, len: NodeIndex, cap: NodeIndex };  // Matches Go's slice struct
pub const UnionInit = struct { variant_idx: u32, payload: ?NodeIndex };
pub const UnionTag = struct { value: NodeIndex };
pub const UnionPayload = struct { variant_idx: u32, value: NodeIndex };
pub const PtrCast = struct { operand: NodeIndex };
pub const IntToPtr = struct { operand: NodeIndex };
pub const PtrToInt = struct { operand: NodeIndex };
pub const TypeMetadata = struct { type_name: []const u8 };
pub const WasmGlobalRead = struct { global_idx: u32 };

// WasmGC struct operations
pub const GcStructNew = struct { type_name: []const u8, field_values: []const NodeIndex };
pub const GcStructGet = struct { base: NodeIndex, type_name: []const u8, field_idx: u32 };
pub const GcStructSet = struct { base: NodeIndex, type_name: []const u8, field_idx: u32, value: NodeIndex };

pub const Node = struct {
    type_idx: TypeIndex,
    span: Span,
    block: BlockIndex,
    data: Data,

    pub const Data = union(enum) {
        const_int: ConstInt, const_float: ConstFloat, const_bool: ConstBool, const_null: void, const_slice: ConstSlice,
        local_ref: LocalRef, global_ref: GlobalRef, global_store: GlobalStore, func_addr: FuncAddr,
        addr_local: AddrLocal, addr_global: GlobalRef, load_local: LocalRef, store_local: StoreLocal,
        binary: Binary, unary: Unary,
        field_local: FieldLocal, store_local_field: StoreLocalField, store_field: StoreField, field_value: FieldValue,
        index_local: IndexLocal, index_value: IndexValue, store_index_local: StoreIndexLocal, store_index_value: StoreIndexValue,
        slice_local: SliceLocal, slice_value: SliceValue, slice_ptr: SlicePtr, slice_len: SliceLen, slice_cap: SliceCap,
        ptr_load: PtrLoad, ptr_store: PtrStore, ptr_field: PtrField, ptr_field_store: PtrFieldStore,
        ptr_load_value: PtrLoadValue, ptr_store_value: PtrStoreValue,
        addr_offset: AddrOffset, addr_index: AddrIndex,
        call: Call, call_indirect: CallIndirect, closure_call: ClosureCall, ret: Return, jump: Jump, branch: Branch, phi: Phi, select: Select,
        convert: Convert,
        list_new: ListNew, list_push: ListPush, list_get: ListGet, list_set: ListSet, list_len: ListLen, list_free: ListLen,
        map_new: MapNew, map_set: MapSet, map_get: MapGet, map_has: MapHas, map_free: ListLen,
        str_concat: StrConcat, string_header: StringHeader, slice_header: SliceHeader,
        union_init: UnionInit, union_tag: UnionTag, union_payload: UnionPayload,
        ptr_cast: PtrCast, int_to_ptr: IntToPtr, ptr_to_int: PtrToInt,
        type_metadata: TypeMetadata,
        wasm_global_read: WasmGlobalRead,
        gc_struct_new: GcStructNew,
        gc_struct_get: GcStructGet,
        gc_struct_set: GcStructSet,
        nop: void,
        trap: void,
    };

    pub fn init(data: Data, type_idx: TypeIndex, span: Span) Node { return .{ .type_idx = type_idx, .span = span, .block = null_block, .data = data }; }
    pub fn withBlock(self: Node, block: BlockIndex) Node { var n = self; n.block = block; return n; }
    pub fn isTerminator(self: *const Node) bool { return switch (self.data) { .ret, .jump, .branch, .trap => true, else => false }; }
    pub fn hasSideEffects(self: *const Node) bool { return switch (self.data) { .store_local, .ptr_store, .ptr_store_value, .ptr_field_store, .store_local_field, .call, .call_indirect, .closure_call, .ret, .jump, .branch, .trap, .list_new, .list_push, .list_set, .list_free, .map_new, .map_set, .map_free, .gc_struct_new, .gc_struct_set => true, else => false }; }
    pub fn isConstant(self: *const Node) bool { return switch (self.data) { .const_int, .const_float, .const_bool, .const_null, .const_slice => true, else => false }; }
};

pub const Block = struct {
    index: BlockIndex,
    preds: []BlockIndex = &.{},
    succs: []BlockIndex = &.{},
    nodes: []NodeIndex = &.{},
    label: []const u8 = "",

    pub fn init(index: BlockIndex) Block { return .{ .index = index }; }
};

pub const Local = struct {
    name: []const u8,
    type_idx: TypeIndex,
    mutable: bool,
    is_param: bool = false,
    param_idx: ParamIdx = 0,
    size: u32 = 8,
    alignment: u32 = 8,
    offset: i32 = 0,

    pub fn init(name: []const u8, type_idx: TypeIndex, mutable: bool) Local { return .{ .name = name, .type_idx = type_idx, .mutable = mutable }; }
    pub fn initParam(name: []const u8, type_idx: TypeIndex, param_idx: ParamIdx, size: u32) Local { return .{ .name = name, .type_idx = type_idx, .mutable = false, .is_param = true, .param_idx = param_idx, .size = size, .alignment = @min(size, 8) }; }
    pub fn initWithSize(name: []const u8, type_idx: TypeIndex, mutable: bool, size: u32) Local { return .{ .name = name, .type_idx = type_idx, .mutable = mutable, .size = size, .alignment = @min(size, 8) }; }
};

pub const Func = struct {
    name: []const u8,
    type_idx: TypeIndex,
    return_type: TypeIndex,
    params: []const Local,
    locals: []const Local,
    blocks: []const Block,
    entry: BlockIndex = 0,
    nodes: []const Node,
    span: Span,
    frame_size: i32 = 0,
    string_literals: []const []const u8 = &.{},
    is_destructor: bool = false,

    pub fn getNode(self: *const Func, idx: NodeIndex) *const Node { return &self.nodes[idx]; }
    pub fn getLocal(self: *const Func, idx: LocalIdx) *const Local { return &self.locals[idx]; }
    pub fn getBlock(self: *const Func, idx: BlockIndex) *const Block { return &self.blocks[idx]; }
};

pub const FuncBuilder = struct {
    allocator: Allocator,
    name: []const u8,
    type_idx: TypeIndex,
    return_type: TypeIndex,
    span: Span,
    locals: std.ArrayListUnmanaged(Local) = .{},
    blocks: std.ArrayListUnmanaged(Block) = .{},
    nodes: std.ArrayListUnmanaged(Node) = .{},
    string_literals: std.ArrayListUnmanaged([]const u8) = .{},
    current_block: BlockIndex = 0,
    local_map: std.StringHashMap(LocalIdx),
    shadow_stack: std.ArrayListUnmanaged(ShadowEntry) = .{},
    /// Original return type for SRET functions (callee uses this to know
    /// what type to write to the __sret pointer). null if not SRET.
    sret_return_type: ?TypeIndex = null,
    /// Set for impl methods named "deinit" — used by driver.zig to register ARC destructors
    /// without scanning function names (which false-positives on generic methods).
    is_destructor: bool = false,

    const ShadowEntry = struct { name: []const u8, old_idx: ?LocalIdx };

    pub fn init(allocator: Allocator, name: []const u8, type_idx: TypeIndex, return_type: TypeIndex, span: Span) FuncBuilder {
        var fb = FuncBuilder{ .allocator = allocator, .name = name, .type_idx = type_idx, .return_type = return_type, .span = span, .local_map = std.StringHashMap(LocalIdx).init(allocator) };
        fb.blocks.append(allocator, Block.init(0)) catch {};
        return fb;
    }

    pub fn deinit(self: *FuncBuilder) void {
        self.locals.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.string_literals.deinit(self.allocator);
        self.local_map.deinit();
        self.shadow_stack.deinit(self.allocator);
    }

    pub fn addLocal(self: *FuncBuilder, local: Local) !LocalIdx {
        const idx: LocalIdx = @intCast(self.locals.items.len);
        try self.locals.append(self.allocator, local);
        try self.local_map.put(local.name, idx);
        return idx;
    }

    pub fn addParam(self: *FuncBuilder, name: []const u8, type_idx: TypeIndex, size: u32) !LocalIdx {
        const idx: LocalIdx = @intCast(self.locals.items.len);
        try self.locals.append(self.allocator, Local.initParam(name, type_idx, idx, size));
        try self.local_map.put(name, idx);
        return idx;
    }

    pub fn addLocalWithSize(self: *FuncBuilder, name: []const u8, type_idx: TypeIndex, mutable: bool, size: u32) !LocalIdx {
        const idx: LocalIdx = @intCast(self.locals.items.len);
        try self.locals.append(self.allocator, Local.initWithSize(name, type_idx, mutable, size));
        try self.shadow_stack.append(self.allocator, .{ .name = name, .old_idx = self.local_map.get(name) });
        try self.local_map.put(name, idx);
        return idx;
    }

    pub fn lookupLocal(self: *const FuncBuilder, name: []const u8) ?LocalIdx { return self.local_map.get(name); }
    pub fn markScopeEntry(self: *const FuncBuilder) usize { return self.shadow_stack.items.len; }

    pub fn restoreScope(self: *FuncBuilder, entry_depth: usize) void {
        while (self.shadow_stack.items.len > entry_depth) {
            const entry = self.shadow_stack.pop() orelse break;
            if (entry.old_idx) |old| self.local_map.put(entry.name, old) catch {} else _ = self.local_map.remove(entry.name);
        }
    }

    pub fn newBlock(self: *FuncBuilder, label: []const u8) !BlockIndex {
        const idx: BlockIndex = @intCast(self.blocks.items.len);
        var block = Block.init(idx);
        block.label = label;
        try self.blocks.append(self.allocator, block);
        return idx;
    }

    pub fn setBlock(self: *FuncBuilder, block: BlockIndex) void { self.current_block = block; }
    pub fn currentBlock(self: *const FuncBuilder) BlockIndex { return self.current_block; }

    pub fn needsTerminator(self: *const FuncBuilder) bool {
        var last: ?NodeIndex = null;
        for (self.nodes.items, 0..) |node, i| if (node.block == self.current_block) { last = @intCast(i); };
        return if (last) |idx| !self.nodes.items[idx].isTerminator() else true;
    }

    pub fn addStringLiteral(self: *FuncBuilder, str: []const u8) !StringIdx {
        for (self.string_literals.items, 0..) |existing, i| if (std.mem.eql(u8, existing, str)) return @intCast(i);
        const idx: StringIdx = @intCast(self.string_literals.items.len);
        try self.string_literals.append(self.allocator, str);
        return idx;
    }

    pub fn emit(self: *FuncBuilder, node: Node) !NodeIndex {
        const idx: NodeIndex = @intCast(self.nodes.items.len);
        var n = node;
        n.block = self.current_block;
        try self.nodes.append(self.allocator, n);
        var block = &self.blocks.items[self.current_block];
        var nodes_list = std.ArrayListUnmanaged(NodeIndex){};
        defer nodes_list.deinit(self.allocator);
        for (block.nodes) |ni| try nodes_list.append(self.allocator, ni);
        try nodes_list.append(self.allocator, idx);
        if (block.nodes.len > 0) self.allocator.free(block.nodes);
        block.nodes = try nodes_list.toOwnedSlice(self.allocator);
        return idx;
    }

    // Emit convenience methods
    pub fn emitConstInt(self: *FuncBuilder, value: i64, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .const_int = .{ .value = value } }, type_idx, span)); }
    pub fn emitConstFloat(self: *FuncBuilder, value: f64, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .const_float = .{ .value = value } }, type_idx, span)); }
    pub fn emitConstBool(self: *FuncBuilder, value: bool, span: Span) !NodeIndex { return self.emit(Node.init(.{ .const_bool = .{ .value = value } }, TypeRegistry.BOOL, span)); }
    pub fn emitConstNull(self: *FuncBuilder, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .const_null = {} }, type_idx, span)); }
    pub fn emitConstSlice(self: *FuncBuilder, string_index: StringIdx, span: Span) !NodeIndex { return self.emit(Node.init(.{ .const_slice = .{ .string_index = string_index } }, TypeRegistry.STRING, span)); }
    pub fn emitFuncAddr(self: *FuncBuilder, name: []const u8, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .func_addr = .{ .name = name } }, type_idx, span)); }
    pub fn emitGlobalRef(self: *FuncBuilder, global_idx: GlobalIdx, name: []const u8, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .global_ref = .{ .global_idx = global_idx, .name = name } }, type_idx, span)); }
    pub fn emitGlobalStore(self: *FuncBuilder, global_idx: GlobalIdx, name: []const u8, value: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .global_store = .{ .global_idx = global_idx, .name = name, .value = value } }, TypeRegistry.VOID, span)); }
    pub fn emitLoadLocal(self: *FuncBuilder, local_idx: LocalIdx, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .load_local = .{ .local_idx = local_idx } }, type_idx, span)); }
    pub fn emitStoreLocal(self: *FuncBuilder, local_idx: LocalIdx, value: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .store_local = .{ .local_idx = local_idx, .value = value } }, TypeRegistry.VOID, span)); }
    pub fn emitAddrLocal(self: *FuncBuilder, local_idx: LocalIdx, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .addr_local = .{ .local_idx = local_idx } }, type_idx, span)); }
    pub fn emitAddrGlobal(self: *FuncBuilder, global_idx: GlobalIdx, name: []const u8, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .addr_global = .{ .global_idx = global_idx, .name = name } }, type_idx, span)); }
    pub fn emitAddrOffset(self: *FuncBuilder, base: NodeIndex, offset: i64, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .addr_offset = .{ .base = base, .offset = offset } }, type_idx, span)); }
    pub fn emitBinary(self: *FuncBuilder, op: BinaryOp, left: NodeIndex, right: NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .binary = .{ .op = op, .left = left, .right = right } }, type_idx, span)); }
    pub fn emitUnary(self: *FuncBuilder, op: UnaryOp, operand: NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .unary = .{ .op = op, .operand = operand } }, type_idx, span)); }
    pub fn emitFieldLocal(self: *FuncBuilder, local_idx: LocalIdx, field_idx: u32, offset: i64, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .field_local = .{ .local_idx = local_idx, .field_idx = field_idx, .offset = offset } }, type_idx, span)); }
    pub fn emitStoreLocalField(self: *FuncBuilder, local_idx: LocalIdx, field_idx: u32, offset: i64, value: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .store_local_field = .{ .local_idx = local_idx, .field_idx = field_idx, .offset = offset, .value = value } }, TypeRegistry.VOID, span)); }
    pub fn emitStoreField(self: *FuncBuilder, base: NodeIndex, field_idx: u32, offset: i64, value: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .store_field = .{ .base = base, .field_idx = field_idx, .offset = offset, .value = value } }, TypeRegistry.VOID, span)); }
    pub fn emitFieldValue(self: *FuncBuilder, base: NodeIndex, field_idx: u32, offset: i64, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .field_value = .{ .base = base, .field_idx = field_idx, .offset = offset } }, type_idx, span)); }
    pub fn emitIndexLocal(self: *FuncBuilder, local_idx: LocalIdx, index: NodeIndex, elem_size: u32, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .index_local = .{ .local_idx = local_idx, .index = index, .elem_size = elem_size } }, type_idx, span)); }
    pub fn emitIndexValue(self: *FuncBuilder, base: NodeIndex, index: NodeIndex, elem_size: u32, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .index_value = .{ .base = base, .index = index, .elem_size = elem_size } }, type_idx, span)); }
    pub fn emitStoreIndexLocal(self: *FuncBuilder, local_idx: LocalIdx, index: NodeIndex, value: NodeIndex, elem_size: u32, span: Span) !NodeIndex { return self.emit(Node.init(.{ .store_index_local = .{ .local_idx = local_idx, .index = index, .value = value, .elem_size = elem_size } }, TypeRegistry.VOID, span)); }
    pub fn emitStoreIndexValue(self: *FuncBuilder, base: NodeIndex, index: NodeIndex, value: NodeIndex, elem_size: u32, span: Span) !NodeIndex { return self.emit(Node.init(.{ .store_index_value = .{ .base = base, .index = index, .value = value, .elem_size = elem_size } }, TypeRegistry.VOID, span)); }
    pub fn emitAddrIndex(self: *FuncBuilder, base: NodeIndex, index: NodeIndex, elem_size: u32, ptr_type: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .addr_index = .{ .base = base, .index = index, .elem_size = elem_size } }, ptr_type, span)); }
    pub fn emitSliceLocal(self: *FuncBuilder, local_idx: LocalIdx, start: ?NodeIndex, end: ?NodeIndex, elem_size: u32, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .slice_local = .{ .local_idx = local_idx, .start = start, .end = end, .elem_size = elem_size } }, type_idx, span)); }
    pub fn emitSliceValue(self: *FuncBuilder, base: NodeIndex, start: ?NodeIndex, end: ?NodeIndex, elem_size: u32, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .slice_value = .{ .base = base, .start = start, .end = end, .elem_size = elem_size } }, type_idx, span)); }
    pub fn emitSlicePtr(self: *FuncBuilder, slice: NodeIndex, ptr_type: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .slice_ptr = .{ .slice = slice } }, ptr_type, span)); }
    pub fn emitSliceLen(self: *FuncBuilder, slice: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .slice_len = .{ .slice = slice } }, TypeRegistry.I64, span)); }
    pub fn emitSliceCap(self: *FuncBuilder, slice: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .slice_cap = .{ .slice = slice } }, TypeRegistry.I64, span)); }
    pub fn emitPtrLoad(self: *FuncBuilder, ptr_local: LocalIdx, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ptr_load = .{ .ptr_local = ptr_local } }, type_idx, span)); }
    pub fn emitPtrStore(self: *FuncBuilder, ptr_local: LocalIdx, value: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ptr_store = .{ .ptr_local = ptr_local, .value = value } }, TypeRegistry.VOID, span)); }
    pub fn emitPtrLoadValue(self: *FuncBuilder, ptr: NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ptr_load_value = .{ .ptr = ptr } }, type_idx, span)); }
    pub fn emitPtrStoreValue(self: *FuncBuilder, ptr: NodeIndex, value: NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ptr_store_value = .{ .ptr = ptr, .value = value } }, TypeRegistry.VOID, span)); }

    pub fn emitCall(self: *FuncBuilder, func_name: []const u8, args: []const NodeIndex, is_builtin: bool, type_idx: TypeIndex, span: Span) !NodeIndex {
        return self.emit(Node.init(.{ .call = .{ .func_name = func_name, .args = try self.allocator.dupe(NodeIndex, args), .is_builtin = is_builtin } }, type_idx, span));
    }

    pub fn emitCallIndirect(self: *FuncBuilder, callee: NodeIndex, args: []const NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex {
        return self.emit(Node.init(.{ .call_indirect = .{ .callee = callee, .args = try self.allocator.dupe(NodeIndex, args) } }, type_idx, span));
    }

    pub fn emitClosureCall(self: *FuncBuilder, callee: NodeIndex, context: NodeIndex, args: []const NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex {
        return self.emit(Node.init(.{ .closure_call = .{ .callee = callee, .context = context, .args = try self.allocator.dupe(NodeIndex, args) } }, type_idx, span));
    }

    pub fn emitRet(self: *FuncBuilder, value: ?NodeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ret = .{ .value = value } }, TypeRegistry.VOID, span)); }
    pub fn emitJump(self: *FuncBuilder, target: BlockIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .jump = .{ .target = target } }, TypeRegistry.VOID, span)); }
    pub fn emitBranch(self: *FuncBuilder, condition: NodeIndex, then_block: BlockIndex, else_block: BlockIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .branch = .{ .condition = condition, .then_block = then_block, .else_block = else_block } }, TypeRegistry.VOID, span)); }
    pub fn emitSelect(self: *FuncBuilder, condition: NodeIndex, then_value: NodeIndex, else_value: NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .select = .{ .condition = condition, .then_value = then_value, .else_value = else_value } }, type_idx, span)); }
    pub fn emitConvert(self: *FuncBuilder, operand: NodeIndex, from_type: TypeIndex, to_type: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .convert = .{ .operand = operand, .from_type = from_type, .to_type = to_type } }, to_type, span)); }
    pub fn emitNop(self: *FuncBuilder, span: Span) !NodeIndex { return self.emit(Node.init(.{ .nop = {} }, TypeRegistry.VOID, span)); }
    pub fn emitTrap(self: *FuncBuilder, span: Span) !NodeIndex { return self.emit(Node.init(.{ .trap = {} }, TypeRegistry.VOID, span)); }

    // Aliases and additional helpers
    pub fn emitIndirectCall(self: *FuncBuilder, callee: NodeIndex, args: []const NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emitCallIndirect(callee, args, type_idx, span); }
    pub fn emitStoreFieldValue(self: *FuncBuilder, base: NodeIndex, field_idx: u32, offset: i64, value: NodeIndex, span: Span) !NodeIndex { return self.emitStoreField(base, field_idx, offset, value, span); }
    pub fn emitIntCast(self: *FuncBuilder, operand: NodeIndex, target_type: TypeIndex, span: Span) !NodeIndex {
        const from_type = self.nodes.items[operand].type_idx;
        return self.emitConvert(operand, from_type, target_type, span);
    }
    pub fn emitPtrCast(self: *FuncBuilder, operand: NodeIndex, target_type: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ptr_cast = .{ .operand = operand } }, target_type, span)); }
    pub fn emitIntToPtr(self: *FuncBuilder, operand: NodeIndex, target_type: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .int_to_ptr = .{ .operand = operand } }, target_type, span)); }
    pub fn emitPtrToInt(self: *FuncBuilder, operand: NodeIndex, target_type: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .ptr_to_int = .{ .operand = operand } }, target_type, span)); }
    pub fn emitMakeSlice(self: *FuncBuilder, base_ptr: NodeIndex, start: ?NodeIndex, end: NodeIndex, elem_size: u32, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emitSliceValue(base_ptr, start, end, elem_size, type_idx, span); }
    pub fn emitTypeMetadata(self: *FuncBuilder, type_name: []const u8, span: Span) !NodeIndex { return self.emit(Node.init(.{ .type_metadata = .{ .type_name = type_name } }, TypeRegistry.I64, span)); }
    pub fn emitUnionInit(self: *FuncBuilder, variant_idx: u32, payload: ?NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .union_init = .{ .variant_idx = variant_idx, .payload = payload } }, type_idx, span)); }
    pub fn emitWasmGlobalRead(self: *FuncBuilder, global_idx: u32, type_idx: TypeIndex, span: Span) !NodeIndex { return self.emit(Node.init(.{ .wasm_global_read = .{ .global_idx = global_idx } }, type_idx, span)); }

    // WasmGC struct operations
    pub fn emitGcStructNew(self: *FuncBuilder, type_name: []const u8, field_values: []const NodeIndex, type_idx: TypeIndex, span: Span) !NodeIndex {
        return self.emit(Node.init(.{ .gc_struct_new = .{ .type_name = type_name, .field_values = try self.allocator.dupe(NodeIndex, field_values) } }, type_idx, span));
    }
    pub fn emitGcStructGet(self: *FuncBuilder, base: NodeIndex, type_name: []const u8, field_idx: u32, type_idx: TypeIndex, span: Span) !NodeIndex {
        return self.emit(Node.init(.{ .gc_struct_get = .{ .base = base, .type_name = type_name, .field_idx = field_idx } }, type_idx, span));
    }
    pub fn emitGcStructSet(self: *FuncBuilder, base: NodeIndex, type_name: []const u8, field_idx: u32, value: NodeIndex, span: Span) !NodeIndex {
        return self.emit(Node.init(.{ .gc_struct_set = .{ .base = base, .type_name = type_name, .field_idx = field_idx, .value = value } }, TypeRegistry.VOID, span));
    }

    pub fn build(self: *FuncBuilder) !Func {
        var params = std.ArrayListUnmanaged(Local){};
        defer params.deinit(self.allocator);
        for (self.locals.items) |local| if (local.is_param) try params.append(self.allocator, local);

        var frame_offset: i32 = 0;
        for (self.locals.items) |*local| {
            const local_align: i32 = @intCast(local.alignment);
            frame_offset = (frame_offset + local_align - 1) & ~(local_align - 1);
            local.offset = -frame_offset - @as(i32, @intCast(local.size));
            frame_offset += @as(i32, @intCast(local.size));
        }

        return Func{
            .name = self.name,
            .type_idx = self.type_idx,
            .return_type = self.return_type,
            .params = try self.allocator.dupe(Local, params.items),
            .locals = try self.locals.toOwnedSlice(self.allocator),
            .blocks = try self.blocks.toOwnedSlice(self.allocator),
            .nodes = try self.nodes.toOwnedSlice(self.allocator),
            .span = self.span,
            .frame_size = (frame_offset + 96 + 15) & ~@as(i32, 15),
            .string_literals = try self.string_literals.toOwnedSlice(self.allocator),
            .is_destructor = self.is_destructor,
        };
    }
};

pub const Global = struct {
    name: []const u8,
    type_idx: TypeIndex,
    is_const: bool,
    span: Span,
    size: u32 = 8,

    pub fn init(name: []const u8, type_idx: TypeIndex, is_const: bool, span: Span) Global { return .{ .name = name, .type_idx = type_idx, .is_const = is_const, .span = span }; }
    pub fn initWithSize(name: []const u8, type_idx: TypeIndex, is_const: bool, span: Span, size: u32) Global { return .{ .name = name, .type_idx = type_idx, .is_const = is_const, .span = span, .size = size }; }
};

pub const StructDef = struct { name: []const u8, type_idx: TypeIndex, span: Span };

pub const IR = struct {
    funcs: []const Func = &.{},
    globals: []const Global = &.{},
    structs: []const StructDef = &.{},
    types: *TypeRegistry,
    allocator: Allocator,

    pub fn init(allocator: Allocator, type_reg: *TypeRegistry) IR { return .{ .types = type_reg, .allocator = allocator }; }

    pub fn deinit(self: *IR) void {
        for (self.funcs) |*f| {
            for (f.blocks) |*block| if (block.nodes.len > 0) self.allocator.free(block.nodes);
            if (f.params.len > 0) self.allocator.free(f.params);
            if (f.locals.len > 0) self.allocator.free(f.locals);
            if (f.blocks.len > 0) self.allocator.free(f.blocks);
            if (f.nodes.len > 0) self.allocator.free(f.nodes);
            if (f.string_literals.len > 0) self.allocator.free(f.string_literals);
        }
        if (self.funcs.len > 0) self.allocator.free(self.funcs);
        if (self.globals.len > 0) self.allocator.free(self.globals);
        if (self.structs.len > 0) self.allocator.free(self.structs);
    }

    pub fn getFunc(self: *const IR, name: []const u8) ?*const Func {
        for (self.funcs) |*f| if (std.mem.eql(u8, f.name, name)) return f;
        return null;
    }

    pub fn getGlobal(self: *const IR, name: []const u8) ?*const Global {
        for (self.globals) |*g| if (std.mem.eql(u8, g.name, name)) return g;
        return null;
    }
};

pub const Builder = struct {
    ir: IR,
    allocator: Allocator,
    current_func: ?FuncBuilder = null,
    funcs: std.ArrayListUnmanaged(Func) = .{},
    globals: std.ArrayListUnmanaged(Global) = .{},
    structs: std.ArrayListUnmanaged(StructDef) = .{},

    pub fn init(allocator: Allocator, type_reg: *TypeRegistry) Builder { return .{ .ir = IR.init(allocator, type_reg), .allocator = allocator }; }

    pub fn deinit(self: *Builder) void {
        self.funcs.deinit(self.allocator);
        self.globals.deinit(self.allocator);
        self.structs.deinit(self.allocator);
        if (self.current_func) |*fb| fb.deinit();
    }

    pub fn startFunc(self: *Builder, name: []const u8, type_idx: TypeIndex, return_type: TypeIndex, span: Span) void { self.current_func = FuncBuilder.init(self.allocator, name, type_idx, return_type, span); }
    pub fn func(self: *Builder) ?*FuncBuilder { return if (self.current_func) |*fb| fb else null; }

    pub fn endFunc(self: *Builder) !void {
        if (self.current_func) |*fb| {
            try self.funcs.append(self.allocator, try fb.build());
            self.current_func = null;
        }
    }

    pub fn addGlobal(self: *Builder, g: Global) !void { try self.globals.append(self.allocator, g); }

    pub fn lookupGlobal(self: *const Builder, name: []const u8) ?struct { idx: GlobalIdx, global: Global } {
        for (self.globals.items, 0..) |g, idx| if (std.mem.eql(u8, g.name, name)) return .{ .idx = @intCast(idx), .global = g };
        return null;
    }

    pub fn addStruct(self: *Builder, s: StructDef) !void { try self.structs.append(self.allocator, s); }

    pub fn getIR(self: *Builder) !IR {
        self.ir.funcs = try self.funcs.toOwnedSlice(self.allocator);
        self.ir.globals = try self.globals.toOwnedSlice(self.allocator);
        self.ir.structs = try self.structs.toOwnedSlice(self.allocator);
        return self.ir;
    }
};

pub fn debugPrintNode(node: *const Node, writer: anytype) !void {
    switch (node.data) {
        .const_int => |c| try writer.print("const_int {d}", .{c.value}),
        .const_float => |c| try writer.print("const_float {d}", .{c.value}),
        .const_bool => |c| try writer.print("const_bool {}", .{c.value}),
        .const_null => try writer.print("const_null", .{}),
        .load_local => |l| try writer.print("load_local local={d}", .{l.local_idx}),
        .store_local => |s| try writer.print("store_local local={d} value={d}", .{ s.local_idx, s.value }),
        .binary => |b| try writer.print("binary {s} left={d} right={d}", .{ @tagName(b.op), b.left, b.right }),
        .unary => |u| try writer.print("unary {s} operand={d}", .{ @tagName(u.op), u.operand }),
        .call => |c| try writer.print("call {s}", .{c.func_name}),
        .ret => |r| if (r.value) |v| try writer.print("ret value={d}", .{v}) else try writer.print("ret void", .{}),
        .jump => |j| try writer.print("jump block={d}", .{j.target}),
        .branch => |b| try writer.print("branch cond={d} then={d} else={d}", .{ b.condition, b.then_block, b.else_block }),
        else => try writer.print("{s}", .{@tagName(node.data)}),
    }
}

// ============================================================================
// Tests
// ============================================================================

test "strongly typed node creation" {
    const int_node = Node.init(.{ .const_int = .{ .value = 42 } }, TypeRegistry.INT, Span.fromPos(Pos.zero));
    try std.testing.expectEqual(@as(i64, 42), int_node.data.const_int.value);
    const add_node = Node.init(.{ .binary = .{ .op = .add, .left = 0, .right = 1 } }, TypeRegistry.INT, Span.fromPos(Pos.zero));
    try std.testing.expectEqual(BinaryOp.add, add_node.data.binary.op);
}

test "binary op predicates" {
    try std.testing.expect(BinaryOp.add.isArithmetic());
    try std.testing.expect(BinaryOp.eq.isComparison());
    try std.testing.expect(BinaryOp.@"and".isLogical());
    try std.testing.expect(BinaryOp.bit_and.isBitwise());
}

test "node properties" {
    const ret_node = Node.init(.{ .ret = .{ .value = null } }, TypeRegistry.VOID, Span.fromPos(Pos.zero));
    try std.testing.expect(ret_node.isTerminator());
    try std.testing.expect(ret_node.hasSideEffects());
    const const_node = Node.init(.{ .const_int = .{ .value = 42 } }, TypeRegistry.INT, Span.fromPos(Pos.zero));
    try std.testing.expect(const_node.isConstant());
}

test "function builder basic" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var fb = FuncBuilder.init(arena.allocator(), "test", 0, TypeRegistry.INT, Span.fromPos(Pos.zero));
    const local_idx = try fb.addLocal(Local.init("x", TypeRegistry.INT, true));
    try std.testing.expectEqual(@as(LocalIdx, 0), local_idx);
    const block_idx = try fb.newBlock("then");
    try std.testing.expectEqual(@as(BlockIndex, 1), block_idx);
    fb.setBlock(block_idx);
    const const_node = try fb.emitConstInt(42, TypeRegistry.INT, Span.fromPos(Pos.zero));
    try std.testing.expectEqual(@as(NodeIndex, 0), const_node);
    _ = try fb.emitRet(const_node, Span.fromPos(Pos.zero));
    const f = try fb.build();
    try std.testing.expectEqual(@as(usize, 1), f.locals.len);
    try std.testing.expectEqual(@as(usize, 2), f.nodes.len);
}

test "function builder with parameters" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var fb = FuncBuilder.init(arena.allocator(), "add", 0, TypeRegistry.INT, Span.fromPos(Pos.zero));
    const a = try fb.addParam("a", TypeRegistry.INT, 8);
    const b = try fb.addParam("b", TypeRegistry.INT, 8);
    try std.testing.expectEqual(a, fb.lookupLocal("a").?);
    try std.testing.expectEqual(b, fb.lookupLocal("b").?);
    const f = try fb.build();
    try std.testing.expectEqual(@as(usize, 2), f.params.len);
}

test "IR builder" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var type_reg = try TypeRegistry.init(arena.allocator());
    var builder = Builder.init(arena.allocator(), &type_reg);
    defer builder.deinit();
    builder.startFunc("main", 0, TypeRegistry.INT, Span.fromPos(Pos.zero));
    if (builder.func()) |fb| {
        _ = try fb.emitConstInt(42, TypeRegistry.INT, Span.fromPos(Pos.zero));
        _ = try fb.emitRet(0, Span.fromPos(Pos.zero));
    }
    try builder.endFunc();
    try builder.addGlobal(Global.init("counter", TypeRegistry.INT, false, Span.fromPos(Pos.zero)));
    const ir = try builder.getIR();
    try std.testing.expectEqual(@as(usize, 1), ir.funcs.len);
    try std.testing.expect(ir.getFunc("main") != null);
}

test "local variable layout" {
    const local = Local.initWithSize("x", TypeRegistry.INT, true, 8);
    try std.testing.expectEqual(@as(u32, 8), local.size);
    try std.testing.expect(local.mutable);
    const param = Local.initParam("arg", TypeRegistry.I32, 0, 4);
    try std.testing.expect(param.is_param);
}
