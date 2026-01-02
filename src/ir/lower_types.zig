//! Lowering Types
//!
//! Standalone types used by the IR lowering process.
//! Extracted from lower.zig following the Ghostty pattern of
//! small, focused type modules.

const std = @import("std");
const ast = @import("../ast/mod.zig");
const ir = @import("ir.zig");

const StmtIdx = ast.StmtIdx;
const StringId = ast.StringId;

/// Errors that can occur during AST to IR lowering
pub const LowerError = error{
    OutOfMemory,
    UndefinedVariable,
    UndefinedType,
    TypeMismatch,
    InvalidExpression,
    UnsupportedFeature,
    UnknownFunction,
    VerificationFailed,
};

/// Generic type definition - stores info needed to instantiate a generic type
pub const GenericDef = struct {
    /// The original statement index (for re-lowering with substitutions)
    stmt_idx: StmtIdx,
    /// Number of type parameters
    type_param_count: u16,
    /// Type parameter names (stored as StringIds)
    type_param_names: []const StringId,
};

/// Key for caching instantiated generics
pub const InstantiationKey = struct {
    base_name: []const u8,
    type_args: []const ir.Type,

    pub fn hash(self: InstantiationKey) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(self.base_name);
        for (self.type_args) |arg| {
            // Hash the type tag
            h.update(std.mem.asBytes(&arg));
        }
        return h.final();
    }

    pub fn eql(a: InstantiationKey, b: InstantiationKey) bool {
        if (!std.mem.eql(u8, a.base_name, b.base_name)) return false;
        if (a.type_args.len != b.type_args.len) return false;
        for (a.type_args, b.type_args) |ta, tb| {
            if (!std.meta.eql(ta, tb)) return false;
        }
        return true;
    }
};

/// Trait method signature
pub const TraitMethodSig = struct {
    name: []const u8,
    param_count: u32,
    return_type: ir.Type,
};

/// Trait definition - stores method signatures for a trait
pub const TraitDef = struct {
    name: []const u8,
    type_param_count: u16,
    methods: []const TraitMethodSig,
};

/// Key for looking up trait implementations
pub const ImplKey = struct {
    trait_name: []const u8,
    type_name: []const u8,

    pub fn hash(self: ImplKey) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(self.trait_name);
        h.update(self.type_name);
        return h.final();
    }

    pub fn eql(a: ImplKey, b: ImplKey) bool {
        return std.mem.eql(u8, a.trait_name, b.trait_name) and
            std.mem.eql(u8, a.type_name, b.type_name);
    }
};

/// Method implementation entry
pub const MethodImpl = struct {
    method_name: []const u8,
    fn_stmt_idx: StmtIdx,
};

/// Hash context for ImplKey in hash maps
pub const ImplKeyContext = struct {
    pub fn hash(_: @This(), key: ImplKey) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: ImplKey, b: ImplKey) bool {
        return a.eql(b);
    }
};
