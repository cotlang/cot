//! Cranelift-style frontend for building CLIF IR.
//!
//! Port of cranelift-frontend crate.
//!
//! This module provides the FunctionBuilder API that allows building CLIF IR
//! with automatic SSA construction. Variables defined with def_var and used
//! with use_var are automatically converted to SSA form.
//!
//! ## Usage
//!
//! ```zig
//! var func = Function.init(allocator);
//! var ctx = FunctionBuilderContext.init(allocator);
//! var builder = FunctionBuilder.init(&func, &ctx);
//!
//! const block0 = try builder.createBlock();
//! builder.switchToBlock(block0);
//! try builder.sealBlock(block0);
//!
//! const x = try builder.declareVar(Type.I32);
//! const val = try builder.ins().iconst(Type.I32, 42);
//! try builder.defVar(x, val);
//!
//! const use_x = try builder.useVar(x);
//! _ = try builder.ins().iadd(use_x, use_x);
//!
//! builder.finalize();
//! ```

pub const frontend = @import("frontend.zig");
pub const ssa = @import("ssa.zig");
pub const variable = @import("variable.zig");

// Re-export main types
pub const FunctionBuilder = frontend.FunctionBuilder;
pub const FunctionBuilderContext = frontend.FunctionBuilderContext;
pub const FuncInstBuilder = frontend.FuncInstBuilder;
pub const Variable = variable.Variable;
pub const SSABuilder = ssa.SSABuilder;
pub const SideEffects = ssa.SideEffects;

// Error types
pub const UseVariableError = frontend.UseVariableError;
pub const DefVariableError = frontend.DefVariableError;

// Re-export CLIF types for convenience
pub const Block = frontend.Block;
pub const Value = frontend.Value;
pub const Inst = frontend.Inst;
pub const Type = frontend.Type;
pub const Function = frontend.Function;
pub const StackSlot = frontend.StackSlot;
pub const StackSlotData = frontend.StackSlotData;
pub const FuncRef = frontend.FuncRef;
pub const SigRef = frontend.SigRef;
pub const Signature = frontend.Signature;
pub const AbiParam = frontend.AbiParam;

test {
    @import("std").testing.refAllDecls(@This());
    _ = frontend;
    _ = ssa;
    _ = variable;
}
