# Wasm→CLIF Translator Port Plan

**Goal**: Port Cranelift's Wasm→CLIF translator with 100% architectural parity.

## Current State

We have partial infrastructure but the translator uses invented intermediate representation:

| Component | Status | Issue |
|-----------|--------|-------|
| `ir/clif/builder.zig` | Partial | Has `FuncBuilder` with `ins()` methods, missing SSA variables |
| `wasm_to_clif/translator.zig` | ❌ Wrong | Uses `EmittedInst` array instead of direct `FuncBuilder` |
| `wasm_to_clif/stack.zig` | ❌ Wrong | Uses internal `Value` type instead of `clif.Value` |

## Cranelift Architecture (Reference)

### Key Files in Cranelift

1. **cranelift-frontend/src/frontend.rs** (~800 lines)
   - `FunctionBuilderContext` - Reusable context between functions
   - `FunctionBuilder` - Builds CLIF IR with SSA variable support
   - Key methods: `declare_var`, `def_var`, `use_var`, `seal_block`

2. **cranelift-frontend/src/ssa.rs** (~400 lines)
   - `SSABuilder` - Handles SSA construction for variables
   - Incomplete CFG algorithm for phi placement

3. **wasmtime/crates/cranelift/src/translate/func_translator.rs** (~333 lines)
   - `FuncTranslator` - Main translator struct
   - `translate_body()` - Entry point for translating a function

4. **wasmtime/crates/cranelift/src/translate/code_translator.rs** (~3000+ lines)
   - `translate_operator()` - Big match on Wasm operators
   - Pops from value stack, calls `builder.ins().xxx()`, pushes result

5. **wasmtime/crates/cranelift/src/translate/stack.rs**
   - Value stack holds `ir::Value` (CLIF values)
   - Control stack for blocks/loops/ifs

### How Cranelift Works

```rust
// In FuncTranslator::translate_body()
let mut builder = FunctionBuilder::new(func, &mut self.func_ctx);
let entry_block = builder.create_block();
builder.switch_to_block(entry_block);
builder.seal_block(entry_block);

// For each Wasm operator:
// In translate_operator()
match op {
    Operator::LocalGet { local_index } => {
        let val = builder.use_var(Variable::from_u32(*local_index));
        state.push1(val);  // Push CLIF Value
    }
    Operator::I32Add => {
        let (a, b) = state.pop2();
        let result = builder.ins().iadd(a, b);  // Returns CLIF Value
        state.push1(result);
    }
}
```

## Port Plan

### Phase 1: Port cranelift-frontend (~800 LOC)

Create `compiler/codegen/native/frontend/`:

1. **frontend.zig** - Port of `frontend.rs`
   - `FunctionBuilderContext` struct
   - `FunctionBuilder` struct with:
     - `declare_var(ty)` → `Variable`
     - `def_var(var, val)`
     - `use_var(var)` → `Value`
     - `create_block()` → `Block`
     - `switch_to_block(block)`
     - `seal_block(block)`
     - `ins()` → returns instruction builder interface
     - `ensure_inserted_block()`
     - `block_params(block)` → slice of Values
     - `append_block_params_for_function_params(block)`

2. **ssa.zig** - Port of `ssa.rs`
   - `SSABuilder` for variable handling
   - Block predecessor tracking
   - Phi node insertion (incomplete CFG algorithm)

3. **variable.zig** - Port of `variable.rs`
   - `Variable` type (index into variables)

### Phase 2: Update translator to use FunctionBuilder

Rewrite `compiler/codegen/native/wasm_to_clif/`:

1. **translator.zig** - Port of `code_translator.rs`
   - Remove `EmittedInst` completely
   - `translate_operator()` directly uses `builder.ins().xxx()`
   - Value stack holds `clif.Value` not internal type

2. **func_translator.zig** - Port of `func_translator.rs`
   - Uses `FunctionBuilder` from frontend
   - Proper local variable handling with `declare_var`/`def_var`/`use_var`

3. **stack.zig** - Update to use CLIF types
   - Value stack: `[]clif.Value`
   - Control stack frames store `clif.Block`

### Phase 3: Driver Integration

Connect the ported translator to the native compilation pipeline:

1. Parse Wasm bytecode (existing `wasm_parser`)
2. For each function:
   - Create `FunctionBuilder`
   - Call `translate_body()`
   - Function is now populated with CLIF IR
3. Pass CLIF `Function` to `compile.compile()`

## Files to Create/Modify

### Create (Port from Cranelift)

| File | Source | Est. Lines |
|------|--------|------------|
| `frontend/frontend.zig` | `cranelift-frontend/src/frontend.rs` | ~600 |
| `frontend/ssa.zig` | `cranelift-frontend/src/ssa.rs` | ~400 |
| `frontend/variable.zig` | `cranelift-frontend/src/variable.rs` | ~50 |
| `frontend/mod.zig` | Module exports | ~30 |

### Rewrite (To Match Cranelift)

| File | Reason |
|------|--------|
| `wasm_to_clif/translator.zig` | Use FunctionBuilder instead of EmittedInst |
| `wasm_to_clif/func_translator.zig` | Use FunctionBuilder properly |
| `wasm_to_clif/stack.zig` | Use clif.Value/Block types |

### Modify

| File | Change |
|------|--------|
| `ir/clif/builder.zig` | May need updates to work with FunctionBuilder |
| `driver.zig` | Wire up new translator |

## Verification

Each phase must pass tests before proceeding:

1. Phase 1: Unit tests for FunctionBuilder SSA handling
2. Phase 2: Translator tests with simple Wasm functions
3. Phase 3: End-to-end native compilation tests

## References

- Cranelift frontend: `~/learning/wasmtime/cranelift/frontend/src/`
- Cranelift translator: `~/learning/wasmtime/crates/cranelift/src/translate/`
- Existing CLIF IR: `compiler/ir/clif/`
