# Cot JIT Implementation Roadmap

## Overview

This directory contains the Zig-side infrastructure for JIT compilation via Cranelift. The implementation is designed so that **JIT is optional** - Cot works perfectly fine with just the bytecode interpreter.

## Current Status: Zig Infrastructure Complete

All Zig-side code is in place. The remaining work is implementing the Rust/Cranelift bridge.

### Completed (Zig Side)

| File | Purpose | Status |
|------|---------|--------|
| `ffi.zig` | C API declarations for Rust bridge | Done |
| `ir_serializer.zig` | Serializes Cot IR to binary format | Done |
| `runtime_exports.zig` | Exports runtime functions with C ABI | Done |
| `jit_manager.zig` | Orchestrates JIT compilation | Done |
| `jit.zig` | Module entry point | Done |
| `build.zig` | `-Djit=true` build flag | Done |

### Remaining (Rust Side)

| Component | Purpose | Status |
|-----------|---------|--------|
| `cranelift-bridge/` | Rust crate implementing C API | Not Started |
| IR Parser | Parse binary IR format | Not Started |
| CLIF Translation | Convert Cot IR to Cranelift IR | Not Started |
| Code Generation | Compile and return native code | Not Started |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Zig Side                            │
├─────────────────────────────────────────────────────────────┤
│  jit_manager.zig                                            │
│  ├── Compiles IR modules                                    │
│  ├── Caches compiled functions                              │
│  └── Provides function lookup                               │
│                                                             │
│  ir_serializer.zig                                          │
│  └── Converts ir.Module → binary bytes                      │
│                                                             │
│  runtime_exports.zig                                        │
│  └── C-ABI functions JIT code can call                      │
│                                                             │
│  ffi.zig                                                    │
│  └── extern "C" declarations + stubs when JIT disabled      │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ Binary IR + Function Pointers
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                        Rust Side                            │
├─────────────────────────────────────────────────────────────┤
│  cranelift-bridge/src/lib.rs                                │
│  ├── cot_jit_init() → Create JIT context                    │
│  ├── cot_jit_compile() → Parse IR, generate code            │
│  ├── cot_jit_register_runtime_fn() → Store fn pointers      │
│  └── cot_jit_destroy() → Cleanup                            │
│                                                             │
│  cranelift-bridge/src/parser.rs                             │
│  └── Parse binary IR format into Rust structs               │
│                                                             │
│  cranelift-bridge/src/compiler.rs                           │
│  └── Translate to CLIF, invoke Cranelift, return code ptr   │
└─────────────────────────────────────────────────────────────┘
```

## Implementation Steps

### Step 1: Create Rust Crate Structure

```bash
cd src/jit
cargo new cranelift-bridge --lib
```

**Cargo.toml:**
```toml
[package]
name = "cot-cranelift"
version = "0.1.0"
edition = "2021"

[lib]
name = "cot_cranelift"
crate-type = ["cdylib", "staticlib"]

[dependencies]
cranelift = "0.111"
cranelift-jit = "0.111"
cranelift-module = "0.111"
cranelift-native = "0.111"
target-lexicon = "0.12"
```

### Step 2: Implement C API (lib.rs)

The FFI functions declared in `ffi.zig` must be implemented:

```rust
use std::ffi::{c_char, CStr};
use std::collections::HashMap;

pub struct JitContext {
    jit: cranelift_jit::JITModule,
    runtime_fns: HashMap<String, *const u8>,
}

#[no_mangle]
pub extern "C" fn cot_jit_init() -> *mut JitContext {
    // Initialize Cranelift JIT
    let builder = cranelift_jit::JITBuilder::new(
        cranelift_module::default_libcall_names()
    ).expect("JIT builder failed");

    let jit = cranelift_jit::JITModule::new(builder);

    Box::into_raw(Box::new(JitContext {
        jit,
        runtime_fns: HashMap::new(),
    }))
}

#[no_mangle]
pub extern "C" fn cot_jit_destroy(ctx: *mut JitContext) {
    if !ctx.is_null() {
        unsafe { drop(Box::from_raw(ctx)); }
    }
}

#[no_mangle]
pub extern "C" fn cot_jit_register_runtime_fn(
    ctx: *mut JitContext,
    name: *const c_char,
    ptr: *const u8,
) {
    let ctx = unsafe { &mut *ctx };
    let name = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    ctx.runtime_fns.insert(name, ptr);
}

#[no_mangle]
pub extern "C" fn cot_jit_compile(
    ctx: *mut JitContext,
    ir_bytes: *const u8,
    ir_len: usize,
    _options: CompileOptions,
) -> CompiledFunction {
    let ctx = unsafe { &mut *ctx };
    let ir_data = unsafe { std::slice::from_raw_parts(ir_bytes, ir_len) };

    // TODO: Parse IR and compile
    // See Step 3 and Step 4

    CompiledFunction {
        code_ptr: std::ptr::null(),
        code_size: 0,
        error_code: 1,
        error_message: c"Not implemented".as_ptr(),
    }
}
```

### Step 3: Implement IR Parser (parser.rs)

Parse the binary format defined in `ir_serializer.zig`:

```rust
pub struct Module {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: TypeId,
    pub blocks: Vec<Block>,
}

pub struct Block {
    pub label: String,
    pub instructions: Vec<Instruction>,
}

pub fn parse_module(data: &[u8]) -> Result<Module, ParseError> {
    let mut cursor = 0;

    // Check magic
    let magic = u32::from_le_bytes(data[0..4].try_into()?);
    if magic != 0x434F5449 { // "COTI"
        return Err(ParseError::InvalidMagic);
    }
    cursor += 4;

    // Version
    let version = u16::from_le_bytes(data[4..6].try_into()?);
    cursor += 2;

    // Function count
    let func_count = u16::from_le_bytes(data[6..8].try_into()?);
    cursor += 2;

    // Parse functions
    let mut functions = Vec::with_capacity(func_count as usize);
    for _ in 0..func_count {
        let (func, new_cursor) = parse_function(&data[cursor..])?;
        functions.push(func);
        cursor += new_cursor;
    }

    Ok(Module { functions })
}
```

### Step 4: Implement CLIF Translation (compiler.rs)

Translate parsed IR to Cranelift IR:

```rust
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, Linkage};

pub fn compile_function(
    jit: &mut JITModule,
    func: &Function,
    runtime_fns: &HashMap<String, *const u8>,
) -> Result<*const u8, CompileError> {
    let mut ctx = jit.make_context();
    let mut builder_ctx = FunctionBuilderContext::new();

    // Build signature
    let mut sig = jit.make_signature();
    for param in &func.params {
        sig.params.push(AbiParam::new(type_to_cranelift(param.ty)));
    }
    if func.return_type != TypeId::Void {
        sig.returns.push(AbiParam::new(type_to_cranelift(func.return_type)));
    }

    // Declare function
    let func_id = jit.declare_function(&func.name, Linkage::Export, &sig)?;
    ctx.func.signature = sig;

    // Build function body
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let mut value_map: HashMap<u32, Value> = HashMap::new();
    let mut block_map: HashMap<usize, Block> = HashMap::new();

    // Create blocks
    for (i, _) in func.blocks.iter().enumerate() {
        block_map.insert(i, builder.create_block());
    }

    // Translate instructions
    for (block_idx, block) in func.blocks.iter().enumerate() {
        let cl_block = block_map[&block_idx];
        builder.switch_to_block(cl_block);

        for inst in &block.instructions {
            translate_instruction(&mut builder, inst, &mut value_map, &block_map, runtime_fns)?;
        }
    }

    builder.seal_all_blocks();
    builder.finalize();

    // Compile
    jit.define_function(func_id, &mut ctx)?;
    jit.clear_context(&mut ctx);
    jit.finalize_definitions()?;

    Ok(jit.get_finalized_function(func_id))
}

fn translate_instruction(
    builder: &mut FunctionBuilder,
    inst: &Instruction,
    values: &mut HashMap<u32, Value>,
    blocks: &HashMap<usize, Block>,
    runtime_fns: &HashMap<String, *const u8>,
) -> Result<(), CompileError> {
    match inst.opcode {
        Opcode::Iadd => {
            let lhs = values[&inst.operands[0]];
            let rhs = values[&inst.operands[1]];
            let result = builder.ins().iadd(lhs, rhs);
            values.insert(inst.result, result);
        }
        Opcode::Icmp => {
            let cond = int_cc_from_u8(inst.flags);
            let lhs = values[&inst.operands[0]];
            let rhs = values[&inst.operands[1]];
            let result = builder.ins().icmp(cond, lhs, rhs);
            values.insert(inst.result, result);
        }
        Opcode::Jump => {
            let target = blocks[&(inst.operands[0] as usize)];
            builder.ins().jump(target, &[]);
        }
        Opcode::Return => {
            if inst.has_value {
                let val = values[&inst.operands[0]];
                builder.ins().return_(&[val]);
            } else {
                builder.ins().return_(&[]);
            }
        }
        // Runtime calls for strings, decimals, I/O
        Opcode::StrConcat => {
            let fn_ptr = runtime_fns["cot_rt_str_concat"];
            // Build call to runtime function
            // ...
        }
        // ... handle all opcodes
    }
    Ok(())
}
```

### Step 5: Build Integration

After implementing the Rust crate:

```bash
# Build Rust library
cd src/jit/cranelift-bridge
cargo build --release

# Build Cot with JIT
cd ../../..
zig build -Djit=true
```

### Step 6: Testing

1. **Unit tests in Rust** - Test IR parsing and compilation
2. **Integration tests** - Compile Cot functions, verify output matches interpreter
3. **Benchmark** - Compare JIT vs interpreter performance

## Binary IR Format Reference

See `ir_serializer.zig` for the complete format. Key points:

- **Magic**: `0x434F5449` ("COTI")
- **Little-endian** byte order
- **Opcodes**: See `Opcode` enum in `ir_serializer.zig`
- **Types**: See `TypeId` enum in `ir_serializer.zig`

## Runtime Functions

JIT code calls these functions (defined in `runtime_exports.zig`):

| Function | Purpose |
|----------|---------|
| `cot_rt_str_concat` | Concatenate strings |
| `cot_rt_str_compare` | Compare strings |
| `cot_rt_dec_add/sub/mul/div` | Decimal arithmetic |
| `cot_rt_mem_alloc/free` | Memory management |
| `cot_rt_err_throw` | Exception handling |
| `cot_rt_io_*` | File I/O (stubs, need integration) |

## Open Tasks

1. **I/O Integration**: Connect `cot_rt_io_*` functions to `channels.zig`
2. **Exception Handling**: Decide between setjmp/longjmp or Cranelift unwinding
3. **Debug Info**: Map JIT code back to source locations for stack traces
4. **Tiered Compilation**: Profile and selectively JIT hot functions

## Files to Create

```
src/jit/cranelift-bridge/
├── Cargo.toml
├── Cargo.lock
└── src/
    ├── lib.rs          # C API exports
    ├── context.rs      # JitContext struct
    ├── parser.rs       # Binary IR parser
    ├── compiler.rs     # CLIF translation
    ├── types.rs        # Type conversions
    └── error.rs        # Error handling
```

## Quick Start for Future Claude

1. Read this file first
2. Read `ffi.zig` to understand the C API contract
3. Read `ir_serializer.zig` to understand the binary format
4. Create the Rust crate and implement the functions
5. Test with `zig build -Djit=true`

The Zig side is complete and tested. Focus entirely on the Rust implementation.
