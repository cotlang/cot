pub mod cir;
pub mod translate;

use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{default_libcall_names, Linkage, Module};
use cranelift_native;
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::ffi::CStr;
use std::os::raw::c_char;

/// Compile CIR bytes to a native object file.
///
/// Phase 1: ignores cir_bytes, compiles a hardcoded trivial function
/// to prove Cranelift pipeline works end-to-end via C ABI.
///
/// Returns 0 on success, non-zero on error.
/// On success, out_ptr/out_len contain the .o file bytes.
#[unsafe(no_mangle)]
pub extern "C" fn clif_compile(
    cir_ptr: *const u8,
    cir_len: usize,
    target_str: *const c_char,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) -> i32 {
    match compile_impl(cir_ptr, cir_len, target_str) {
        Ok(bytes) => {
            let boxed = bytes.into_boxed_slice();
            unsafe {
                *out_len = boxed.len();
                *out_ptr = boxed.as_ptr();
            }
            std::mem::forget(boxed);
            0
        }
        Err(e) => {
            eprintln!("libclif error: {e}");
            1
        }
    }
}

/// Free bytes previously returned by clif_compile.
#[unsafe(no_mangle)]
pub extern "C" fn clif_free(ptr: *mut u8, len: usize) {
    if !ptr.is_null() && len > 0 {
        unsafe {
            drop(Vec::from_raw_parts(ptr, len, len));
        }
    }
}

/// Returns the version string of the Cranelift backend.
#[unsafe(no_mangle)]
pub extern "C" fn clif_version() -> *const c_char {
    static VERSION: &[u8] = b"libclif 0.1.0 (cranelift 0.116)\0";
    VERSION.as_ptr() as *const c_char
}

fn resolve_isa(target_name: &str) -> Result<std::sync::Arc<dyn cranelift_codegen::isa::TargetIsa>, Box<dyn std::error::Error>> {
    let mut flag_builder = settings::builder();
    flag_builder.set("opt_level", "speed")?;
    flag_builder.set("is_pic", "true")?;
    let flags = settings::Flags::new(flag_builder);

    match target_name {
        "arm64-macos" | "aarch64-macos" => {
            let triple = "aarch64-apple-darwin"
                .parse::<target_lexicon::Triple>()
                .map_err(|e| format!("parse triple: {e}"))?;
            Ok(cranelift_codegen::isa::lookup(triple)?.finish(flags)?)
        }
        "x64-linux" | "x86_64-linux" => {
            let triple = "x86_64-unknown-linux-gnu"
                .parse::<target_lexicon::Triple>()
                .map_err(|e| format!("parse triple: {e}"))?;
            Ok(cranelift_codegen::isa::lookup(triple)?.finish(flags)?)
        }
        "native" => Ok(cranelift_native::builder()
            .map_err(|e| format!("native ISA: {e}"))?
            .finish(flags)?),
        _ => Err(format!("unknown target: {target_name}").into()),
    }
}

fn compile_impl(cir_ptr: *const u8, cir_len: usize, target_str: *const c_char) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let target_cstr = unsafe { CStr::from_ptr(target_str) };
    let target_name = target_cstr.to_str()?;
    let isa = resolve_isa(target_name)?;

    // If CIR bytes provided, use the real pipeline
    if !cir_ptr.is_null() && cir_len > 0 {
        let cir_bytes = unsafe { std::slice::from_raw_parts(cir_ptr, cir_len) };
        let mut reader = cir::CirReader::new(cir_bytes);
        let module = reader.read_module().map_err(|e| format!("CIR read: {e}"))?;
        return translate::translate_module(&module, isa).map_err(|e| e.into());
    }

    // No CIR — hardcoded test function for smoke testing
    let obj_builder = ObjectBuilder::new(isa, "cot_module", default_libcall_names())?;
    let mut module = ObjectModule::new(obj_builder);

    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(I64));
    sig.params.push(AbiParam::new(I64));
    sig.returns.push(AbiParam::new(I64));

    let func_id = module.declare_function("_cot_test_add", Linkage::Export, &sig)?;
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig.clone());
    let mut func_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let a = builder.block_params(entry_block)[0];
        let b = builder.block_params(entry_block)[1];
        let sum = builder.ins().iadd(a, b);
        builder.ins().return_(&[sum]);
        builder.finalize();
    }
    let mut ctx = Context::for_function(func);
    module.define_function(func_id, &mut ctx)?;

    let product = module.finish();
    Ok(product.emit()?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_compile_native() {
        let target = CString::new("native").unwrap();
        let mut out_ptr: *const u8 = std::ptr::null();
        let mut out_len: usize = 0;

        let result = clif_compile(
            std::ptr::null(),
            0,
            target.as_ptr(),
            &mut out_ptr,
            &mut out_len,
        );

        assert_eq!(result, 0, "clif_compile should succeed");
        assert!(out_len > 0, "should produce non-empty .o");
        assert!(!out_ptr.is_null(), "out_ptr should be set");

        // Verify it's a valid object file (Mach-O or ELF magic)
        let bytes = unsafe { std::slice::from_raw_parts(out_ptr, out_len) };
        let is_macho = bytes.len() >= 4 && bytes[..4] == [0xCF, 0xFA, 0xED, 0xFE];
        let is_elf = bytes.len() >= 4 && bytes[..4] == [0x7F, 0x45, 0x4C, 0x46];
        assert!(is_macho || is_elf, "output should be valid Mach-O or ELF");

        clif_free(out_ptr as *mut u8, out_len);
    }

    #[test]
    fn test_version() {
        let v = clif_version();
        let s = unsafe { CStr::from_ptr(v) }.to_str().unwrap();
        assert!(s.contains("cranelift"));
    }
}
