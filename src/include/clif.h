/**
 * libclif — Cot Native Backend
 *
 * Target-specific: reads CIR, produces native machine code.
 *
 * Multiple implementations can satisfy this interface:
 *   - libclif-zig:  Zig Cranelift port (current, battle-tested)
 *   - libclif-rs:   Rust Cranelift crate (free updates)
 *   - libllvm-c:    LLVM backend (optimized release builds)
 *   - libclif:      Cot Cranelift port (ultimate self-hosting)
 *
 * The CLI selects the backend via --backend= flag.
 * All implementations produce the same output for the same input.
 */

#ifndef CLIF_H
#define CLIF_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ═══════════════════════════════════════════════════════════════
   Compile CIR → native object file
   ═══════════════════════════════════════════════════════════════ */

/**
 * In-process: compile CIR module from memory pointer.
 * The pointer comes from cir_get_module_ptr().
 * Returns 0 on success, non-zero on error.
 */
int clif_compile(const void* cir_module,
                 const char* target,        /* "arm64-macos", "x64-linux" */
                 const char* output_path);  /* "main.o" */

/**
 * From file: read .wasm with cot_ssa custom section.
 * Deserializes CIR from the binary format, then compiles.
 * Returns 0 on success, non-zero on error.
 */
int clif_compile_file(const char* wasm_path,
                      const char* target,
                      const char* output_path);

/* ═══════════════════════════════════════════════════════════════
   Link object file → executable
   ═══════════════════════════════════════════════════════════════ */

/**
 * Link an object file into an executable.
 * libs: null-terminated array of library names (e.g., {"sqlite3", NULL})
 * Returns 0 on success, non-zero on error.
 */
int clif_link(const char* object_path,
              const char* output_path,
              const char* target,
              const char** libs);

/* ═══════════════════════════════════════════════════════════════
   Query
   ═══════════════════════════════════════════════════════════════ */

int         clif_target_count(void);
const char* clif_target_name(int index);
const char* clif_version(void);
const char* clif_backend_name(void);  /* "cranelift-zig", "cranelift-rs", "llvm" */

/* ═══════════════════════════════════════════════════════════════
   Error reporting
   ═══════════════════════════════════════════════════════════════ */

const char* clif_last_error(void);

#ifdef __cplusplus
}
#endif

#endif /* CLIF_H */
