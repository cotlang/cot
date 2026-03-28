/**
 * libcot — Cot Compiler Frontend
 *
 * Language-specific: scanner, parser, checker, lowerer, LSP.
 * Produces CIR instructions via libcir's C ABI.
 *
 * This header is the contract. Any implementation (Zig, Cot, or other)
 * that satisfies this interface can serve as the frontend.
 */

#ifndef COT_H
#define COT_H

#include <stdint.h>
#include <stdbool.h>
#include "cir.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque compiler handle */
typedef struct CotCompiler* CotCompilerRef;

/* Diagnostic entry */
typedef struct CotDiagnostic {
    const char* file;
    uint32_t    line;
    uint32_t    col;
    const char* message;
    int         severity;  /* 0=error, 1=warning, 2=info */
} CotDiagnostic;

/* ═══════════════════════════════════════════════════════════════
   Lifecycle
   ═══════════════════════════════════════════════════════════════ */

CotCompilerRef  cot_compiler_create(void);
void            cot_compiler_destroy(CotCompilerRef c);

/* ═══════════════════════════════════════════════════════════════
   Configuration
   ═══════════════════════════════════════════════════════════════ */

void cot_set_target(CotCompilerRef c, const char* target);
void cot_set_safe_mode(CotCompilerRef c, bool safe);
void cot_set_test_mode(CotCompilerRef c, bool test);
void cot_set_release_mode(CotCompilerRef c, bool release);

/* ═══════════════════════════════════════════════════════════════
   Compile: source → CIR module
   Calls libcir's cir_build_* functions internally.
   Returns NULL on compilation failure.
   ═══════════════════════════════════════════════════════════════ */

CirModuleRef cot_compile(CotCompilerRef c, const char* source, const char* filename);
CirModuleRef cot_compile_file(CotCompilerRef c, const char* path);

/* ═══════════════════════════════════════════════════════════════
   Check only: parse + type-check, no IR output
   ═══════════════════════════════════════════════════════════════ */

bool cot_check(CotCompilerRef c, const char* source, const char* filename);
bool cot_check_file(CotCompilerRef c, const char* path);

/* ═══════════════════════════════════════════════════════════════
   Diagnostics
   ═══════════════════════════════════════════════════════════════ */

int             cot_diagnostic_count(CotCompilerRef c);
CotDiagnostic   cot_diagnostic_get(CotCompilerRef c, int index);
bool            cot_has_errors(CotCompilerRef c);

/* ═══════════════════════════════════════════════════════════════
   Version
   ═══════════════════════════════════════════════════════════════ */

const char* cot_version(void);

#ifdef __cplusplus
}
#endif

#endif /* COT_H */
