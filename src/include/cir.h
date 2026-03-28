/**
 * libcir — Cot Intermediate Representation
 *
 * Language-agnostic: SSA builder, optimization passes, ARC, VWT,
 * generics, concurrency, Wasm emission, CIR binary serialization.
 *
 * Any frontend that calls these functions gets ARC + actors +
 * generics + Wasm + native compilation for free.
 *
 * This header is the contract. Any implementation (Zig, Cot, or other)
 * that satisfies this interface can serve as the IR library.
 */

#ifndef CIR_H
#define CIR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ═══════════════════════════════════════════════════════════════
   Opaque handles
   ═══════════════════════════════════════════════════════════════ */

typedef struct CirModule*  CirModuleRef;
typedef struct CirFunc*    CirFuncRef;
typedef struct CirBlock*   CirBlockRef;
typedef struct CirBuilder* CirBuilderRef;
typedef uint32_t           CirValueRef;
typedef uint32_t           CirTypeRef;

/* ═══════════════════════════════════════════════════════════════
   Pre-registered type constants
   ═══════════════════════════════════════════════════════════════ */

#define CIR_TYPE_INVALID      0
#define CIR_TYPE_BOOL         1
#define CIR_TYPE_I8           2
#define CIR_TYPE_I16          3
#define CIR_TYPE_I32          4
#define CIR_TYPE_I64          5
#define CIR_TYPE_U8           6
#define CIR_TYPE_U16          7
#define CIR_TYPE_U32          8
#define CIR_TYPE_U64          9
#define CIR_TYPE_F32         10
#define CIR_TYPE_F64         11
#define CIR_TYPE_VOID        12
#define CIR_TYPE_STRING      17
#define CIR_TYPE_NORETURN    22

/* ═══════════════════════════════════════════════════════════════
   Module lifecycle
   ═══════════════════════════════════════════════════════════════ */

CirModuleRef cir_module_create(const char* name);
void         cir_module_destroy(CirModuleRef mod);

/* ═══════════════════════════════════════════════════════════════
   Type construction
   ═══════════════════════════════════════════════════════════════ */

CirTypeRef cir_type_pointer(CirModuleRef mod, CirTypeRef elem, bool managed);
CirTypeRef cir_type_optional(CirModuleRef mod, CirTypeRef elem);
CirTypeRef cir_type_error_union(CirModuleRef mod, CirTypeRef elem, CirTypeRef error_set);
CirTypeRef cir_type_slice(CirModuleRef mod, CirTypeRef elem);
CirTypeRef cir_type_array(CirModuleRef mod, CirTypeRef elem, uint64_t length);
CirTypeRef cir_type_list(CirModuleRef mod, CirTypeRef elem);
CirTypeRef cir_type_map(CirModuleRef mod, CirTypeRef key, CirTypeRef value);
CirTypeRef cir_type_struct(CirModuleRef mod, const char* name,
                           const char** field_names, CirTypeRef* field_types,
                           uint32_t* field_offsets, uint32_t field_count,
                           uint32_t size, uint8_t alignment);
CirTypeRef cir_type_enum(CirModuleRef mod, const char* name,
                         const char** variant_names, int64_t* variant_values,
                         uint32_t variant_count, CirTypeRef backing_type);
CirTypeRef cir_type_union(CirModuleRef mod, const char* name,
                          const char** variant_names, CirTypeRef* payload_types,
                          uint32_t variant_count, CirTypeRef tag_type);
CirTypeRef cir_type_func(CirModuleRef mod, CirTypeRef* param_types,
                         uint32_t param_count, CirTypeRef return_type);
CirTypeRef cir_type_distinct(CirModuleRef mod, const char* name, CirTypeRef underlying);
CirTypeRef cir_type_actor(CirModuleRef mod, const char* name,
                          const char** field_names, CirTypeRef* field_types,
                          uint32_t* field_offsets, uint32_t field_count,
                          uint32_t size, uint8_t alignment);
CirTypeRef cir_type_task(CirModuleRef mod, CirTypeRef result_type);

/* ═══════════════════════════════════════════════════════════════
   Function and block construction
   ═══════════════════════════════════════════════════════════════ */

CirFuncRef    cir_func_create(CirModuleRef mod, const char* name,
                              CirTypeRef return_type, bool is_async);
void          cir_func_set_export(CirFuncRef func, bool is_export);
void          cir_func_set_actor_isolated(CirFuncRef func, CirTypeRef actor_type);
CirBlockRef   cir_block_create(CirFuncRef func);
void          cir_block_set_entry(CirFuncRef func, CirBlockRef block);
CirBuilderRef cir_builder_create(CirBlockRef block);
void          cir_builder_position_at_end(CirBuilderRef b, CirBlockRef block);

/* ═══════════════════════════════════════════════════════════════
   Global variables
   ═══════════════════════════════════════════════════════════════ */

uint32_t cir_global_create(CirModuleRef mod, const char* name,
                           CirTypeRef type, uint32_t size, bool is_const);

/* ═══════════════════════════════════════════════════════════════
   Constants
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_const_int(CirBuilderRef b, CirTypeRef type, int64_t value);
CirValueRef cir_build_const_float(CirBuilderRef b, CirTypeRef type, double value);
CirValueRef cir_build_const_bool(CirBuilderRef b, bool value);
CirValueRef cir_build_const_nil(CirBuilderRef b, CirTypeRef type);
CirValueRef cir_build_const_string(CirBuilderRef b, const char* data, uint32_t length);

/* ═══════════════════════════════════════════════════════════════
   Arithmetic
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_add(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_sub(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_mul(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_div(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_mod(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_neg(CirBuilderRef b, CirValueRef operand);

/* ═══════════════════════════════════════════════════════════════
   Bitwise
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_and(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_or(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_xor(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_shl(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_shr(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_not(CirBuilderRef b, CirValueRef operand);

/* ═══════════════════════════════════════════════════════════════
   Comparison
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_eq(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_ne(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_lt(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_le(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_gt(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef cir_build_ge(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);

/* ═══════════════════════════════════════════════════════════════
   Memory
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_load(CirBuilderRef b, CirValueRef addr, CirTypeRef type);
void        cir_build_store(CirBuilderRef b, CirValueRef addr, CirValueRef value);
CirValueRef cir_build_local_addr(CirBuilderRef b, uint32_t slot_index);
CirValueRef cir_build_global_addr(CirBuilderRef b, const char* name);
CirValueRef cir_build_off_ptr(CirBuilderRef b, CirValueRef base, int32_t offset);
void        cir_build_move(CirBuilderRef b, CirValueRef dst, CirValueRef src,
                           CirValueRef size);
void        cir_build_zero(CirBuilderRef b, CirValueRef dst, CirValueRef size);

/* ═══════════════════════════════════════════════════════════════
   Control flow
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_phi(CirBuilderRef b, CirTypeRef type,
                          CirValueRef* values, CirBlockRef* blocks, uint32_t count);
CirValueRef cir_build_arg(CirBuilderRef b, CirTypeRef type, uint32_t index);
CirValueRef cir_build_call(CirBuilderRef b, CirFuncRef callee,
                           CirValueRef* args, uint32_t num_args);
CirValueRef cir_build_static_call(CirBuilderRef b, const char* name,
                                  CirValueRef* args, uint32_t num_args,
                                  CirTypeRef return_type);
void        cir_build_ret(CirBuilderRef b, CirValueRef value);
void        cir_build_ret_void(CirBuilderRef b);
void        cir_build_br(CirBuilderRef b, CirBlockRef target);
void        cir_build_brif(CirBuilderRef b, CirValueRef cond,
                           CirBlockRef then_block, CirBlockRef else_block);
CirValueRef cir_build_select(CirBuilderRef b, CirValueRef cond,
                             CirValueRef true_val, CirValueRef false_val);

/* ═══════════════════════════════════════════════════════════════
   String / Slice / Optional
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_string_make(CirBuilderRef b, CirValueRef ptr, CirValueRef len);
CirValueRef cir_build_string_ptr(CirBuilderRef b, CirValueRef str);
CirValueRef cir_build_string_len(CirBuilderRef b, CirValueRef str);
CirValueRef cir_build_slice_make(CirBuilderRef b, CirValueRef ptr,
                                 CirValueRef len, CirValueRef cap);
CirValueRef cir_build_opt_make(CirBuilderRef b, CirValueRef tag, CirValueRef payload);
CirValueRef cir_build_opt_tag(CirBuilderRef b, CirValueRef opt);
CirValueRef cir_build_opt_data(CirBuilderRef b, CirValueRef opt);

/* ═══════════════════════════════════════════════════════════════
   ARC — Automatic Reference Counting
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_retain(CirBuilderRef b, CirValueRef value);
CirValueRef cir_build_release(CirBuilderRef b, CirValueRef value);
CirValueRef cir_build_alloc(CirBuilderRef b, CirValueRef metadata, uint64_t size);
void        cir_build_dealloc(CirBuilderRef b, CirValueRef value);
CirValueRef cir_build_is_unique(CirBuilderRef b, CirValueRef value);

/* ═══════════════════════════════════════════════════════════════
   VWT — Value Witness Tables (generics)
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_vwt_copy(CirBuilderRef b, CirValueRef metadata,
                               CirValueRef src, CirValueRef dst);
CirValueRef cir_build_vwt_destroy(CirBuilderRef b, CirValueRef metadata,
                                  CirValueRef value);
CirValueRef cir_build_vwt_size(CirBuilderRef b, CirValueRef metadata);
CirValueRef cir_build_vwt_stride(CirBuilderRef b, CirValueRef metadata);
CirValueRef cir_build_metadata_addr(CirBuilderRef b, const char* type_name);

/* ═══════════════════════════════════════════════════════════════
   Concurrency — Actors, Tasks
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_actor_enqueue(CirBuilderRef b, CirValueRef actor,
                                    CirValueRef job);
CirValueRef cir_build_actor_resign(CirBuilderRef b, CirValueRef actor);
CirValueRef cir_build_task_create(CirBuilderRef b, const char* func_name,
                                  CirValueRef context);
CirValueRef cir_build_task_switch(CirBuilderRef b, CirValueRef executor);
CirValueRef cir_build_task_cancel(CirBuilderRef b, CirValueRef task);
CirValueRef cir_build_task_is_cancelled(CirBuilderRef b, CirValueRef task);
CirValueRef cir_build_await(CirBuilderRef b, CirTypeRef type, CirValueRef future);
void        cir_build_async_suspend(CirBuilderRef b, uint32_t state_index);
void        cir_build_async_resume(CirBuilderRef b, uint32_t state_index);

/* ═══════════════════════════════════════════════════════════════
   Atomics
   ═══════════════════════════════════════════════════════════════ */

CirValueRef cir_build_atomic_load(CirBuilderRef b, CirValueRef addr, CirTypeRef type);
void        cir_build_atomic_store(CirBuilderRef b, CirValueRef addr, CirValueRef val);
CirValueRef cir_build_atomic_add(CirBuilderRef b, CirValueRef addr, CirValueRef val);
CirValueRef cir_build_atomic_cas(CirBuilderRef b, CirValueRef addr,
                                 CirValueRef expected, CirValueRef desired);
CirValueRef cir_build_atomic_exchange(CirBuilderRef b, CirValueRef addr,
                                      CirValueRef val);

/* ═══════════════════════════════════════════════════════════════
   Passes and emission
   ═══════════════════════════════════════════════════════════════ */

void cir_run_passes(CirModuleRef mod);
int  cir_emit_wasm(CirModuleRef mod, const char* output_path);
int  cir_emit_wasm_with_cir(CirModuleRef mod, const char* output_path);

/* ═══════════════════════════════════════════════════════════════
   In-process native compilation (pass to libclif)
   ═══════════════════════════════════════════════════════════════ */

const void* cir_get_module_ptr(CirModuleRef mod);

/* ═══════════════════════════════════════════════════════════════
   Validation
   ═══════════════════════════════════════════════════════════════ */

#define CIR_VALIDATE_STRUCTURAL  0
#define CIR_VALIDATE_ARC         1
#define CIR_VALIDATE_TYPES       2
#define CIR_VALIDATE_CONCURRENCY 3
#define CIR_VALIDATE_ALL         4

int         cir_validate(CirModuleRef mod, int level);
int         cir_validation_error_count(CirModuleRef mod);
const char* cir_validation_error_get(CirModuleRef mod, int index);

/* ═══════════════════════════════════════════════════════════════
   Version
   ═══════════════════════════════════════════════════════════════ */

const char* cir_version(void);

#ifdef __cplusplus
}
#endif

#endif /* CIR_H */
