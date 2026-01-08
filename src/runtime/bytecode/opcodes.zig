//! Cot Bytecode Opcodes - Register-Based Architecture
//!
//! Pure register-based instruction set for the Cot virtual machine.
//! All operations use 16 virtual registers (r0-r15).
//!
//! Instruction Format:
//!   [opcode:8] [operands:variable]
//!
//! Register encoding:
//!   - 3-operand: [dest:4|src1:4] [src2:4|flags:4]
//!   - 2-operand: [dest:4|src:4] [imm8 or 0]
//!   - 1-operand: [reg:4|0] [imm8 or 0]
//!
//! Register conventions:
//!   r0-r7:  General purpose, caller-saved, function arguments
//!   r8-r13: General purpose, callee-saved
//!   r14:    Frame pointer (reserved)
//!   r15:    Return value

const std = @import("std");

/// Bytecode instruction opcodes
pub const Opcode = enum(u8) {
    // ============================================
    // Core Operations (0x00-0x0F)
    // ============================================

    nop = 0x00,
    halt = 0x01,

    // ============================================
    // Register Moves & Immediates (0x10-0x1F)
    // ============================================

    /// mov rd, rs - rd = rs
    /// Format: [rd:4|rs:4] [0]
    mov = 0x10,

    /// movi rd, imm8 - rd = sign_extend(imm8)
    /// Format: [rd:4|0] [imm8]
    movi = 0x11,

    /// movi16 rd, imm16 - rd = sign_extend(imm16)
    /// Format: [rd:4|0] [imm16:16]
    movi16 = 0x12,

    /// movi32 rd, imm32 - rd = sign_extend(imm32)
    /// Format: [rd:4|0] [imm32:32]
    movi32 = 0x13,

    /// load_const rd, idx - rd = constants[idx]
    /// Format: [rd:4|0] [idx:16]
    load_const = 0x14,

    /// load_null rd - rd = null
    /// Format: [rd:4|0] [0]
    load_null = 0x15,

    /// load_true rd - rd = true
    /// Format: [rd:4|0] [0]
    load_true = 0x16,

    /// load_false rd - rd = false
    /// Format: [rd:4|0] [0]
    load_false = 0x17,

    // ============================================
    // Local & Global Variables (0x20-0x2F)
    // ============================================

    /// load_local rd, slot - rd = locals[slot]
    /// Format: [rd:4|0] [slot:8]
    load_local = 0x20,

    /// store_local rs, slot - locals[slot] = rs
    /// Format: [rs:4|0] [slot:8]
    store_local = 0x21,

    /// load_local16 rd, slot - rd = locals[slot] (16-bit slot)
    /// Format: [rd:4|0] [slot:16]
    load_local16 = 0x22,

    /// store_local16 rs, slot - locals[slot] = rs (16-bit slot)
    /// Format: [rs:4|0] [slot:16]
    store_local16 = 0x23,

    /// load_global rd, idx - rd = globals[idx]
    /// Format: [rd:4|0] [idx:16]
    load_global = 0x24,

    /// store_global rs, idx - globals[idx] = rs
    /// Format: [rs:4|0] [idx:16]
    store_global = 0x25,

    // ============================================
    // Arithmetic (0x30-0x3F)
    // ============================================

    /// add rd, rs1, rs2 - rd = rs1 + rs2
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    add = 0x30,

    /// sub rd, rs1, rs2 - rd = rs1 - rs2
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    sub = 0x31,

    /// mul rd, rs1, rs2 - rd = rs1 * rs2
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    mul = 0x32,

    /// div rd, rs1, rs2 - rd = rs1 / rs2
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    div = 0x33,

    /// mod rd, rs1, rs2 - rd = rs1 % rs2
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    mod = 0x34,

    /// neg rd, rs - rd = -rs
    /// Format: [rd:4|rs:4] [0]
    neg = 0x35,

    /// addi rd, rs, imm8 - rd = rs + sign_extend(imm8)
    /// Format: [rd:4|rs:4] [imm8]
    addi = 0x36,

    /// subi rd, rs, imm8 - rd = rs - sign_extend(imm8)
    /// Format: [rd:4|rs:4] [imm8]
    subi = 0x37,

    /// muli rd, rs, imm8 - rd = rs * sign_extend(imm8)
    /// Format: [rd:4|rs:4] [imm8]
    muli = 0x38,

    /// incr rd - rd = rd + 1
    /// Format: [rd:4|0] [0]
    incr = 0x39,

    /// decr rd - rd = rd - 1
    /// Format: [rd:4|0] [0]
    decr = 0x3A,

    /// Decimal arithmetic with precision
    /// add_dec rd, rs1, rs2, prec - rd = rs1 + rs2 (decimal)
    /// Format: [rd:4|rs1:4] [rs2:4|prec:4]
    add_dec = 0x3B,

    /// sub_dec rd, rs1, rs2, prec
    sub_dec = 0x3C,

    /// mul_dec rd, rs1, rs2, prec
    mul_dec = 0x3D,

    /// div_dec rd, rs1, rs2, prec
    div_dec = 0x3E,

    // ============================================
    // Comparison (0x40-0x4F)
    // ============================================

    /// cmp_eq rd, rs1, rs2 - rd = (rs1 == rs2)
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    cmp_eq = 0x40,

    /// cmp_ne rd, rs1, rs2 - rd = (rs1 != rs2)
    cmp_ne = 0x41,

    /// cmp_lt rd, rs1, rs2 - rd = (rs1 < rs2)
    cmp_lt = 0x42,

    /// cmp_le rd, rs1, rs2 - rd = (rs1 <= rs2)
    cmp_le = 0x43,

    /// cmp_gt rd, rs1, rs2 - rd = (rs1 > rs2)
    cmp_gt = 0x44,

    /// cmp_ge rd, rs1, rs2 - rd = (rs1 >= rs2)
    cmp_ge = 0x45,

    /// cmp_str_eq rd, rs1, rs2 - rd = (rs1 == rs2) string comparison
    cmp_str_eq = 0x46,

    /// cmp_str_lt rd, rs1, rs2 - rd = (rs1 < rs2) string comparison
    cmp_str_lt = 0x47,

    /// cmp_str_ne rd, rs1, rs2 - rd = (rs1 != rs2) string comparison
    cmp_str_ne = 0x48,

    /// cmp_str_le rd, rs1, rs2 - rd = (rs1 <= rs2) string comparison
    cmp_str_le = 0x49,

    /// cmp_str_gt rd, rs1, rs2 - rd = (rs1 > rs2) string comparison
    cmp_str_gt = 0x4A,

    /// cmp_str_ge rd, rs1, rs2 - rd = (rs1 >= rs2) string comparison
    cmp_str_ge = 0x4B,

    // ============================================
    // Logical & Bitwise (0x50-0x5F)
    // ============================================

    /// log_and rd, rs1, rs2 - rd = rs1 && rs2
    log_and = 0x50,

    /// log_or rd, rs1, rs2 - rd = rs1 || rs2
    log_or = 0x51,

    /// log_not rd, rs - rd = !rs
    log_not = 0x52,

    /// bit_and rd, rs1, rs2 - rd = rs1 & rs2
    bit_and = 0x53,

    /// bit_or rd, rs1, rs2 - rd = rs1 | rs2
    bit_or = 0x54,

    /// bit_xor rd, rs1, rs2 - rd = rs1 ^ rs2
    bit_xor = 0x55,

    /// bit_not rd, rs - rd = ~rs
    bit_not = 0x56,

    /// is_null rd, rs - rd = (rs == null)
    /// Format: [rd:4|rs:4] [0]
    is_null = 0x57,

    /// select rd, cond, rtrue, rfalse - rd = cond ? rtrue : rfalse
    /// Format: [rd:4|cond:4] [rtrue:4|rfalse:4]
    select = 0x58,

    /// ptr_offset rd, rs, offset - rd = rs + offset (byte-level pointer arithmetic)
    /// Format: [rd:4|rs:4] [offset:16]
    ptr_offset = 0x59,

    /// shl rd, rs1, rs2 - rd = rs1 << rs2 (shift left)
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    shl = 0x5A,

    /// shr rd, rs1, rs2 - rd = rs1 >> rs2 (shift right)
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    shr = 0x5B,

    /// is_type rd, rs, type_tag - rd = (rs is type_tag)
    /// Format: [rd:4|rs:4] [type_tag:8]
    /// type_tag: 0=null, 1=bool, 2=int, 3=float, 4=string
    is_type = 0x5C,

    // ============================================
    // Control Flow (0x60-0x6F)
    // ============================================

    /// jmp offset - unconditional jump
    /// Format: [0] [offset:16]
    jmp = 0x60,

    /// jmp32 offset - unconditional jump (32-bit offset)
    /// Format: [0] [offset:32]
    jmp32 = 0x61,

    /// jz rs, offset - jump if rs == 0/false/null
    /// Format: [rs:4|0] [offset:16]
    jz = 0x62,

    /// jnz rs, offset - jump if rs != 0/false/null
    /// Format: [rs:4|0] [offset:16]
    jnz = 0x63,

    /// jeq rs1, rs2, offset - jump if rs1 == rs2
    /// Format: [rs1:4|rs2:4] [offset:16]
    jeq = 0x64,

    /// jne rs1, rs2, offset - jump if rs1 != rs2
    jne = 0x65,

    /// jlt rs1, rs2, offset - jump if rs1 < rs2
    jlt = 0x66,

    /// jge rs1, rs2, offset - jump if rs1 >= rs2
    jge = 0x67,

    /// loop_start - mark start of loop (for profiling)
    loop_start = 0x68,

    /// loop_end - mark end of loop
    loop_end = 0x69,

    /// set_error_handler offset - set error handler
    /// Format: [0] [offset:16]
    set_error_handler = 0x6A,

    /// clear_error_handler - clear error handler
    clear_error_handler = 0x6B,

    /// throw rs - throw exception with value in rs
    /// Format: [rs:4|0] [0]
    throw = 0x6C,

    // ============================================
    // Function Calls (0x70-0x7F)
    // ============================================

    /// call routine_idx, argc, stack_argc - call function
    /// Args in r0..r(argc-1), plus stack_argc args already pushed to stack
    /// Format: [argc:4|stack_argc:4] [routine_idx:16]
    /// Total args = argc + stack_argc
    call = 0x70,

    /// call_external import_idx, argc - call external function
    /// Format: [argc:4|0] [import_idx:16]
    call_external = 0x71,

    /// call_native native_idx, argc - call native function
    /// Format: [argc:4|0] [native_idx:16]
    call_native = 0x72,

    /// call_indirect rs, argc - call function pointer in rs
    /// Format: [argc:4|rs:4] [0]
    call_indirect = 0x73,

    /// call_dynamic name_idx, argc - call by name at runtime
    /// Format: [argc:4|0] [name_idx:16]
    call_dynamic = 0x74,

    /// ret count - return with optional overflow values preserved on stack
    /// Format: [count:8] [0]
    /// If count > 0, preserves that many values pushed via push_arg for caller to pop.
    /// count=0 for normal returns, count>0 for large struct returns or ref param writeback.
    ret = 0x75,

    /// ret_val rs - return value in rs
    /// Format: [rs:4|0] [0]
    ret_val = 0x76,

    /// push_arg slot - push local slot value to stack for overflow args
    /// Instruction: [opcode:8] [slot_lo:8] [slot_hi:8] = 3 bytes total
    /// Handler reads: slot = u16 at ip, then ip += 2
    push_arg = 0x77,

    /// push_arg_reg rs - push register value to stack for overflow args
    /// Instruction: [opcode:8] [rs:4|0:4] [0:8] = 3 bytes total (but only 2 operand bytes used)
    push_arg_reg = 0x78,

    /// pop_arg slot - pop stack value to local slot (callee prologue)
    /// Instruction: [opcode:8] [slot_lo:8] [slot_hi:8] = 3 bytes total
    /// Handler reads: slot = u16 at ip, then ip += 2
    pop_arg = 0x79,

    // 0x7A-0x7F reserved for future control flow opcodes

    // ============================================
    // Record/Field Operations (0x80-0x8F)
    // ============================================

    /// new_record rd, type_idx - rd = new record of type
    /// Format: [rd:4|0] [type_idx:16]
    new_record = 0x80,

    /// free_record rs - free record in rs
    /// Format: [rs:4|0] [0]
    free_record = 0x81,

    /// load_field rd, rs, field_idx - rd = rs.field[idx]
    /// Format: [rd:4|rs:4] [field_idx:16]
    load_field = 0x82,

    /// store_field rd, rs, field_idx - rd.field[idx] = rs
    /// Format: [rd:4|rs:4] [field_idx:16]
    store_field = 0x83,

    /// load_field_fast rd, rs, offset - rd = rs.field at byte offset
    /// Format: [rd:4|rs:4] [offset:8]
    load_field_fast = 0x84,

    /// store_field_fast rd, rs, offset - rd.field at offset = rs
    /// Format: [rd:4|rs:4] [offset:8]
    store_field_fast = 0x85,

    /// load_record_buf rd, type_idx, base, flags - serialize record fields to buffer
    /// Format: [rd:4|flags:4] [type_idx:16] [base:16]
    /// flags: 0=locals, 1=globals
    /// Serializes fields from storage[base..] using type definition, result in rd
    load_record_buf = 0x86,

    /// store_record_buf rs, type_idx, base, flags - deserialize buffer to record fields
    /// Format: [rs:4|flags:4] [type_idx:16] [base:16]
    /// flags: 0=locals, 1=globals
    /// Distributes buffer from rs into storage[base..] using type definition
    store_record_buf = 0x87,

    /// clear_record rs - clear record fields
    /// Format: [rs:4|0] [0]
    clear_record = 0x88,

    /// alloc_buffer slot, size - allocate mutable buffer of given size
    /// Format: [slot:8] [size:16]
    /// Creates a mutable FixedString filled with spaces at the local slot
    alloc_buffer = 0x89,

    // ============================================
    // Sum Type (Variant) Operations (0x8A-0x8F)
    // ============================================

    /// variant_construct rd, tag, argc - construct a variant with payload
    /// Creates a Variant value with the given tag and payload values from registers.
    /// Format: [rd:4|argc:4] [tag:16]
    /// Payload values are in r0..r(argc-1)
    variant_construct = 0x8A,

    /// variant_get_tag rd, src_reg - get the tag from a variant
    /// Format: [rd:4|src_reg:4] [0]
    variant_get_tag = 0x8B,

    /// variant_get_payload rd, src_reg, field_idx - get payload field from variant
    /// Format: [rd:4|src_reg:4] [field_idx:16]
    variant_get_payload = 0x8C,

    // ============================================
    // String Operations (0x90-0x9F)
    // ============================================

    /// str_concat rd, rs1, rs2 - rd = rs1 + rs2
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    str_concat = 0x90,

    /// str_len rd, rs - rd = len(rs)
    /// Format: [rd:4|rs:4] [0]
    str_len = 0x91,

    /// str_index rd, rs, idx_reg - rd = rs[idx_reg]
    /// Format: [rd:4|rs:4] [idx_reg:4|0]
    str_index = 0x92,

    /// str_slice rd, rs, start_reg, len_reg - rd = rs[start:start+len]
    /// Format: [rd:4|rs:4] [start_reg:4|len_reg:4]
    str_slice = 0x93,

    /// str_slice_store rd, start_reg, len_reg, val_reg - rd[start:len] = val
    /// Format: [rd:4|start_reg:4] [len_reg:4|val_reg:4]
    str_slice_store = 0x94,

    /// str_trim rd, rs - rd = trim(rs)
    str_trim = 0x95,

    /// str_upper rd, rs - rd = upper(rs)
    str_upper = 0x96,

    /// str_lower rd, rs - rd = lower(rs)
    str_lower = 0x97,

    /// str_find rd, rs1, rs2 - rd = find position of rs2 in rs1
    str_find = 0x98,

    /// str_replace rd, rs, old_reg, new_reg - rd = replace in rs
    /// Format: [rd:4|rs:4] [old_reg:4|new_reg:4]
    str_replace = 0x99,

    /// str_setchar rd, idx_reg, char_reg - rd[idx] = char
    /// Format: [rd:4|idx_reg:4] [char_reg:4|0]
    str_setchar = 0x9A,

    // ============================================
    // Type Conversion (0xA0-0xAF)
    // ============================================

    /// to_int rd, rs - rd = int(rs)
    to_int = 0xA0,

    /// to_str rd, rs - rd = str(rs)
    to_str = 0xA1,

    /// to_bool rd, rs - rd = bool(rs)
    to_bool = 0xA2,

    /// to_dec rd, rs, prec - rd = decimal(rs, precision)
    /// Format: [rd:4|rs:4] [prec:8]
    to_dec = 0xA3,

    /// to_char rd, rs - rd = char(rs) (int to single char string)
    to_char = 0xA4,

    /// to_fixed_string rd, rs, size - rd = fixed_string(rs, size)
    /// Format: [rd:4|rs:4] [size:16]
    to_fixed_string = 0xA5,

    /// format_decimal rd, rs, width - rd = zero-padded decimal string of rs
    /// For DBL compatibility: formats integer to fixed-width zero-padded string
    /// Negative values use overpunch encoding (last digit becomes letter)
    /// Format: [rd:4|rs:4] [width:8]
    format_decimal = 0xA6,

    /// parse_decimal rd, rs - rd = integer parsed from string rs
    /// For DBL compatibility: validates string contains only digits (with optional leading minus)
    /// Raises runtime error "bad digit" if string contains non-numeric characters
    /// Format: [rd:4|rs:4] [0]
    parse_decimal = 0xA7,

    // ============================================
    // Array Operations (0xB0-0xBF)
    // ============================================

    /// array_load idx_reg, slot - load arr[idx] into r0
    /// Format: [idx_reg:8] [slot:16]
    array_load = 0xB0,

    /// array_store idx_reg, val_reg, slot - arr[idx] = val
    /// Format: [idx_reg:4|val_reg:4] [slot:16]
    array_store = 0xB1,

    /// array_len rd, slot - rd = len(arr) (compile-time constant for fixed arrays)
    /// Format: [rd:8] [slot:16]
    array_len = 0xB2,

    /// list_len rd, list_reg - rd = list.len()
    /// Format: [rd:4|list:4] [0]
    list_len = 0xB3,

    /// list_clear list_reg - list.clear()
    /// Format: [list:4|0] [0]
    list_clear = 0xB4,

    /// list_push_struct list_reg, base_slot, field_count - push struct from consecutive slots
    /// Format: [list:4|0] [field_count:8] [base_slot:16]
    list_push_struct = 0xB5,

    /// list_get_struct list_reg, idx_reg, field_count, base_reg - get struct and expand to high registers
    /// Format: [list:4|idx:4] [field_count:8] [base_reg:8] [0]
    list_get_struct = 0xB6,

    /// list_pop_struct dest_slot, list_reg, field_count - pop struct and expand to slots
    /// Format: [list:4|0] [field_count:8] [dest_slot:16]
    list_pop_struct = 0xB7,

    /// list_set_struct list_reg, idx_reg, base_slot, field_count - set struct from consecutive slots
    /// Format: [list:4|idx:4] [field_count:8] [base_slot:16]
    list_set_struct = 0xB8,

    /// map_set_struct map_reg, key_reg, base_slot, field_count - store struct to map
    /// Format: [map:4|key:4] [field_count:8] [base_slot:16]
    map_set_struct = 0xB9,

    /// map_get_struct map_reg, key_reg, field_count, base_reg - get struct from map to high registers
    /// Format: [map:4|key:4] [field_count:8] [base_reg:8] [0]
    map_get_struct = 0xBA,

    /// array_slice rd, start_reg, end_reg, slot - rd = arr[start..end] or arr[start..=end]
    /// Creates a new list containing elements from start to end (exclusive unless inclusive flag set)
    /// Format: [rd:4|inclusive:1|0:3] [start_reg:4|end_reg:4] [slot_lo:8] [slot_hi:8]
    array_slice = 0xBB,

    /// array_load_opt rd, idx_reg, slot, len - optional load from array, returns null if out of bounds
    /// Format: [idx_reg:8] [slot:16] [len:16]
    /// Result stored in r0, returns null instead of throwing on out-of-bounds
    array_load_opt = 0xBC,

    /// list_to_slice rd, list_reg - rd = list.to_slice() (converts List<T> to []T)
    /// Format: [rd:4|list:4] [0]
    list_to_slice = 0xBD,

    // ============================================
    // Built-in Functions (0xC0-0xCF)
    // ============================================

    /// fn_abs rd, rs - rd = abs(rs)
    fn_abs = 0xC0,

    /// fn_sqrt rd, rs - rd = sqrt(rs)
    fn_sqrt = 0xC1,

    /// fn_sin rd, rs - rd = sin(rs)
    fn_sin = 0xC2,

    /// fn_cos rd, rs - rd = cos(rs)
    fn_cos = 0xC3,

    /// fn_tan rd, rs - rd = tan(rs)
    fn_tan = 0xC4,

    /// fn_log rd, rs - rd = log(rs)
    fn_log = 0xC5,

    /// fn_log10 rd, rs - rd = log10(rs)
    fn_log10 = 0xC6,

    /// fn_exp rd, rs - rd = exp(rs)
    fn_exp = 0xC7,

    /// fn_round rd, rs, prec - rd = round(rs, precision)
    /// Format: [rd:4|rs:4] [prec:8]
    fn_round = 0xC8,

    /// fn_trunc rd, rs - rd = trunc(rs)
    fn_trunc = 0xC9,

    /// fn_date rd - rd = current date
    fn_date = 0xCA,

    /// fn_time rd - rd = current time
    fn_time = 0xCB,

    /// fn_size rd, rs - rd = size(rs)
    fn_size = 0xCC,

    /// fn_instr rd, rs1, rs2 - rd = instr(rs1, rs2)
    fn_instr = 0xCD,

    /// fn_mem rd - rd = available memory
    fn_mem = 0xCE,

    /// fn_error rd - rd = last error
    fn_error = 0xCF,

    // ============================================
    // Terminal I/O (0xD0-0xDF)
    // ============================================

    /// print rs, argc - write to stdout without newline
    /// Format: [rs:4|argc:4] [0]
    print = 0xD0,

    /// println rs, argc - write to stdout with newline
    /// Format: [rs:4|argc:4] [0]
    println = 0xD1,

    /// readln rd - rd = read line from stdin
    /// Format: [rd:4|0] [0]
    readln = 0xD2,

    /// readkey rd - rd = read single key
    /// Format: [rd:4|0] [0]
    readkey = 0xD3,

    /// log rs, argc - write to stderr (debugging)
    /// Format: [rs:4|argc:4] [0]
    log = 0xD4,

    // ============================================
    // Map Operations (0xD5-0xDF)
    // ============================================

    /// map_new rd, flags - rd = new map with flags
    /// flags: bit 0 = case_sensitive, bit 1 = preserve_spaces
    /// Format: [rd:4|flags:4] [0]
    map_new = 0xD5,

    /// map_set map_reg, key_reg, val_reg - map[key] = val
    /// Format: [map:4|key:4] [val:4|0]
    map_set = 0xD6,

    /// map_get rd, map_reg, key_reg - rd = map[key]
    /// Format: [rd:4|map:4] [key:4|0]
    map_get = 0xD7,

    /// map_delete map_reg, key_reg - delete map[key]
    /// Format: [map:4|key:4] [0]
    map_delete = 0xD8,

    /// map_has rd, map_reg, key_reg - rd = map.has(key)
    /// Format: [rd:4|map:4] [key:4|0]
    map_has = 0xD9,

    /// map_len rd, map_reg - rd = map.len()
    /// Format: [rd:4|map:4] [0]
    map_len = 0xDA,

    /// map_clear map_reg - map.clear()
    /// Format: [map:4|0] [0]
    map_clear = 0xDB,

    /// map_keys rd, map_reg - rd = array of keys
    /// Format: [rd:4|map:4] [0]
    map_keys = 0xDC,

    /// map_values rd, map_reg - rd = array of values
    /// Format: [rd:4|map:4] [0]
    map_values = 0xDD,

    /// map_get_at rd, map_reg, idx_reg - rd = map entry at position (1-based)
    /// Format: [rd:4|map:4] [idx:4|0]
    map_get_at = 0xDE,

    /// map_set_at map_reg, idx_reg, val_reg - map[pos] = val (by position)
    /// Format: [map:4|idx:4] [val:4|0]
    map_set_at = 0xDF,

    /// map_key_at rd, map_reg, idx_reg - rd = key at position (1-based)
    /// Format: [rd:4|map:4] [idx:4|0]
    map_key_at = 0xFC,

    // ============================================
    // List Operations (0xED-0xEF and extended)
    // ============================================

    /// list_new rd - rd = new empty list
    /// Format: [rd:4|0] [0]
    list_new = 0xED,

    /// list_push list_reg, val_reg - list.push(val)
    /// Format: [list:4|val:4] [0]
    list_push = 0xEE,

    /// list_pop rd, list_reg - rd = list.pop()
    /// Format: [rd:4|list:4] [0]
    list_pop = 0xEF,

    /// list_get rd, list_reg, idx_reg - rd = list[idx]
    /// Format: [rd:4|list:4] [idx:4|0]
    list_get = 0xFD,

    /// list_set list_reg, idx_reg, val_reg - list[idx] = val
    /// Format: [list:4|idx:4] [val:4|0]
    list_set = 0xFF,

    // ============================================
    // Debug & Meta (0xF0-0xFF)
    // ============================================

    /// debug_break - debugger breakpoint
    debug_break = 0xF0,

    /// debug_line line - set current source line
    /// Format: [0] [line:16]
    debug_line = 0xF1,

    /// assert rs - assert rs is true
    /// Format: [rs:4|0] [0]
    assert = 0xF2,

    /// weak_ref rd, rs - create weak reference: rd = weak(rs)
    /// Creates a weak reference to rs without incrementing refcount.
    /// When rs is freed, rd becomes null.
    /// Format: [rd:4|rs:4] [0]
    weak_ref = 0xF3,

    /// weak_load rd, rs - load from weak reference: rd = *rs (or null if freed)
    /// Returns null if the weak reference target has been freed.
    /// Returns the value (with retain) if still alive.
    /// Format: [rd:4|rs:4] [0]
    weak_load = 0xF4,

    // ============================================
    // ARC Operations (0xF5-0xF7)
    // Explicit reference counting operations for compiler integration
    // ============================================

    /// arc_retain rs - increment reference count of heap value
    /// No-op for inline values (ints, bools, null).
    /// Format: [rs:4|0] [0]
    arc_retain = 0xF5,

    /// arc_release rs - decrement reference count (may free)
    /// No-op for inline values. Frees object when refcount reaches 0.
    /// Format: [rs:4|0] [0]
    arc_release = 0xF6,

    /// arc_move rd, rs - move value without ARC (transfer ownership)
    /// Copies rs to rd without retain, sets rs to null.
    /// Used for last-use optimization.
    /// Format: [rd:4|rs:4] [0]
    arc_move = 0xF7,

    // ============================================
    // Closure Operations (0xF8-0xF9)
    // ============================================

    /// make_closure rd, env_reg, fn_idx - create closure from function and environment
    /// Creates a closure value containing the function index and captured environment.
    /// Format: [rd:4|env_reg:4] [fn_idx:16]
    make_closure = 0xF8,

    /// call_closure rd, closure_reg, argc - call a closure
    /// Extracts function and env from closure, calls with env as first arg.
    /// Format: [rd:4|closure_reg:4] [argc:8]
    call_closure = 0xF9,

    // ============================================
    // Trait Object Operations (0xFA-0xFB)
    // ============================================

    /// make_trait_object rd, src_reg, vtable_idx - create trait object from value
    /// Creates a fat pointer containing the value and vtable reference.
    /// Format: [rd:4|src_reg:4] [vtable_idx:16]
    make_trait_object = 0xFA,

    /// call_trait_method rd, obj_reg, method_idx, argc - call method on trait object
    /// Looks up method in vtable and calls with data as first arg.
    /// Format: [rd:4|obj_reg:4] [method_idx:8] [argc:8]
    call_trait_method = 0xFB,

    /// extended sub_opcode - extended opcode prefix
    /// Format: [sub_opcode:8] [...]
    extended = 0xFE,

    // ============================================
    // Quickened/Specialized Opcodes (0xE0-0xEF)
    // These are runtime-specialized versions of generic ops
    // ============================================

    /// add_int rd, rs1, rs2 - integer-specialized add (no type check)
    /// Format: [rd:4|rs1:4] [rs2:4|0]
    add_int = 0xE0,

    /// sub_int rd, rs1, rs2 - integer-specialized subtract
    sub_int = 0xE1,

    /// mul_int rd, rs1, rs2 - integer-specialized multiply
    mul_int = 0xE2,

    /// div_int rd, rs1, rs2 - integer-specialized divide
    div_int = 0xE3,

    /// cmp_lt_int rd, rs1, rs2 - integer less-than
    cmp_lt_int = 0xE4,

    /// cmp_le_int rd, rs1, rs2 - integer less-equal
    cmp_le_int = 0xE5,

    /// cmp_gt_int rd, rs1, rs2 - integer greater-than
    cmp_gt_int = 0xE6,

    /// cmp_ge_int rd, rs1, rs2 - integer greater-equal
    cmp_ge_int = 0xE7,

    /// cmp_eq_int rd, rs1, rs2 - integer equality
    cmp_eq_int = 0xE8,

    /// cmp_ne_int rd, rs1, rs2 - integer not-equal
    cmp_ne_int = 0xE9,

    /// incr_int rd - integer increment (no type check)
    incr_int = 0xEA,

    /// decr_int rd - integer decrement (no type check)
    decr_int = 0xEB,

    /// fn_datetime rd - rd = current datetime as YYYYMMDDHHMMSSUUUUUU
    fn_datetime = 0xEC,

    _,

    /// Get the size of operands for this opcode (NOT including the opcode byte itself).
    ///
    /// ## Operand Size Calculation Rules
    /// - `[reg:4|reg:4]` = 1 byte (two 4-bit nibbles packed into one byte)
    /// - `[byte]` or `[u8]` = 1 byte
    /// - `[u16]` = 2 bytes (little endian)
    /// - `[u32]` = 4 bytes (little endian)
    ///
    /// ## Common Mistakes to Avoid
    /// - DON'T count each 4-bit field as a full byte: `[rd:4|src:4]` is 1 byte, not 2
    /// - DON'T include the opcode in the count - this returns OPERAND size only
    /// - DO count each distinct byte field: `[a:8] [b:8]` is 2 bytes
    /// - DO add up all u16 as 2 bytes: `[reg:8] [offset:16]` is 3 bytes
    ///
    /// ## Examples
    /// - Format `[rd:4|src:4]` = 1 byte (nibbles packed)
    /// - Format `[rd:4|src:4] [u16]` = 1 + 2 = 3 bytes
    /// - Format `[rd:4|src:4] [u8] [u8]` = 1 + 1 + 1 = 3 bytes
    /// - Format `[u8] [u16] [u16]` = 1 + 2 + 2 = 5 bytes
    pub fn operandSize(self: Opcode) usize {
        return switch (self) {
            // No operands
            .nop, .halt => 0,
            .loop_start, .loop_end, .clear_error_handler => 0,
            .debug_break => 0,

            // 2-byte operands: [reg:4|reg:4] [byte]
            .mov, .movi => 2,
            .load_local, .store_local => 2,
            .add, .sub, .mul, .div, .mod, .neg => 2,
            .addi, .subi, .muli => 2,
            .incr, .decr => 2,
            .add_dec, .sub_dec, .mul_dec, .div_dec => 2,
            .cmp_eq, .cmp_ne, .cmp_lt, .cmp_le, .cmp_gt, .cmp_ge => 2,
            .weak_ref, .weak_load => 2,
            // ARC operations
            .arc_retain, .arc_release => 2,
            .arc_move => 2,
            // Closure operations
            .make_closure => 3, // [rd:4|env_reg:4] [fn_idx:16] = 1 + 2 = 3
            .call_closure => 2, // [rd:4|closure_reg:4] [argc:8] = 1 + 1 = 2
            // Trait object operations
            .make_trait_object => 3, // [rd:4|src_reg:4] [vtable_idx:16] = 1 + 2 = 3
            .call_trait_method => 3, // [rd:4|obj_reg:4] [method_idx:8] [argc:8] = 1 + 1 + 1 = 3
            // Variant (sum type) operations
            .variant_construct => 3, // [rd:4|argc:4] [tag:16]
            .variant_get_tag => 2, // [rd:4|src_reg:4] [0]
            .variant_get_payload => 3, // [rd:4|src_reg:4] [field_idx:16]
            // Quickened integer-specialized opcodes
            .add_int, .sub_int, .mul_int, .div_int => 2,
            .cmp_lt_int, .cmp_le_int, .cmp_gt_int, .cmp_ge_int => 2,
            .cmp_eq_int, .cmp_ne_int => 2,
            .incr_int, .decr_int => 2,
            .cmp_str_eq, .cmp_str_lt, .cmp_str_ne, .cmp_str_le, .cmp_str_gt, .cmp_str_ge => 2,
            .log_and, .log_or, .log_not => 2,
            .bit_and, .bit_or, .bit_xor, .bit_not, .shl, .shr => 2,
            .is_null, .is_type, .select => 2,
            .load_null, .load_true, .load_false => 2,
            .ret, .ret_val, .throw, .push_arg_reg => 2,
            .free_record, .clear_record => 2,
            .load_field_fast, .store_field_fast => 2,
            .str_concat, .str_len, .str_index, .str_slice => 2,
            .str_slice_store, .str_trim, .str_upper, .str_lower => 2,
            .str_find, .str_replace, .str_setchar => 2,
            .to_int, .to_str, .to_bool, .to_dec, .to_char, .format_decimal, .parse_decimal => 2,
            .array_load, .array_store, .array_len => 3, // [idx_reg/regs:8] [slot:16]
            .array_load_opt => 5, // [idx_reg:8] [slot:16] [len:16]
            .array_slice => 4, // [rd:4|0] [start_reg:4|end_reg:4] [slot_lo:8] [slot_hi:8]
            .fn_abs, .fn_sqrt, .fn_sin, .fn_cos, .fn_tan => 2,
            .fn_log, .fn_log10, .fn_exp, .fn_round, .fn_trunc => 2,
            .fn_date, .fn_time, .fn_datetime, .fn_size, .fn_instr, .fn_mem, .fn_error => 2,
            .print, .println, .readln => 2,
            .readkey, .log => 2,
            .call_indirect => 2,
            .assert => 2,
            .extended => 1, // sub_opcode byte, then variable
            // Map operations
            .map_new, .map_set, .map_get, .map_delete => 2,
            .map_has, .map_len, .map_clear => 2,
            .map_keys, .map_values => 2,
            .map_get_at, .map_set_at, .map_key_at => 2,
            // List operations
            .list_new, .list_push, .list_pop => 2,
            .list_get, .list_set => 2,
            .list_len, .list_clear, .list_to_slice => 2,
            // List struct operations (4 bytes: opcode + field_count + slot16)
            .list_push_struct, .list_get_struct, .list_pop_struct, .list_set_struct => 4,
            // Map struct operations (4 bytes: opcode + field_count + slot16/base_reg)
            .map_set_struct, .map_get_struct => 4,

            // 3-byte operands: [reg:4|0] [u16]
            .movi16, .load_const => 3,
            .load_local16, .store_local16 => 3,
            .load_global, .store_global => 3,
            .jmp, .jz, .jnz => 3,
            .jeq, .jne, .jlt, .jge => 3,
            .set_error_handler => 3,
            .call, .call_external, .call_native, .call_dynamic => 3,
            .push_arg, .pop_arg => 2, // [slot:16] = 2 operand bytes
            .new_record, .load_field, .store_field => 3,
            .to_fixed_string => 3,
            .debug_line => 3, // [0] [line:16] = 3 operand bytes
            .ptr_offset => 3, // [rd:4|rs:4] [offset:16]

            // 5-byte operands: [reg:4|0] [u32] or [reg:4|0] [u16] [u16]
            .movi32, .jmp32 => 5,
            .load_record_buf, .store_record_buf => 5,

            // 3-byte operands: [u8] [u16]
            .alloc_buffer => 3,

            // Unknown opcodes
            _ => 0,
        };
    }

    /// Get human-readable name
    pub fn name(self: Opcode) []const u8 {
        return @tagName(self);
    }
};

test "opcode operand sizes" {
    const testing = std.testing;

    try testing.expectEqual(@as(usize, 0), Opcode.nop.operandSize());
    try testing.expectEqual(@as(usize, 2), Opcode.mov.operandSize());
    try testing.expectEqual(@as(usize, 2), Opcode.add.operandSize());
    try testing.expectEqual(@as(usize, 3), Opcode.load_const.operandSize());
    try testing.expectEqual(@as(usize, 3), Opcode.jmp.operandSize());
    try testing.expectEqual(@as(usize, 5), Opcode.movi32.operandSize());
}
