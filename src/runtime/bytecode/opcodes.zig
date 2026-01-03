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

    // ============================================
    // Function Calls (0x70-0x7F)
    // ============================================

    /// call routine_idx, argc - call function
    /// Args in r0..r(argc-1), return in r15
    /// Format: [argc:4|0] [routine_idx:16]
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

    /// ret - return void
    /// Format: [0] [0]
    ret = 0x75,

    /// ret_val rs - return value in rs
    /// Format: [rs:4|0] [0]
    ret_val = 0x76,

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

    /// load_record_buf rd, type_idx, local_base - serialize record fields to buffer
    /// Format: [rd:4|0] [type_idx:16] [local_base:16]
    /// Serializes fields from locals[local_base..] using type definition, result in rd
    load_record_buf = 0x86,

    /// store_record_buf type_idx, local_base - deserialize buffer to record fields
    /// Format: [type_idx:16] [local_base:16]
    store_record_buf = 0x87,

    /// clear_record rs - clear record fields
    /// Format: [rs:4|0] [0]
    clear_record = 0x88,

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

    /// array_load rd, arr_reg, idx_reg - rd = arr[idx]
    /// Format: [rd:4|arr_reg:4] [idx_reg:4|0]
    array_load = 0xB0,

    /// array_store arr_reg, idx_reg, val_reg - arr[idx] = val
    /// Format: [arr_reg:4|idx_reg:4] [val_reg:4|0]
    array_store = 0xB1,

    /// array_len rd, arr_reg - rd = len(arr)
    /// Format: [rd:4|arr_reg:4] [0]
    array_len = 0xB2,

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
    // Console I/O (0xD0-0xDF)
    // ============================================

    /// console_write rs, argc - write rs (with argc format args following)
    /// Format: [rs:4|argc:4] [0]
    console_write = 0xD0,

    /// console_writeln rs, argc - write rs with newline
    /// Format: [rs:4|argc:4] [0]
    console_writeln = 0xD1,

    /// console_read rd - rd = read line from console
    /// Format: [rd:4|0] [0]
    console_read = 0xD2,

    /// console_readkey rd - rd = read single key
    /// Format: [rd:4|0] [0]
    console_readkey = 0xD3,

    /// console_log rs, argc - log to dev pane
    /// Format: [rs:4|argc:4] [0]
    console_log = 0xD4,

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

    _,

    /// Get the size of operands for this opcode
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
            // Quickened integer-specialized opcodes
            .add_int, .sub_int, .mul_int, .div_int => 2,
            .cmp_lt_int, .cmp_le_int, .cmp_gt_int, .cmp_ge_int => 2,
            .cmp_eq_int, .cmp_ne_int => 2,
            .incr_int, .decr_int => 2,
            .cmp_str_eq, .cmp_str_lt, .cmp_str_ne, .cmp_str_le, .cmp_str_gt, .cmp_str_ge => 2,
            .log_and, .log_or, .log_not => 2,
            .bit_and, .bit_or, .bit_xor, .bit_not => 2,
            .is_null, .select => 2,
            .load_null, .load_true, .load_false => 2,
            .ret, .ret_val => 2,
            .free_record, .clear_record => 2,
            .load_field_fast, .store_field_fast => 2,
            .str_concat, .str_len, .str_index, .str_slice => 2,
            .str_slice_store, .str_trim, .str_upper, .str_lower => 2,
            .str_find, .str_replace, .str_setchar => 2,
            .to_int, .to_str, .to_bool, .to_dec, .to_char, .format_decimal, .parse_decimal => 2,
            .array_load, .array_store, .array_len => 2,
            .fn_abs, .fn_sqrt, .fn_sin, .fn_cos, .fn_tan => 2,
            .fn_log, .fn_log10, .fn_exp, .fn_round, .fn_trunc => 2,
            .fn_date, .fn_time, .fn_size, .fn_instr, .fn_mem, .fn_error => 2,
            .console_write, .console_writeln, .console_read => 2,
            .console_readkey, .console_log => 2,
            .call_indirect => 2,
            .assert => 2,
            .extended => 1, // sub_opcode byte, then variable
            // Map operations
            .map_new, .map_set, .map_get, .map_delete => 2,
            .map_has, .map_len, .map_clear => 2,
            .map_keys, .map_values => 2,
            .map_get_at, .map_set_at => 2,

            // 3-byte operands: [reg:4|0] [u16]
            .movi16, .load_const => 3,
            .load_local16, .store_local16 => 3,
            .load_global, .store_global => 3,
            .jmp, .jz, .jnz => 3,
            .jeq, .jne, .jlt, .jge => 3,
            .set_error_handler => 3,
            .call, .call_external, .call_native, .call_dynamic => 3,
            .new_record, .load_field, .store_field => 3,
            .to_fixed_string => 3,
            .debug_line => 3,
            .ptr_offset => 3, // [rd:4|rs:4] [offset:16]

            // 4-byte operands: [u16] [u16]
            .store_record_buf => 4,

            // 5-byte operands: [reg:4|0] [u32] or [reg:4|0] [u16] [u16]
            .movi32, .jmp32 => 5,
            .load_record_buf => 5,

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
