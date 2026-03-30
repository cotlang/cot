//! CIR binary format reader.
//!
//! Deserializes the CIR binary format (spec: src/claude/CIR_FORMAT_SPEC.md)
//! into an intermediate representation that can be translated to Cranelift IR.

use std::collections::HashMap;

// -- Constants --

pub const MAGIC: u32 = 0x00434952; // "CIR\0" little-endian
pub const VERSION_1_0: u32 = 0x00010000;

// Section IDs
pub const SECTION_STRING_HEAP: u16 = 0x01;
pub const SECTION_TYPE_DECLS: u16 = 0x02;
pub const SECTION_TYPE_META: u16 = 0x03;
pub const SECTION_GLOBALS: u16 = 0x04;
pub const SECTION_FUNC_DECLS: u16 = 0x05;
pub const SECTION_FUNC_DEFS: u16 = 0x06;
pub const SECTION_DEBUG_INFO: u16 = 0x07;
pub const SECTION_DECORATIONS: u16 = 0x08;

// Pre-registered type indices (match Zig TypeRegistry)
pub const TYPE_INVALID: u32 = 0;
pub const TYPE_BOOL: u32 = 1;
pub const TYPE_I8: u32 = 2;
pub const TYPE_I16: u32 = 3;
pub const TYPE_I32: u32 = 4;
pub const TYPE_I64: u32 = 5;
pub const TYPE_U8: u32 = 6;
pub const TYPE_U16: u32 = 7;
pub const TYPE_U32: u32 = 8;
pub const TYPE_U64: u32 = 9;
pub const TYPE_F32: u32 = 10;
pub const TYPE_F64: u32 = 11;
pub const TYPE_VOID: u32 = 12;
pub const TYPE_STRING: u32 = 17;
pub const TYPE_NORETURN: u32 = 22;

// CIR opcodes — integer arithmetic
pub const OP_ADD: u16 = 0x0010;
pub const OP_SUB: u16 = 0x0011;
pub const OP_MUL: u16 = 0x0012;
pub const OP_DIV: u16 = 0x0013;
pub const OP_UDIV: u16 = 0x0014;
pub const OP_MOD: u16 = 0x0015;
pub const OP_UMOD: u16 = 0x0016;
pub const OP_NEG: u16 = 0x0017;

// Bitwise
pub const OP_AND: u16 = 0x0020;
pub const OP_OR: u16 = 0x0021;
pub const OP_XOR: u16 = 0x0022;
pub const OP_SHL: u16 = 0x0023;
pub const OP_SHR: u16 = 0x0024;
pub const OP_SAR: u16 = 0x0025;
pub const OP_NOT: u16 = 0x0026;
pub const OP_POPCNT: u16 = 0x0028;
pub const OP_CLZ: u16 = 0x0029;
pub const OP_CTZ: u16 = 0x002A;
pub const OP_BOOL_NOT: u16 = 0x0027;

// Comparison
pub const OP_EQ: u16 = 0x0030;
pub const OP_NE: u16 = 0x0031;
pub const OP_LT: u16 = 0x0032;
pub const OP_LE: u16 = 0x0033;
pub const OP_GT: u16 = 0x0034;
pub const OP_GE: u16 = 0x0035;
pub const OP_ULT: u16 = 0x0036;
pub const OP_ULE: u16 = 0x0037;
pub const OP_UGT: u16 = 0x0038;
pub const OP_UGE: u16 = 0x0039;

// Constants
pub const OP_CONST_BOOL: u16 = 0x0000;
pub const OP_CONST_INT: u16 = 0x0001;
pub const OP_CONST_FLOAT: u16 = 0x0002;
pub const OP_CONST_NIL: u16 = 0x0003;

// Memory
pub const OP_LOAD: u16 = 0x0070;
pub const OP_STORE: u16 = 0x0071;
pub const OP_LOCAL_ADDR: u16 = 0x0080;
pub const OP_GLOBAL_VALUE: u16 = 0x0081;
pub const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
pub const OP_GLOBAL_VALUE_IADD: u16 = 0x00B2;
pub const OP_OFF_PTR: u16 = 0x0083;

// Control flow
pub const OP_PHI: u16 = 0x0090;
pub const OP_COPY: u16 = 0x0091;
pub const OP_ARG: u16 = 0x0092;
pub const OP_STATIC_CALL: u16 = 0x0093;
pub const OP_CALL: u16 = 0x0094;
pub const OP_RET: u16 = 0x0097;
pub const OP_RET_VOID: u16 = 0x0098;

// Control flow — branches
pub const OP_JUMP: u16 = 0x00A0;
pub const OP_BRIF: u16 = 0x00A1;
pub const OP_COND_SELECT: u16 = 0x0099;

pub const OP_TRAP: u16 = 0x00A2;
pub const OP_BR_TABLE: u16 = 0x00A4;
pub const OP_COND_TRAP: u16 = 0x00A3;

// Stack slots
pub const OP_STACK_SLOT_DECL: u16 = 0x00B0;

// ARC
pub const OP_RETAIN: u16 = 0x00C0;
pub const OP_RELEASE: u16 = 0x00C1;
pub const OP_ALLOC: u16 = 0x00C2;
pub const OP_DEALLOC: u16 = 0x00C3;

// Type conversion
pub const OP_UEXTEND: u16 = 0x0040;
pub const OP_SEXTEND: u16 = 0x0041;
pub const OP_IREDUCE: u16 = 0x0042;
pub const OP_FCVT_FROM_SINT: u16 = 0x0043;
pub const OP_FCVT_TO_SINT_SAT: u16 = 0x0044;
pub const OP_FCVT_FROM_UINT: u16 = 0x0045;
pub const OP_FPROMOTE: u16 = 0x0046;
pub const OP_FDEMOTE: u16 = 0x0047;
pub const OP_FCVT_TO_UINT: u16 = 0x0048;

// Float arithmetic
pub const OP_ADD_F: u16 = 0x0050;
pub const OP_SUB_F: u16 = 0x0051;
pub const OP_MUL_F: u16 = 0x0052;
pub const OP_DIV_F: u16 = 0x0053;
pub const OP_NEG_F: u16 = 0x0054;
pub const OP_FABS: u16 = 0x0055;
pub const OP_SQRT: u16 = 0x0056;
pub const OP_CEIL: u16 = 0x0057;
pub const OP_FLOOR: u16 = 0x0058;
pub const OP_TRUNC_F: u16 = 0x0059;
pub const OP_NEAREST: u16 = 0x005A;
pub const OP_FMIN: u16 = 0x005B;
pub const OP_FMAX: u16 = 0x005C;
pub const OP_FCOPYSIGN: u16 = 0x005D;

// Function address (for function pointers)
pub const OP_FUNC_ADDR: u16 = 0x0095;

// Float comparison
pub const OP_EQ_F: u16 = 0x0060;
pub const OP_NE_F: u16 = 0x0061;
pub const OP_LT_F: u16 = 0x0062;
pub const OP_LE_F: u16 = 0x0063;
pub const OP_GT_F: u16 = 0x0064;
pub const OP_GE_F: u16 = 0x0065;

// -- Data structures --

/// A parsed CIR module.
pub struct CirModule {
    pub strings: StringHeap,
    pub functions: Vec<CirFunction>,
    pub globals: Vec<CirGlobal>,
    pub bound: u32,
}

pub struct StringHeap {
    data: Vec<u8>,
}

impl StringHeap {
    pub fn get(&self, offset: u32) -> &str {
        let off = offset as usize;
        if off >= self.data.len() {
            return "";
        }
        let len = u32::from_le_bytes([
            self.data[off],
            self.data[off + 1],
            self.data[off + 2],
            self.data[off + 3],
        ]) as usize;
        let start = off + 4;
        let end = start + len;
        std::str::from_utf8(&self.data[start..end]).unwrap_or("")
    }
}

pub struct CirFunction {
    pub name_offset: u32,
    pub param_count: u32,
    pub return_count: u32,
    pub flags: u32,
    pub param_types: Vec<u32>,
    pub return_types: Vec<u32>,
    pub blocks: Vec<CirBlock>,
}

pub struct CirBlock {
    pub id: u32,
    pub kind: u8,
    pub successors: Vec<u32>,
    pub predecessors: Vec<u32>,
    pub instructions: Vec<CirInst>,
}

pub struct CirInst {
    pub opcode: u16,
    pub words: Vec<u32>, // raw operand words (after word 0)
}

impl CirInst {
    /// Result ID (word 1 for most instructions)
    pub fn result_id(&self) -> u32 {
        if self.words.is_empty() { 0 } else { self.words[0] }
    }

    /// Type index (word 2 for most instructions)
    pub fn type_idx(&self) -> u32 {
        if self.words.len() < 2 { 0 } else { self.words[1] }
    }
}

pub struct CirGlobal {
    pub name_offset: u32,
    pub type_idx: u32,
    pub size: u32,
}

// -- Reader --

pub struct CirReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> CirReader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    pub fn read_module(&mut self) -> Result<CirModule, String> {
        // Header
        let magic = self.read_u32()?;
        if magic != MAGIC {
            return Err(format!("bad magic: 0x{magic:08X}, expected 0x{MAGIC:08X}"));
        }
        let version = self.read_u32()?;
        if (version >> 16) != 1 {
            return Err(format!("unsupported major version: {}", version >> 16));
        }
        let _generator = self.read_u32()?;
        let bound = self.read_u32()?;
        let _reserved = self.read_u32()?;

        let mut strings = StringHeap { data: vec![] };
        let mut functions = vec![];
        let mut globals = vec![];

        // Read sections
        while self.pos < self.data.len() {
            let section_word = self.read_u32()?;
            let section_word_count = (section_word >> 16) as usize;
            let section_id = (section_word & 0xFFFF) as u16;

            let section_start = self.pos;
            let section_byte_len = (section_word_count.saturating_sub(1)) * 4;

            match section_id {
                SECTION_STRING_HEAP => {
                    let byte_count = self.read_u32()? as usize;
                    let heap_start = self.pos;
                    strings = StringHeap {
                        data: self.data[heap_start..heap_start + byte_count].to_vec(),
                    };
                    // Advance past heap data (padded to word alignment)
                    self.pos = heap_start + ((byte_count + 3) / 4) * 4;
                }
                SECTION_FUNC_DEFS => {
                    // Section word count may be 0 (placeholder) — read until end of data
                    let end = if section_word_count > 0 {
                        section_start + section_byte_len
                    } else {
                        self.data.len()
                    };
                    functions = self.read_functions(end)?;
                }
                SECTION_GLOBALS => {
                    let end = if section_word_count > 0 {
                        section_start + section_byte_len
                    } else {
                        self.data.len()
                    };
                    globals = self.read_globals(end)?;
                }
                _ => {
                    // Skip unknown sections
                    if section_word_count > 0 {
                        self.pos = section_start + section_byte_len;
                    }
                }
            }
        }

        Ok(CirModule { strings, functions, globals, bound })
    }

    fn read_functions(&mut self, end: usize) -> Result<Vec<CirFunction>, String> {
        let mut funcs = vec![];
        while self.pos < end {
            let header = self.read_u32()?;
            let opcode = (header & 0xFFFF) as u16;

            // FUNC_BEGIN marker
            if opcode == 0xFF00 {
                let word_count = (header >> 16) as usize;
                let name_offset = self.read_u32()?;
                let param_count = self.read_u32()? as usize;
                let return_count = self.read_u32()? as usize;
                let _block_count = self.read_u32()?;
                let flags = self.read_u32()?;

                // Read param types
                let mut param_types = Vec::with_capacity(param_count);
                for _ in 0..param_count {
                    param_types.push(self.read_u32()?);
                }
                // Read return types
                let mut return_types = Vec::with_capacity(return_count);
                for _ in 0..return_count {
                    return_types.push(self.read_u32()?);
                }

                // Skip any remaining words in the header (future extensibility)
                let consumed = 5 + param_count + return_count; // 5 fixed fields
                let remaining = word_count.saturating_sub(1 + consumed); // -1 for header word
                for _ in 0..remaining {
                    let _ = self.read_u32()?;
                }

                let blocks = self.read_blocks()?;

                funcs.push(CirFunction {
                    name_offset,
                    param_count: param_count as u32,
                    return_count: return_count as u32,
                    flags,
                    param_types,
                    return_types,
                    blocks,
                });
            }
        }
        Ok(funcs)
    }

    fn read_blocks(&mut self) -> Result<Vec<CirBlock>, String> {
        let mut blocks = vec![];
        loop {
            if self.pos >= self.data.len() { break; }
            let header = self.read_u32()?;
            let opcode = (header & 0xFFFF) as u16;

            // FUNC_END
            if opcode == 0xFF01 { break; }

            // BLOCK_BEGIN (0xFF02)
            if opcode == 0xFF02 {
                let id = self.read_u32()?;
                let kind = self.read_u32()? as u8;
                let succ_count = self.read_u32()? as usize;
                let mut successors = Vec::with_capacity(succ_count);
                for _ in 0..succ_count {
                    successors.push(self.read_u32()?);
                }
                let pred_count = self.read_u32()? as usize;
                let mut predecessors = Vec::with_capacity(pred_count);
                for _ in 0..pred_count {
                    predecessors.push(self.read_u32()?);
                }

                let instructions = self.read_instructions()?;
                blocks.push(CirBlock { id, kind, successors, predecessors, instructions });
            }
        }
        Ok(blocks)
    }

    fn read_instructions(&mut self) -> Result<Vec<CirInst>, String> {
        let mut insts = vec![];
        loop {
            if self.pos >= self.data.len() { break; }

            // Peek at next word without advancing
            let word0 = u32::from_le_bytes([
                self.data[self.pos],
                self.data[self.pos + 1],
                self.data[self.pos + 2],
                self.data[self.pos + 3],
            ]);
            let opcode = (word0 & 0xFFFF) as u16;

            // BLOCK_END (0xFF03) or BLOCK_BEGIN or FUNC_END — stop reading instructions
            if opcode == 0xFF03 || opcode == 0xFF02 || opcode == 0xFF01 {
                if opcode == 0xFF03 {
                    self.pos += 4; // consume BLOCK_END
                }
                break;
            }

            // Read instruction
            self.pos += 4; // consume word 0
            let word_count = (word0 >> 16) as usize;
            let operand_count = word_count.saturating_sub(1);
            let mut words = Vec::with_capacity(operand_count);
            for _ in 0..operand_count {
                words.push(self.read_u32()?);
            }
            insts.push(CirInst { opcode, words });
        }
        Ok(insts)
    }

    fn read_globals(&mut self, end: usize) -> Result<Vec<CirGlobal>, String> {
        let mut globals = vec![];
        while self.pos < end {
            let name_offset = self.read_u32()?;
            let type_idx = self.read_u32()?;
            let size = self.read_u32()?;
            globals.push(CirGlobal { name_offset, type_idx, size });
        }
        Ok(globals)
    }

    fn read_u32(&mut self) -> Result<u32, String> {
        if self.pos + 4 > self.data.len() {
            return Err(format!("unexpected end of CIR at offset {}", self.pos));
        }
        let val = u32::from_le_bytes([
            self.data[self.pos],
            self.data[self.pos + 1],
            self.data[self.pos + 2],
            self.data[self.pos + 3],
        ]);
        self.pos += 4;
        Ok(val)
    }
}

// -- CIR Writer (for tests) --

/// Build a CIR binary blob for testing. Used by Rust tests to create
/// CIR input without needing the Zig serializer.
pub struct CirWriter {
    data: Vec<u32>,
    string_heap: Vec<u8>,
    string_offsets: HashMap<String, u32>,
}

impl CirWriter {
    pub fn new(bound: u32) -> Self {
        Self {
            data: vec![MAGIC, VERSION_1_0, 0, bound, 0], // header
            string_heap: vec![],
            string_offsets: HashMap::new(),
        }
    }

    /// Intern a string and return its offset in the heap.
    pub fn intern_string(&mut self, s: &str) -> u32 {
        if let Some(&off) = self.string_offsets.get(s) {
            return off;
        }
        let offset = self.string_heap.len() as u32;
        let len = s.len() as u32;
        self.string_heap.extend_from_slice(&len.to_le_bytes());
        self.string_heap.extend_from_slice(s.as_bytes());
        // Pad to 4-byte alignment
        while self.string_heap.len() % 4 != 0 {
            self.string_heap.push(0);
        }
        self.string_offsets.insert(s.to_string(), offset);
        offset
    }

    /// Write the string heap section.
    pub fn write_string_heap(&mut self) {
        let heap_bytes = self.string_heap.len();
        let heap_words = (heap_bytes + 3) / 4;
        // Section header: (word_count << 16) | section_id
        // word_count = 1 (header) + 1 (byte_count) + heap_words
        let word_count = 2 + heap_words;
        self.data.push(((word_count as u32) << 16) | SECTION_STRING_HEAP as u32);
        self.data.push(heap_bytes as u32);
        // Copy heap bytes as u32 words
        for chunk in self.string_heap.chunks(4) {
            let mut word = [0u8; 4];
            word[..chunk.len()].copy_from_slice(chunk);
            self.data.push(u32::from_le_bytes(word));
        }
    }

    /// Begin function definitions section.
    pub fn begin_func_defs(&mut self) {
        // Placeholder — we'll patch the word count later
        self.data.push(((0u32) << 16) | SECTION_FUNC_DEFS as u32);
    }

    /// Begin a function with full signature.
    /// Format: name_offset, param_count, return_count, block_count, flags, [param_types...], [return_types...]
    pub fn begin_func(&mut self, name_offset: u32, param_types: &[u32], return_types: &[u32], block_count: u32, flags: u32) {
        let word_count = 1 + 5 + param_types.len() + return_types.len(); // header + 5 fixed + types
        self.data.push(((word_count as u32) << 16) | 0xFF00);
        self.data.push(name_offset);
        self.data.push(param_types.len() as u32);
        self.data.push(return_types.len() as u32);
        self.data.push(block_count);
        self.data.push(flags);
        for &pt in param_types { self.data.push(pt); }
        for &rt in return_types { self.data.push(rt); }
    }

    pub fn end_func(&mut self) {
        self.data.push((1 << 16) | 0xFF01);
    }

    pub fn begin_block(&mut self, id: u32, kind: u8, successors: &[u32], predecessors: &[u32]) {
        let word_count = 5 + successors.len() + predecessors.len();
        self.data.push(((word_count as u32) << 16) | 0xFF02);
        self.data.push(id);
        self.data.push(kind as u32);
        self.data.push(successors.len() as u32);
        for s in successors { self.data.push(*s); }
        self.data.push(predecessors.len() as u32);
        for p in predecessors { self.data.push(*p); }
    }

    pub fn end_block(&mut self) {
        self.data.push((1 << 16) | 0xFF03);
    }

    /// Emit an instruction: (word_count << 16) | opcode, followed by operand words.
    pub fn emit(&mut self, opcode: u16, operands: &[u32]) {
        let word_count = 1 + operands.len();
        self.data.push(((word_count as u32) << 16) | opcode as u32);
        for op in operands { self.data.push(*op); }
    }

    /// Finalize and return bytes.
    pub fn finish(self) -> Vec<u8> {
        // Patch func_defs section word count if present
        // (for simplicity, we won't patch — consumer handles it)
        let mut bytes = Vec::with_capacity(self.data.len() * 4);
        for word in &self.data {
            bytes.extend_from_slice(&word.to_le_bytes());
        }
        bytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_round_trip_header() {
        let writer = CirWriter::new(100);
        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();
        assert_eq!(module.bound, 100);
    }

    #[test]
    fn test_string_heap() {
        let mut writer = CirWriter::new(10);
        let off_hello = writer.intern_string("hello");
        let off_world = writer.intern_string("world");
        writer.write_string_heap();
        let bytes = writer.finish();

        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();
        assert_eq!(module.strings.get(off_hello), "hello");
        assert_eq!(module.strings.get(off_world), "world");
    }

    #[test]
    fn test_function_with_add() {
        let mut writer = CirWriter::new(10);
        let name_off = writer.intern_string("test_add");
        writer.write_string_heap();

        // Function: fn test_add(a: i64, b: i64) -> i64 { return a + b }
        writer.begin_func_defs();
        writer.begin_func(name_off, &[TYPE_I64, TYPE_I64], &[TYPE_I64], 1, 0);
        writer.begin_block(0, 0x05, &[], &[]); // entry block

        // v0 = arg 0 : i64
        writer.emit(OP_ARG, &[0, TYPE_I64, 0]);
        // v1 = arg 1 : i64
        writer.emit(OP_ARG, &[1, TYPE_I64, 1]);
        // v2 = add v0, v1 : i64
        writer.emit(OP_ADD, &[2, TYPE_I64, 0, 1]);
        // ret v2
        writer.emit(OP_RET, &[2]);

        writer.end_block();
        writer.end_func();

        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();

        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.strings.get(module.functions[0].name_offset), "test_add");
        assert_eq!(module.functions[0].param_count, 2);
        assert_eq!(module.functions[0].blocks.len(), 1);
        assert_eq!(module.functions[0].blocks[0].instructions.len(), 4);

        let add_inst = &module.functions[0].blocks[0].instructions[2];
        assert_eq!(add_inst.opcode, OP_ADD);
        assert_eq!(add_inst.result_id(), 2);
        assert_eq!(add_inst.type_idx(), TYPE_I64);
    }
}
