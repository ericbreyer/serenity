use std::fmt::Debug;

use tracing::{ info, instrument, Level };

use crate::{
    typing::UValueType,
    value::{pointer::Pointer, { ConstantPool, Value }},
    dyn_event,
    dyn_span,
};

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Opcode {
    NoOp,
    Return,
    Constant,
    ConstantLong,
    NegateInt,
    NegateFloat,
    AddInt,
    AddUint,
    AddFloat,
    Concat,
    PointerAdd,
    SubtractInt,
    SubtractUint,
    SubtractFloat,
    PointerSubtract,

    MultiplyInt,
    MultiplyUint,
    MultiplyFloat,
    DivideInt,
    DivideUint,
    DivideFloat,
    Ternary,
    False,
    Nil,
    True,
    Not,
    Equal,
    GreaterInt,
    GreaterUint,
    GreaterFloat,
    LessInt,
    LessUint,
    LessFloat,
    
    PrintInt,
    PrintUint,
    PrintFloat,
    PrintSharedString,
    PrintBool,
    PrintPointer,
    PrintNil,
    PrintChar,
    Pop,
    // PopClosure,
    DefineGlobal,
    DefineGlobalLong,
    GetGlobal,
    GetGlobalLong,
    GetLocal,
    JumpIfFalse,
    JumpIfTrue,
    Jump,
    Loop,
    Call,
    RefLocal,
    RefGlobal,
    DerefGet,
    DerefAssign,
    DefineStackArray,
    DefineGlobalArray,
    // Cloxsure,
    // CopyClosure,
    // GetUpvalue,
    GetField,

    CastIntToFloat,
    CastFloatToInt,
    CastIntToSharedString,
    CastFloatToSharedString,
    CastBoolToFloat,
    CastBoolToInt,
    CastBoolToSharedString,
    CastIntToBool,
    CastBoolToUint,
    CastUintToBool,
    CastUintToInt,
    CastIntToUint,
    CastUintToFloat,

    FrameBase,

    Break,
    Continue,
    
}

impl From<u8> for Opcode {
    fn from(m: u8) -> Opcode {
        unsafe { std::mem::transmute(m) }
    }
}

impl From<Opcode> for u8 {
    fn from(m: Opcode) -> u8 {
        m as u8
    }
}

impl Opcode {
    fn next(&self) -> Opcode {
        unsafe { std::mem::transmute((*self as u8) + 1) }
    }
}

pub struct Chunk {
    pub code: Vec<(u8, usize)>, // code, line
    pub constants: ConstantPool,
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.disassemble("chunk", Level::DEBUG);
        write!(f, "")
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: ConstantPool::new(),
        }
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push((byte, line));
    }

    pub fn write_constant(&mut self, value: Value, line: usize) {
        let id = self.add_constant(value);
        self.write_pool_opcode(Opcode::Constant, id as i32, line);
    }

    pub fn write_pool_opcode(&mut self, code: Opcode, pool_id: i32, line: usize) {
        if pool_id > 255 {
            self.write(code.next().into(), line);
            self.write(pool_id as u8, line);
            self.write((pool_id >> 8) as u8, line);
            self.write((pool_id >> 16) as u8, line);
        } else {
            self.write(code.into(), line);
            self.write(pool_id as u8, line);
        }
    }

    pub fn write_pop(&mut self, t: UValueType, line: usize) {
        // if t == ValueType::AnyFunction.intern() {
        //     self.write(Opcode::PopClosure.into(), line);
        // } else {
            self.write(Opcode::Pop.into(), line);
            self.write(t.num_words() as u8, line);
        // }
    }

    pub fn define_global(&mut self, global_id: u32, line: usize, t: UValueType) {
        if global_id > 255 {
            self.write(Opcode::DefineGlobalLong.into(), line);
            self.write(global_id as u8, line);
            self.write((global_id >> 8) as u8, line);
            self.write((global_id >> 16) as u8, line);
        } else {
            self.write(Opcode::DefineGlobal.into(), line);
            self.write(global_id as u8, line);
            self.write(t.num_words() as u8, line);
        }
    }

    pub fn add_constant(&mut self, value: Value) -> u32 {
        
        self.constants.write(value)
    }

    pub fn disassemble(&self, name: &str, trace_level: Level) {
        let _guard = dyn_span!(trace_level, "disassemble", name = name).entered();

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset, trace_level);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize, trace_level: Level) -> usize {

        let (instruction, _) = self.code[offset];
        match Opcode::from(instruction) {
            Opcode::NoOp => {
                self.simple_instruction("OP_NOOP", offset, trace_level)
            }
            Opcode::Return => {
                self.byte_instruction("OP_RETURN", offset, trace_level)
            }
            Opcode::Constant => {
                self.constant_instruction("OP_CONSTANT", offset, trace_level)
            }
            Opcode::ConstantLong => {
                self.constant_long_instruction("OP_CONSTANT_LONG", offset, trace_level)
            }
            Opcode::NegateInt => {
                self.simple_instruction("OP_NEGATE", offset, trace_level)
            }
            Opcode::NegateFloat => {
                self.simple_instruction("OP_NEGATE_FLOAT", offset, trace_level)
            }
            Opcode::AddInt => {
                self.simple_instruction("OP_ADD", offset, trace_level)
            }
            Opcode::AddUint => {
                self.simple_instruction("OP_ADD_UINT", offset, trace_level)
            }
            Opcode::AddFloat => {
                self.simple_instruction("OP_ADD_FLOAT", offset, trace_level)
            }
            Opcode::Concat => {
                self.simple_instruction("OP_CONCAT", offset, trace_level)
            }
            Opcode::PointerAdd => {
                self.byte_instruction("OP_POINTER_ADD", offset, trace_level)
            }
            Opcode::SubtractInt => {
                self.simple_instruction("OP_SUBTRACT", offset, trace_level)
            }
            Opcode::SubtractUint => {
                self.simple_instruction("OP_SUBTRACT_UINT", offset, trace_level)
            }
            Opcode::SubtractFloat => {
                self.simple_instruction("OP_SUBTRACT_FLOAT", offset, trace_level)
            }
            Opcode::PointerSubtract => {
                self.byte_instruction("OP_POINTER_SUBTRACT", offset, trace_level)
            }
            Opcode::MultiplyInt => {
                self.simple_instruction("OP_MULTIPLY", offset, trace_level)
            }
            Opcode::MultiplyUint => {
                self.simple_instruction("OP_MULTIPLY_UINT", offset, trace_level)
            }
            Opcode::MultiplyFloat => {
                self.simple_instruction("OP_MULTIPLY_FLOAT", offset, trace_level)
            }
            Opcode::DivideInt => {
                self.simple_instruction("OP_DIVIDE", offset, trace_level)
            }
            Opcode::DivideUint => {
                self.simple_instruction("OP_DIVIDE_UINT", offset, trace_level)
            }
            Opcode::DivideFloat => {
                self.simple_instruction("OP_DIVIDE_FLOAT", offset, trace_level)
            }
            Opcode::Ternary => {
                self.simple_instruction("OP_TERNARY", offset, trace_level)
            }
            Opcode::False => {
                self.simple_instruction("OP_FALSE", offset, trace_level)
            }
            Opcode::Nil => {
                self.simple_instruction("OP_NIL", offset, trace_level)
            }
            Opcode::True => {
                self.simple_instruction("OP_TRUE", offset, trace_level)
            }
            Opcode::Not => {
                self.simple_instruction("OP_NOT", offset, trace_level)
            }
            Opcode::Equal => {
                self.simple_instruction("OP_EQUAL", offset, trace_level)
            }
            Opcode::GreaterInt => {
                self.simple_instruction("OP_GREATER", offset, trace_level)
            }
            Opcode::GreaterUint => {
                self.simple_instruction("OP_GREATER_UINT", offset, trace_level)
            }
            Opcode::GreaterFloat => {
                self.simple_instruction("OP_GREATER_FLOAT", offset, trace_level)
            }
            Opcode::LessInt => {
                self.simple_instruction("OP_LESS", offset, trace_level)
            }
            Opcode::LessUint => {
                self.simple_instruction("OP_LESS_UINT", offset, trace_level)
            }
            Opcode::LessFloat => {
                self.simple_instruction("OP_LESS_FLOAT", offset, trace_level)
            }
            Opcode::PrintInt => {
                self.simple_instruction("OP_PRINT_INT", offset, trace_level)
            }
            Opcode::PrintUint => {
                self.simple_instruction("OP_PRINT_UINT", offset, trace_level)
            }
            Opcode::PrintFloat => {
                self.simple_instruction("OP_PRINT_FLOAT", offset, trace_level)
            }
            Opcode::PrintSharedString => {
                self.simple_instruction("OP_PRINT_SharedString", offset, trace_level)
            }
            Opcode::PrintBool => {
                self.simple_instruction("OP_PRINT_BOOL", offset, trace_level)
            }
            Opcode::PrintPointer => {
                self.simple_instruction("OP_PRINT_POINTER", offset, trace_level)
            }
            Opcode::PrintNil => {
                self.simple_instruction("OP_PRINT_NIL", offset, trace_level)
            }
            Opcode::PrintChar => {
                self.simple_instruction("OP_PRINT_CHAR", offset, trace_level)
            }
            Opcode::GetField => {
                self.three_byte_instruction("OP_GET_FIELD", offset, trace_level)
            }
            Opcode::Pop => {
                self.byte_instruction("OP_POP", offset, trace_level)
            }
            Opcode::DefineGlobal => {
                self.two_byte_instruction("OP_DEFINE_GLOBAL", offset, trace_level)
            }
            Opcode::DefineGlobalLong => {
                self.constant_long_instruction("OP_DEFINE_GLOBAL_LONG", offset, trace_level)
            }
            Opcode::GetGlobal => {
                self.two_byte_instruction("OP_GET_GLOBAL", offset, trace_level)
            }
            Opcode::GetGlobalLong => {
                self.constant_long_instruction("OP_GET_GLOBAL_LONG", offset, trace_level)
            }
            Opcode::GetLocal => {
                self.two_byte_instruction("OP_GET_LOCAL", offset, trace_level)
            }
            Opcode::JumpIfFalse => {
                self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset, trace_level)
            }
            Opcode::JumpIfTrue => {
                self.jump_instruction("OP_JUMP_IF_TRUE", 1, offset, trace_level)
            }
            Opcode::Jump => {
                self.jump_instruction("OP_JUMP", 1, offset, trace_level)
            }
            Opcode::Loop => {
                self.jump_instruction("OP_LOOP", -1, offset, trace_level)
            }
            Opcode::Call => {
                self.two_byte_instruction("OP_CALL", offset, trace_level)
            }
            Opcode::RefLocal => {
                self.simple_instruction("OP_REF", offset, trace_level)
            }
            Opcode::RefGlobal => {
                self.byte_instruction("OP_REF_GLOBAL", offset, trace_level)
            }
            Opcode::DerefGet => {
                self.byte_instruction("OP_DEREF_GET", offset, trace_level)
            }
            Opcode::DerefAssign => {
                self.byte_instruction("OP_DEREF_ASSIGN", offset, trace_level)
            }
            Opcode::DefineStackArray => {
                self.simple_instruction("OP_DefineStackArray", offset, trace_level)
            }
            Opcode::DefineGlobalArray => {
                self.three_byte_instruction("OP_DefineGlobalArray", offset, trace_level)
            }
            // Opcode::Closure => {
            //     let (fid, _) = self.code[offset + 1];
            //     let (count_upvals, _) = self.code[offset + 2];
            //     dyn_event!(
            //         trace_level,
            //         "{:16} {:?} {:?}",
            //         "OP_CLOSURE",
            //         fid as usize,
            //         count_upvals
            //     );

            //     offset + 3
            // }
            // Opcode::GetUpvalue => {
            //     self.three_byte_instruction("OP_GET_UPVALUE", offset, trace_level)
            // }
            Opcode::CastIntToFloat => {
                self.simple_instruction("OP_CAST_INT_TO_FLOAT", offset, trace_level)
            }
            Opcode::CastFloatToInt => {
                self.simple_instruction("OP_CAST_FLOAT_TO_INT", offset, trace_level)
            }
            Opcode::CastIntToSharedString => {
                self.simple_instruction("OP_CAST_INT_to_string", offset, trace_level)
            }
            Opcode::CastFloatToSharedString => {
                self.simple_instruction("OP_CAST_FLOAT_to_string", offset, trace_level)
            }
            Opcode::CastBoolToFloat => {
                self.simple_instruction("OP_CAST_BOOL_TO_FLOAT", offset, trace_level)
            }
            Opcode::CastBoolToInt => {
                self.simple_instruction("OP_CAST_BOOL_TO_INT", offset, trace_level)
            }
            Opcode::CastBoolToSharedString => {
                self.simple_instruction("OP_CAST_BOOL_to_string", offset, trace_level)
            }
            Opcode::CastIntToBool => {
                self.simple_instruction("OP_CAST_INT_TO_BOOL", offset, trace_level)
            }
            Opcode::CastBoolToUint => {
                self.simple_instruction("OP_CAST_BOOL_TO_UINT", offset, trace_level)
            }
            Opcode::CastUintToBool => {
                self.simple_instruction("OP_CAST_UINT_TO_BOOL", offset, trace_level)
            }
            Opcode::CastUintToInt => {
                self.simple_instruction("OP_CAST_UINT_TO_INT", offset, trace_level)
            }
            Opcode::CastIntToUint => {
                self.simple_instruction("OP_CAST_INT_TO_UINT", offset, trace_level)
            }
            Opcode::CastUintToFloat => {
                self.simple_instruction("OP_CAST_UINT_TO_FLOAT", offset, trace_level)
            }
            // Opcode::PopClosure => {
            //     self.simple_instruction("OP_POP_CLOSURE", offset, trace_level)
            // }
            Opcode::FrameBase => {
                self.simple_instruction("OP_FRAME_BASE", offset, trace_level)
            }
            // Opcode::CopyClosure => {
            //     self.simple_instruction("OP_COPY_CLOSURE", offset, trace_level)
            // }
            _ => {
                info!("Unknown opcode: {}", instruction);
                offset + 1
            }
        }
    }

    #[instrument(level = "trace", skip(self))]
    fn simple_instruction(&self, name: &str, offset: usize, trace_level: Level) -> usize {
        dyn_event!(trace_level, "{:16} ", name);
        offset + 1
    }

    #[instrument(level = "trace", skip(self))]
    fn two_byte_instruction(&self, name: &str, offset: usize, trace_level: Level) -> usize {
        let (slot, _) = self.code[offset + 1];
        let (slot2, _) = self.code[offset + 2];
        dyn_event!(trace_level, "{:16} {} {}", name, slot, slot2);
        offset + 3
    }

    #[instrument(level = "trace", skip(self))]
    fn three_byte_instruction(&self, name: &str, offset: usize, trace_level: Level) -> usize {
        let (slot, _) = self.code[offset + 1];
        let (slot2, _) = self.code[offset + 2];
        let (slot3, _) = self.code[offset + 3];
        dyn_event!(trace_level, "{:16} {} {} {}", name, slot, slot2, slot3);
        offset + 4
    }

    #[instrument(level = "trace", skip(self))]
    fn constant_instruction(&self, name: &str, offset: usize, trace_level: Level) -> usize {
        let (constant_offset, _) = self.code[offset + 1];
        dyn_event!(trace_level, "{:16} {:?}", name, constant_offset as usize);
        offset + 2
    }

    #[instrument(level = "trace", skip(self))]
    fn constant_long_instruction(&self, name: &str, offset: usize, trace_level: Level) -> usize {
        let (constant_offset, _) = self.code[offset + 1];
        let (constant_offset2, _) = self.code[offset + 2];
        let (constant_offset3, _) = self.code[offset + 3];
        dyn_event!(
            trace_level,
            "{:16} {:?}",
            name,
            self.constants
                [
                    (u32::from(constant_offset) |
                        (u32::from(constant_offset2) << 8) |
                        (u32::from(constant_offset3) << 16)) as usize
                ]
        );
        offset + 5
    }

    #[instrument(level = "trace", skip(self))]
    fn byte_instruction(&self, name: &str, offset: usize, trace_level: Level) -> usize {
        let (slot, _) = self.code[offset + 1];
        dyn_event!(trace_level, "{:16} {}", name, slot);
        offset + 2
    }

    #[instrument(level = "trace", skip(self))]
    fn jump_instruction(&self, name: &str, sign: i32, offset: usize, trace_level: Level) -> usize {
        let mut jump = i32::from(self.code[offset + 1].0) << 8;
        jump |= i32::from(self.code[offset + 2].0);
        dyn_event!(
            trace_level,
            "{:16} {} -> {}",
            name,
            offset,
            (offset as i32) + 3 + sign * jump
        );
        offset + 3
    }

    pub fn write_local_ptr(&mut self, i: usize, line: usize) {
        self.write_constant(Value::Pointer(Pointer::Local(i)), line);
        self.write(Opcode::FrameBase as u8, line);
        self.write(Opcode::PointerAdd as u8, line);
        self.write(1, line);
    }
}
