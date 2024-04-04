use std::fmt::Debug;

use tracing::{ info, instrument, Level };

use crate::{
    typing::{ UValueType, ValueType },
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
    AddFloat,
    Concat,
    PointerAdd,
    SubtractInt,
    SubtractFloat,
    PointerSubtract,

    MultiplyInt,
    MultiplyFloat,
    DivideInt,
    DivideFloat,
    Ternary,
    False,
    Nil,
    True,
    Not,
    Equal,
    GreaterInt,
    GreaterFloat,
    LessInt,
    LessFloat,
    Or,
    And,
    PrintInt,
    PrintFloat,
    PrintString,
    PrintBool,
    PrintPointer,
    PrintNil,
    PrintChar,
    Pop,
    PopClosure,
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
    Closure,
    CopyClosure,
    GetUpvalue,
    GetField,

    CastIntToFloat,
    CastFloatToInt,
    CastIntToString,
    CastFloatToString,
    CastBoolToFloat,
    CastBoolToInt,
    CastBoolToString,
    CastIntToBool,

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
        self.dissassemble("chunk", Level::DEBUG);
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
        self.write_pool_opcode(Opcode::Constant, id, line);
    }

    pub fn write_pool_opcode(&mut self, code: Opcode, pool_id: u32, line: usize) {
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
        if t == ValueType::AnyFunction.intern() {
            self.write(Opcode::PopClosure.into(), line);
        } else {
            self.write(Opcode::Pop.into(), line);
            self.write(t.num_words() as u8, line);
        }
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
        let id = self.constants.write(value);
        id
    }

    pub fn dissassemble(&self, name: &str, trace_level: Level) {
        let _guard = dyn_span!(trace_level, "dissassemble", name = name).entered();

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.dissassemble_instruction(offset, trace_level);
        }
    }

    pub fn dissassemble_instruction(&self, offset: usize, trace_level: Level) -> usize {
        let (instruction, _) = self.code[offset];
        match Opcode::from(instruction) {
            Opcode::NoOp => {
                return self.simple_instruction("OP_NOOP", offset, trace_level);
            }
            Opcode::Return => {
                return self.byte_instruction("OP_RETURN", offset, trace_level);
            }
            Opcode::Constant => {
                return self.constant_instruction("OP_CONSTANT", offset, trace_level);
            }
            Opcode::ConstantLong => {
                return self.constant_long_instruction("OP_CONSTANT_LONG", offset, trace_level);
            }
            Opcode::NegateInt => {
                return self.simple_instruction("OP_NEGATE", offset, trace_level);
            }
            Opcode::NegateFloat => {
                return self.simple_instruction("OP_NEGATE_FLOAT", offset, trace_level);
            }
            Opcode::AddInt => {
                return self.simple_instruction("OP_ADD", offset, trace_level);
            }
            Opcode::AddFloat => {
                return self.simple_instruction("OP_ADD_FLOAT", offset, trace_level);
            }
            Opcode::Concat => {
                return self.simple_instruction("OP_CONCAT", offset, trace_level);
            }
            Opcode::PointerAdd => {
                return self.simple_instruction("OP_POINTER_ADD", offset, trace_level);
            }
            Opcode::SubtractInt => {
                return self.simple_instruction("OP_SUBTRACT", offset, trace_level);
            }
            Opcode::SubtractFloat => {
                return self.simple_instruction("OP_SUBTRACT_FLOAT", offset, trace_level);
            }
            Opcode::PointerSubtract => {
                return self.simple_instruction("OP_POINTER_SUBTRACT", offset, trace_level);
            }
            Opcode::MultiplyInt => {
                return self.simple_instruction("OP_MULTIPLY", offset, trace_level);
            }
            Opcode::MultiplyFloat => {
                return self.simple_instruction("OP_MULTIPLY_FLOAT", offset, trace_level);
            }
            Opcode::DivideInt => {
                return self.simple_instruction("OP_DIVIDE", offset, trace_level);
            }
            Opcode::DivideFloat => {
                return self.simple_instruction("OP_DIVIDE_FLOAT", offset, trace_level);
            }
            Opcode::Ternary => {
                return self.simple_instruction("OP_TERNARY", offset, trace_level);
            }
            Opcode::False => {
                return self.simple_instruction("OP_FALSE", offset, trace_level);
            }
            Opcode::Nil => {
                return self.simple_instruction("OP_NIL", offset, trace_level);
            }
            Opcode::True => {
                return self.simple_instruction("OP_TRUE", offset, trace_level);
            }
            Opcode::Not => {
                return self.simple_instruction("OP_NOT", offset, trace_level);
            }
            Opcode::Equal => {
                return self.simple_instruction("OP_EQUAL", offset, trace_level);
            }
            Opcode::GreaterInt => {
                return self.simple_instruction("OP_GREATER", offset, trace_level);
            }
            Opcode::GreaterFloat => {
                return self.simple_instruction("OP_GREATER_FLOAT", offset, trace_level);
            }
            Opcode::LessInt => {
                return self.simple_instruction("OP_LESS", offset, trace_level);
            }
            Opcode::LessFloat => {
                return self.simple_instruction("OP_LESS_FLOAT", offset, trace_level);
            }
            Opcode::Or => {
                return self.simple_instruction("OP_OR", offset, trace_level);
            }
            Opcode::And => {
                return self.simple_instruction("OP_AND", offset, trace_level);
            }
            Opcode::PrintInt => {
                return self.simple_instruction("OP_PRINT_INT", offset, trace_level);
            }
            Opcode::PrintFloat => {
                return self.simple_instruction("OP_PRINT_FLOAT", offset, trace_level);
            }
            Opcode::PrintString => {
                return self.simple_instruction("OP_PRINT_STRING", offset, trace_level);
            }
            Opcode::PrintBool => {
                return self.simple_instruction("OP_PRINT_BOOL", offset, trace_level);
            }
            Opcode::PrintPointer => {
                return self.simple_instruction("OP_PRINT_POINTER", offset, trace_level);
            }
            Opcode::PrintNil => {
                return self.simple_instruction("OP_PRINT_NIL", offset, trace_level);
            }
            Opcode::PrintChar => {
                return self.simple_instruction("OP_PRINT_CHAR", offset, trace_level);
            }
            Opcode::GetField => {
                return self.three_byte_instruction("OP_GET_FIELD", offset, trace_level);
            }
            Opcode::Pop => {
                return self.byte_instruction("OP_POP", offset, trace_level);
            }
            Opcode::DefineGlobal => {
                return self.two_byte_instruction("OP_DEFINE_GLOBAL", offset, trace_level);
            }
            Opcode::DefineGlobalLong => {
                return self.constant_long_instruction("OP_DEFINE_GLOBAL_LONG", offset, trace_level);
            }
            Opcode::GetGlobal => {
                return self.two_byte_instruction("OP_GET_GLOBAL", offset, trace_level);
            }
            Opcode::GetGlobalLong => {
                return self.constant_long_instruction("OP_GET_GLOBAL_LONG", offset, trace_level);
            }
            Opcode::GetLocal => {
                return self.two_byte_instruction("OP_GET_LOCAL", offset, trace_level);
            }
            Opcode::JumpIfFalse => {
                return self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset, trace_level);
            }
            Opcode::JumpIfTrue => {
                return self.jump_instruction("OP_JUMP_IF_TRUE", 1, offset, trace_level);
            }
            Opcode::Jump => {
                return self.jump_instruction("OP_JUMP", 1, offset, trace_level);
            }
            Opcode::Loop => {
                return self.jump_instruction("OP_LOOP", -1, offset, trace_level);
            }
            Opcode::Call => {
                return self.byte_instruction("OP_CALL", offset, trace_level);
            }
            Opcode::RefLocal => {
                return self.simple_instruction("OP_REF", offset, trace_level);
            }
            Opcode::RefGlobal => {
                return self.byte_instruction("OP_REF_GLOBAL", offset, trace_level);
            }
            Opcode::DerefGet => {
                return self.byte_instruction("OP_DEREF_GET", offset, trace_level);
            }
            Opcode::DerefAssign => {
                return self.byte_instruction("OP_DEREF_ASSIGN", offset, trace_level);
            }
            Opcode::DefineStackArray => {
                return self.simple_instruction("OP_DefineStackArray", offset, trace_level);
            }
            Opcode::DefineGlobalArray => {
                return self.three_byte_instruction("OP_DefineGlobalArray", offset, trace_level);
            }
            Opcode::Closure => {
                let (fid, _) = self.code[offset + 1];
                let (count_upvals, _) = self.code[offset + 2];
                dyn_event!(
                    trace_level,
                    "{:16} {:?} {:?}",
                    "OP_CLOSURE",
                    fid as usize,
                    count_upvals
                );

                return offset + 3 + ((count_upvals * 3) as usize);
            }
            Opcode::GetUpvalue => {
                return self.three_byte_instruction("OP_GET_UPVALUE", offset, trace_level);
            }
            Opcode::CastIntToFloat => {
                return self.simple_instruction("OP_CAST_INT_TO_FLOAT", offset, trace_level);
            }
            Opcode::CastFloatToInt => {
                return self.simple_instruction("OP_CAST_FLOAT_TO_INT", offset, trace_level);
            }
            Opcode::CastIntToString => {
                return self.simple_instruction("OP_CAST_INT_TO_STRING", offset, trace_level);
            }
            Opcode::CastFloatToString => {
                return self.simple_instruction("OP_CAST_FLOAT_TO_STRING", offset, trace_level);
            }
            Opcode::CastBoolToFloat => {
                return self.simple_instruction("OP_CAST_BOOL_TO_FLOAT", offset, trace_level);
            }
            Opcode::CastBoolToInt => {
                return self.simple_instruction("OP_CAST_BOOL_TO_INT", offset, trace_level);
            }
            Opcode::CastBoolToString => {
                return self.simple_instruction("OP_CAST_BOOL_TO_STRING", offset, trace_level);
            }
            Opcode::CastIntToBool => {
                return self.simple_instruction("OP_CAST_INT_TO_BOOL", offset, trace_level);
            }
            Opcode::PopClosure => {
                return self.simple_instruction("OP_POP_CLOSURE", offset, trace_level);
            }
            Opcode::FrameBase => {
                return self.simple_instruction("OP_FRAME_BASE", offset, trace_level);
            }
            Opcode::CopyClosure => {
                return self.simple_instruction("OP_COPY_CLOSURE", offset, trace_level);
            }
            _ => {
                info!("Unknown opcode: {}", instruction);
                return offset + 1;
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
                    ((constant_offset as u32) |
                        ((constant_offset2 as u32) << 8) |
                        ((constant_offset3 as u32) << 16)) as usize
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
        let mut jump = (self.code[offset + 1].0 as i32) << 8;
        jump |= self.code[offset + 2].0 as i32;
        dyn_event!(
            trace_level,
            "{:16} {} -> {}",
            name,
            offset,
            (offset as i32) + 3 + sign * (jump as i32)
        );
        offset + 3
    }

    pub fn write_local_ptr(&mut self, i: usize, line: usize) {
        self.write_constant(Value::Pointer(Pointer::Local(i)), line);
        self.write(Opcode::FrameBase as u8, line);
        self.write(Opcode::PointerAdd as u8, line);
    }
}
