use std::{collections::HashMap, fmt::Debug};

use crate::value::{object::Object, ConstantPool, Value};

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Opcode {
    NoOp,
    Return,
    Constant,
    ConstantLong,
    Negate,
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
    Pop,
    DefineGlobal,
    DefineGlobalLong,
    GetGlobal,
    GetGlobalLong,
    SetGlobal,
    SetGlobalLong,
    GetLocal,
    SetLocal,
    JumpIfFalse,
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
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,

    CastIntToFloat,
    CastFloatToInt,
    CastIntToString,
    CastFloatToString,
    CastBoolToFloat,
    CastBoolToInt,
    CastBoolToString,
    CastIntToBool,

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
        unsafe { std::mem::transmute(*self as u8 + 1) }
    }
}

pub struct Chunk {
    pub code: Vec<(u8, usize)>, // code, line
    pub constants: ConstantPool,
    constant_pool_cache: HashMap<String, u32>,
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.dissassemble("chunk");
        write!(f, "")
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: ConstantPool::new(),
            constant_pool_cache: HashMap::new(),
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

    pub fn define_global(&mut self, global_id: u32, line: usize) {
        if global_id > 255 {
            self.write(Opcode::DefineGlobalLong.into(), line);
            self.write(global_id as u8, line);
            self.write((global_id >> 8) as u8, line);
            self.write((global_id >> 16) as u8, line);
        } else {
            self.write(Opcode::DefineGlobal.into(), line);
            self.write(global_id as u8, line);
        }
    }

    pub fn add_constant(&mut self, value: Value) -> u32 {
        if let Value::Object(Object::String(s)) = &value {
            if let Some(id) = self.constant_pool_cache.get(&s.to_string()) {
                return *id;
            }
        }
        let id = self.constants.write(value);
        if let Value::Object(Object::String(s)) = &self.constants[id as usize] {
            self.constant_pool_cache.insert(s.to_string(), id);
        };
        id
    }

    pub fn dissassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.dissassemble_instruction(offset);
        }
    }

    pub fn dissassemble_instruction(&self, mut offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.code[offset].1 == self.code[offset - 1].1 {
            print!("   | ");
        } else {
            print!("{:4} ", self.code[offset].1);
        }

        let (instruction, _) = self.code[offset];
        match Opcode::from(instruction) {
            Opcode::NoOp => return self.simple_instruction("OP_NOOP", offset),
            Opcode::Return => return self.simple_instruction("OP_RETURN", offset),
            Opcode::Constant => return self.constant_instruction("OP_CONSTANT", offset),
            Opcode::ConstantLong => return self.constant_long_instruction("OP_CONSTANT_LONG", offset),
            Opcode::Negate => return self.simple_instruction("OP_NEGATE", offset),
            Opcode::AddInt => return self.simple_instruction("OP_ADD", offset),
            Opcode::AddFloat => return self.simple_instruction("OP_ADD_FLOAT", offset),
            Opcode::Concat => return self.simple_instruction("OP_CONCAT", offset),
            Opcode::PointerAdd => return self.simple_instruction("OP_POINTER_ADD", offset),
            Opcode::SubtractInt => return self.simple_instruction("OP_SUBTRACT", offset),
            Opcode::SubtractFloat => return self.simple_instruction("OP_SUBTRACT_FLOAT", offset),
            Opcode::PointerSubtract => return self.simple_instruction("OP_POINTER_SUBTRACT", offset),
            Opcode::MultiplyInt => return self.simple_instruction("OP_MULTIPLY", offset),
            Opcode::MultiplyFloat => return self.simple_instruction("OP_MULTIPLY_FLOAT", offset),
            Opcode::DivideInt => return self.simple_instruction("OP_DIVIDE", offset),
            Opcode::DivideFloat => return self.simple_instruction("OP_DIVIDE_FLOAT", offset),
            Opcode::Ternary => return self.simple_instruction("OP_TERNARY", offset),
            Opcode::False => return self.simple_instruction("OP_FALSE", offset),
            Opcode::Nil => return self.simple_instruction("OP_NIL", offset),
            Opcode::True => return self.simple_instruction("OP_TRUE", offset),
            Opcode::Not => return self.simple_instruction("OP_NOT", offset),
            Opcode::Equal => return self.simple_instruction("OP_EQUAL", offset),
            Opcode::GreaterInt => return self.simple_instruction("OP_GREATER", offset),
            Opcode::GreaterFloat => return self.simple_instruction("OP_GREATER_FLOAT", offset),
            Opcode::Or => return self.simple_instruction("OP_OR", offset),
            Opcode::And => return self.simple_instruction("OP_AND", offset),
            Opcode::PrintInt => return self.simple_instruction("OP_PRINT_INT", offset),
            Opcode::PrintFloat => return self.simple_instruction("OP_PRINT_FLOAT", offset),
            Opcode::PrintString => return self.simple_instruction("OP_PRINT_STRING", offset),
            Opcode::PrintBool => return self.simple_instruction("OP_PRINT_BOOL", offset),
            Opcode::PrintPointer => return self.simple_instruction("OP_PRINT_POINTER", offset),
            Opcode::PrintNil => return self.simple_instruction("OP_PRINT_NIL", offset),

            Opcode::Pop => return self.simple_instruction("OP_POP", offset),
            Opcode::DefineGlobal => return self.byte_instruction("OP_DEFINE_GLOBAL", offset),
            Opcode::DefineGlobalLong => return self.constant_long_instruction("OP_DEFINE_GLOBAL_LONG", offset),
            Opcode::GetGlobal => return self.byte_instruction("OP_GET_GLOBAL", offset),
            Opcode::GetGlobalLong => return self.constant_long_instruction("OP_GET_GLOBAL_LONG", offset),
            Opcode::SetGlobal => return self.byte_instruction("OP_SET_GLOBAL", offset),
            Opcode::SetGlobalLong => return self.constant_long_instruction("OP_SET_GLOBAL_LONG", offset),
            Opcode::GetLocal => return self.byte_instruction("OP_GET_LOCAL", offset),
            Opcode::SetLocal => return self.byte_instruction("OP_SET_LOCAL", offset),
            Opcode::JumpIfFalse => return self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            Opcode::Jump => return self.jump_instruction("OP_JUMP", 1, offset),
            Opcode::Loop => return self.jump_instruction("OP_LOOP", -1, offset),
            Opcode::Call => return self.byte_instruction("OP_CALL", offset),
            Opcode::RefLocal => return self.simple_instruction("OP_REF", offset),
            Opcode::RefGlobal => return self.byte_instruction("OP_REF_GLOBAL", offset),
            Opcode::DerefGet => return self.simple_instruction("OP_DEREF_GET", offset),
            Opcode::DerefAssign => return self.simple_instruction("OP_DEREF_ASSIGN", offset),
            Opcode::DefineStackArray => return self.simple_instruction("OP_DefineStackArray", offset),
            Opcode::DefineGlobalArray => return self.simple_instruction("OP_DefineGlobalArray", offset),
            Opcode::Closure => {
                offset += 1;
                let constant = self.code[offset].0;
                offset += 1;
                println!("{:16} {}", "OP_CLOSURE", constant);

                // let Value::Object(Object::Function(func)) = &self.constants[constant as usize] else {
                //     println!("Malformed closure");
                //     return offset;
                // };

                // for _ in 0..func.upvalue_count {
                //     let is_local = self.code[offset].0 != 0;
                //     offset += 1;
                //     let index = self.code[offset].0;
                //     offset += 1;
                //     println!("{:04}      |                     {} {}", offset - 2, if is_local {"local"} else{"upvalue"}, index);
                // }

                return offset;
            }
            Opcode::GetUpvalue => return self.byte_instruction("OP_GETUPVALUE", offset),
            Opcode::SetUpvalue => return self.byte_instruction("OP_SETUPVALUE", offset),
            Opcode::CloseUpvalue => return self.simple_instruction("OP_CLOSEUPVSLUR", offset),
            Opcode::CastIntToFloat => return self.simple_instruction("OP_CAST_INT_TO_FLOAT", offset),
            Opcode::CastFloatToInt => return self.simple_instruction("OP_CAST_FLOAT_TO_INT", offset),
            Opcode::CastIntToString => return self.simple_instruction("OP_CAST_INT_TO_STRING", offset),
            Opcode::CastFloatToString => return self.simple_instruction("OP_CAST_FLOAT_TO_STRING", offset),
            Opcode::CastBoolToFloat => return self.simple_instruction("OP_CAST_BOOL_TO_FLOAT", offset),
            Opcode::CastBoolToInt => return self.simple_instruction("OP_CAST_BOOL_TO_INT", offset),
            Opcode::CastBoolToString => return self.simple_instruction("OP_CAST_BOOL_TO_STRING", offset),
            Opcode::CastIntToBool => return self.simple_instruction("OP_CAST_INT_TO_BOOL", offset),
            
            _ => {
                println!("Unknown opcode {}", instruction);
                return offset + 1;
            }
            
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let (constant_offset, _) = self.code[offset + 1];
        println!("{:16} {:20} @ 0x{:x}", name, format!("{:?}", self.constants[constant_offset as usize]), constant_offset);
        offset + 2
    }

    fn constant_long_instruction(&self, name: &str, offset: usize) -> usize {
        let (constant_offset, _) = self.code[offset + 1];
        println!("{:16} {:?}", name, self.constants[constant_offset as usize]);
        offset + 5
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let (slot, _) = self.code[offset + 1];
        println!("{:16} {}", name, slot);
        offset + 2
    }

    fn jump_instruction(&self, name: &str, sign: i32, offset: usize) -> usize {
        let mut jump = (self.code[offset + 1].0 as i32) << 8;
        jump |= self.code[offset + 2].0 as i32;
        println!("{:16} {} -> {}", name, offset, offset as i32 + 3 + (sign * jump as i32));
        offset + 3
    }
}
