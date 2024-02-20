use std::cell::RefCell;
use std::collections::HashMap;
use tracing::{event, span, Level};

use std::rc::Rc;

use crate::chunk::Opcode;

use crate::compiler::Compiler;

use crate::lexer::Lexer;
use crate::{parser};
use crate::common::runnable::{Function, Runnable};

use crate::value::pointer::Pointer;
use crate::typing::{ValueType, ValueTypeK};
use crate::value::Value::{self, Bool, Nil};
use crate::value::Word;
use crate::value::{Upvalue, Closure};

const STACK_MAX: usize = 256;
const HEAP_MAX: usize = 2048;
pub struct VM {
    stack: [Word; STACK_MAX],
    stack_top: usize,
    natives: HashMap<String, (usize, ValueType)>,
    heap: [Word; HEAP_MAX],
    static_segment: Vec<Word>,
    function_segment: Vec<Runnable>,
    brk: usize,
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,

    frames: Vec<CallFrame>,
}

struct CallFrame {
    closure: Closure,
    function: Rc<Function>,
    ip: usize,
    frame_pointer: usize,
}

impl CallFrame {
    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.function.chunk.code[self.ip - 1].0
    }

    fn read_short(&mut self) -> u16 {
        self.ip += 2;
        let b1 = self.function.chunk.code[self.ip - 2].0;
        let b2 = self.function.chunk.code[self.ip - 1].0;
        (b1 as u16) << 8 | (b2 as u16)
    }

    fn get_constant(&mut self, constant_idx: usize) -> Value {
        self.function.chunk.constants[constant_idx].clone()
    }
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

macro_rules! runtime_error {
    ($self:expr, $fmt:literal $(, $args:expr)* ) => {
        {
            print!("Runtime Error: ");
            println!($fmt $(, $args)*);
            for frame in $self.frames.iter().rev() {
                let function = &frame.function;
                let instruction = frame.ip - 1;
                let line = function.chunk.code[instruction].1;
                println!("[line {}] in {}()", line, function.name);
            }

            $self.stack_top = 0;
            $self.open_upvalues = Vec::new();
        }
    };
}

fn clock_native(_args: &[Word]) -> Value {
    let now = std::time::SystemTime::now();
    let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
    Value::Float(duration.as_secs_f64())
}


impl VM {
    pub fn new() -> VM {
        let mut vm = VM {
            stack: std::array::from_fn(|_| Word::new(0)),
            stack_top: 0,
            natives: HashMap::new(),
            frames: Vec::new(),
            heap: std::array::from_fn(|_| Word::new(0)),
            brk: 0,
            open_upvalues: Vec::new(),
            static_segment: Vec::new(),
            function_segment: Vec::new(),
        };
        vm.define_native(
            "clock",
            clock_native,
            ValueTypeK::Closure(Box::new([ValueTypeK::Float.intern()])).intern(),
        );
        vm
    }
    
    pub fn interpret<P>(&mut self, source: String) -> InterpretResult
    where P: parser::Parser {
        
        let lexer = Lexer::new(source);
        let parsed = P::parse(lexer);
        let compiled = Compiler::compile(parsed, &self.natives);

        let Some(func) = compiled.function else {
            return InterpretResult::CompileError;
        };
        let f = Rc::new(func);

        self.static_segment = compiled.static_data_segment;
        self.function_segment.extend(compiled.function_segment);
        
        self.function_segment.push(Runnable::Function(f.clone()));
        let c = Closure::new(Pointer::Function(self.function_segment.len() - 1), 0);
        // self.brk += 1;
        self.push_many(&c.clone().to_words());

        self.call(c, 0);

        return self.run();
    }

    fn run(&mut self) -> InterpretResult {
        let mut frame_no = self.frames.len() - 1;
        let lspan = span!(Level::TRACE, "vm::run");
        loop {
            
            let _guard = lspan.enter();

            event!(Level::TRACE, "stack: {:?}", &self.stack[0..self.stack_top]);
            

            let instruction = self.frames[frame_no].read_byte();

            self.frames[frame_no]
                .function
                .chunk
                .dissassemble_instruction(self.frames[frame_no].ip - 1, Level::DEBUG);
            

            match Opcode::from(instruction) {
                Opcode::NoOp => { // NoOp
                }
                Opcode::Return => {
                    // Return
                    let num_bytes = self.frames[frame_no].read_byte();
                    self.close_upvalues(self.frames[frame_no].frame_pointer);
                    let result = self.pop_many(num_bytes as usize);

                    let Some(old_frame) = self.frames.pop() else {
                        return InterpretResult::RuntimeError;
                    };
                    if self.frames.is_empty() {
                        // self.pop();
                        return InterpretResult::Ok;
                    }
                    frame_no = self.frames.len() - 1;

                    self.stack_top = old_frame.frame_pointer;
                    self.push_many(&result);
                }
                Opcode::Constant => {
                    // Constant
                    let constant_idx = self.frames[frame_no].read_byte();
                    let constant = self.frames[frame_no].get_constant(constant_idx as usize);
                    self.push(constant.to_word());
                }
                Opcode::ConstantLong => { // ConstantLong
                }
                Opcode::NegateInt => {
                    // Negate
                    let n = self.peek(0).to_i64();
                    self.place(0, Value::Integer(-n).to_word());
                }
                Opcode::NegateFloat => {
                    // Negate
                    let n = self.peek(0).to_float();
                    self.place(0, Value::Float(-n).to_word());
                }
                Opcode::AddInt => {
                    let b = self.pop().to_i64();
                    let a = self.peek(0).to_i64();
                    self.stack[self.stack_top - 1] = Value::Integer(a + b).to_word();
                }
                Opcode::AddFloat => {
                    let b = self.pop().to_float();
                    let a = self.peek(0).to_float();
                    self.stack[self.stack_top - 1] = Value::Float(a + b).to_word();
                }
                Opcode::Concat => {
                    // Concat
                    let b = self.pop().to_pointer();
                    let a = self.pop().to_pointer();
                    let s1 = self.get_string(a);
                    let s2 = self.get_string(b);
                    let s = s1 + &s2;
                    self.push(Value::Pointer(Pointer::Heap(self.brk)).to_word());
                    self.heap[self.brk] = Value::Integer(s.len() as i64).to_word();
                    self.brk += 1;
                    for i in 0..s.len() {
                        self.heap[self.brk] = Value::Char(s.as_bytes()[i]).to_word();
                        self.brk += 1;
                    }
                }
                Opcode::PointerAdd => {
                    // PointerAdd
                    let n = self.pop().to_i64();
                    let p = self.pop().to_pointer();
                    match p {
                        Pointer::Local(slot) => {
                            self.push(Value::Pointer(Pointer::Local(slot + n as usize)).to_word());
                        }
                        Pointer::Heap(idx) => {
                            self.push(Value::Pointer(Pointer::Heap(idx + n as usize)).to_word());
                        }
                        Pointer::Static(idx) => {
                            self.push(Value::Pointer(Pointer::Static(idx + n as usize)).to_word());
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::SubtractInt => {
                    let b = self.pop().to_i64();
                    let a = self.peek(0).to_i64();
                    self.stack[self.stack_top - 1] = Value::Integer(a - b).to_word();
                }
                Opcode::SubtractFloat => {
                    let b = self.pop().to_float();
                    let a = self.peek(0).to_float();
                    self.stack[self.stack_top - 1] = Value::Float(a - b).to_word();
                }
                Opcode::PointerSubtract => {
                    // PointerSubtract
                    let n = self.pop().to_i64();
                    let p = self.pop().to_pointer();
                    match p {
                        Pointer::Local(slot) => {
                            self.push(Value::Pointer(Pointer::Local(slot - n as usize)).to_word());
                        }
                        Pointer::Heap(idx) => {
                            self.push(Value::Pointer(Pointer::Heap(idx - n as usize)).to_word());
                        }
                        Pointer::Static(idx) => {
                            self.push(Value::Pointer(Pointer::Static(idx - n as usize)).to_word());
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::MultiplyInt => {
                    let b = self.pop().to_i64();
                    let a = self.peek(0).to_i64();
                    self.stack[self.stack_top - 1] = Value::Integer(a * b).to_word();
                }
                Opcode::MultiplyFloat => {
                    let b = self.pop().to_float();
                    let a = self.peek(0).to_float();
                    self.stack[self.stack_top - 1] = Value::Float(a * b).to_word();
                }
                Opcode::DivideInt => {
                    let a = self.pop().to_i64();
                    let b = self.peek(0).to_i64();
                    self.stack[self.stack_top - 1] = Value::Integer(b / a).to_word();
                }
                Opcode::DivideFloat => {
                    let a = self.pop().to_float();
                    let b = self.peek(0).to_float();
                    self.stack[self.stack_top - 1] = Value::Float(b / a).to_word();
                }
                Opcode::Ternary => {
                    let alt = self.pop();
                    let conseq = self.pop();
                    let cond = self.pop().to_bool();

                    if cond == true {
                        self.push(conseq);
                    } else {
                        self.push(alt);
                    }
                }
                Opcode::False => {
                    // False
                    self.push(Bool(false).to_word());
                }
                Opcode::Nil => {
                    // Nil
                    self.push(Nil.to_word());
                }
                Opcode::True => {
                    // True
                    self.push(Bool(true).to_word());
                }
                Opcode::Not => {
                    // Not
                    let operand = self.peek(0);
                    if operand.is_falsy() {
                        self.place(0, Bool(true).to_word());
                    } else {
                        self.place(0, Bool(false).to_word());
                    }
                }
                Opcode::Equal => {
                    // Equal
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Bool(a == b).to_word());
                }
                Opcode::GreaterInt => {
                    // Greater
                    let b = self.pop().to_i64();
                    let a = self.peek(0).to_i64();
                    self.stack[self.stack_top - 1] = Bool(a > b).to_word();
                }
                Opcode::GreaterFloat => {
                    // Greater
                    let b = self.pop().to_float();
                    let a = self.peek(0).to_float();
                    self.stack[self.stack_top - 1] = Bool(a > b).to_word();
                }
                Opcode::LessInt => {
                    // Less
                    let b = self.pop().to_i64();
                    let a = self.peek(0).to_i64();
                    self.stack[self.stack_top - 1] = Bool(a < b).to_word();
                }
                Opcode::LessFloat => {
                    // Less
                    let b = self.pop().to_float();
                    let a = self.peek(0).to_float();
                    self.stack[self.stack_top - 1] = Bool(a < b).to_word();
                }
                Opcode::PrintInt => {
                    // Print
                    let v = self.pop().to_i64();
                    println!("{:?}", v);
                }
                Opcode::PrintFloat => {
                    // Print
                    let v = self.pop().to_float();
                    println!("{:?}", v);
                }
                Opcode::PrintBool => {
                    // Print
                    let v = self.pop().to_bool();
                    println!("{:?}", v);
                }
                Opcode::PrintString => {
                    // Print
                    let v = self.pop();
                    let s = self.get_string(v.to_pointer());
                    println!("{}", s);
                }
                Opcode::PrintChar => {
                    // Print
                    let v = self.pop().to_char();
                    println!("{}", v);
                }
                Opcode::PrintNil => {
                    // Print
                    let _ = self.pop();
                    println!("{:?}", Nil);
                }
                Opcode::PrintPointer => {
                    // Print
                    let v = self.pop().to_pointer();
                    println!("{:?}", v);
                }

                Opcode::Pop => {
                    // Pop
                    let slize = self.frames[frame_no].read_byte() as usize;
                    self.pop_many(slize);
                }
                Opcode::PopClosure => {
                    // PopClosure
                    let _c = self.pop_many(3);
                    // if c[1].to_u64() > 0 {
                    // let upvals =
                    //     unsafe { Box::from_raw(c[2].to_u64() as *mut Vec<Rc<RefCell<Upvalue>>>) };
                    // drop(upvals);
                // }
                }
                Opcode::DefineGlobal => {
                    // DefineGlobal
                    let global_id = self.frames[frame_no].read_byte() as u8;
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let v = self.pop_many(slize);
                    for i in 0..slize {
                        self.static_segment[global_id as usize + i] = v[i];
                        self.heap[self.brk] = v[i];
                        self.brk += 1;
                    }
                    // self.push_many(&v);
                }
                Opcode::DefineGlobalLong => {
                    // DefineGlobalLong
                }
                Opcode::GetGlobal => {
                    // GetGlobal
                    let global_id = self.frames[frame_no].read_byte() as u8;
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let v = self.static_segment[global_id as usize..global_id as usize + slize]
                        .to_vec();
                    self.push_many(&v);
                }
                Opcode::CopyClosure => {
                    // CopyClosure
                    let p = self.pop();
                    let n = self.pop().to_i64();

                    let upvals;
                    if n > 0 {
                    let rupvals =
                        unsafe { Box::from_raw(p.to_u64() as *mut Vec<Rc<RefCell<Upvalue>>>) };
                     upvals = Some(rupvals.clone());
                    Box::into_raw(rupvals);
                    } else {
                        upvals = None;
                    }

                    let c = self.pop().to_pointer();

                    let mut c = Closure::new(c, n as u32);
                    c.upvalues = upvals;

                    self.push_many(&c.to_words());
                }
                Opcode::GetGlobalLong => {
                    // GetGlobalLong
                }
                
                Opcode::GetLocal => {
                    // GetLocal
                    let slot = self.frames[frame_no].read_byte() as usize;
                    let slize = self.frames[frame_no].read_byte() as usize;
                    // self.push(self.stack[self.frames[frame_no].frame_pointer + slot].clone());
                    let v = self.stack[self.frames[frame_no].frame_pointer + slot
                        ..self.frames[frame_no].frame_pointer + slot + slize]
                        .to_vec();

                    self.push_many(&v);
                }
                
                Opcode::JumpIfFalse => {
                    // JumpIfFalse
                    let offset = self.frames[frame_no].read_short() as usize;
                    if self.peek(0).is_falsy() {
                        self.frames[frame_no].ip += offset;
                    }
                }
                Opcode::JumpIfTrue => {
                    // JumpIfFalse
                    let offset = self.frames[frame_no].read_short() as usize;
                    if !self.peek(0).is_falsy() {
                        self.frames[frame_no].ip += offset;
                    }
                }
                Opcode::Jump => {
                    // Jump
                    let offset = self.frames[frame_no].read_short() as usize;
                    self.frames[frame_no].ip += offset;
                }
                Opcode::Loop => {
                    // Loop
                    let offset = self.frames[frame_no].read_short() as usize;
                    self.frames[frame_no].ip -= offset;
                }
                Opcode::Call => {
                    // Call
                    let arg_count = self.frames[frame_no].read_byte() as usize;

                    let f = self.peek(arg_count + 2).to_pointer();
                    let n = self.peek(arg_count + 1).to_i64();
                    let p = self.peek(arg_count);
                    let upvals;
                    if n > 0 {
                        let rupvals =
                            unsafe { Box::from_raw(p.to_u64() as *mut Vec<Rc<RefCell<Upvalue>>>) };
                        upvals = Some(rupvals.clone());
                        Box::into_raw(rupvals);
                    } else {
                        upvals = None;
                    }

                    let mut c = Closure::new(f, n as u32);
                    c.upvalues = upvals;

                    let args = self.pop_many(arg_count);
                    // self.pop_many(3);
                    self.push_many(&args);
                    if !self.call(c, arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                    frame_no = self.frames.len() - 1;
                }
                Opcode::RefLocal => {
                    // Ref
                    let slot = self.pop().to_i64() as usize;
                    self.push(
                        Value::Pointer(Pointer::Local(
                            slot as usize,
                        ))
                        .to_word(),
                    );
                }
                Opcode::RefGlobal => {
                    // global ref
                    let p = self.frames[frame_no].read_byte();
                    self.push(Value::Pointer(Pointer::Static(p as usize)).to_word());
                }
                Opcode::DerefGet => {
                    // deref
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let tp = self.pop();
                    let p = tp.to_pointer();
                    match p {
                        Pointer::Local(slot) => {
                            let base = self.frames[frame_no].frame_pointer;
                            let v = self.stack[base+slot..base+slot + slize].to_vec();
                            self.push_many(&v);
                        }
                        Pointer::Heap(idx) => {
                            let v = self.heap[idx..idx + slize].to_vec();
                            self.push_many(&v);
                        }
                        Pointer::Static(idx) => {
                            let v = self.static_segment[idx..idx + slize].to_vec();
                            self.push_many(&v);
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::DerefAssign => {
                    // deref Assign mem
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let v = self.pop_many(slize);
                    let p = self.pop().to_pointer();

                    self.push_many(&v);
                    match p {
                        Pointer::Local(slot) => {
                            for i in 0..slize {
                                let base = self.frames[frame_no].frame_pointer;
                                self.stack[base + slot + i] = v[i];
                            }
                        }
                        Pointer::Heap(idx) => {
                            for i in 0..slize {
                                self.heap[idx + i] = v[i];
                            }
                        }
                        Pointer::Static(idx) => {
                            for i in 0..slize {
                                self.static_segment[idx + i] = v[i];
                            }
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::DefineStackArray => {
                    // push stack top
                    self.push(Value::Pointer(Pointer::Local(self.stack_top + 1)).to_word());
                }
                Opcode::DefineGlobalArray => {
                    // define global array
                    let glob_id = self.frames[frame_no].read_byte();
                    let num = self.frames[frame_no].read_byte() as usize;
                    let slize = self.frames[frame_no].read_byte() as usize;
                    for i in 0..num {
                        let v = self.pop_many(slize);
                        for j in 0..slize {
                            self.static_segment[glob_id as usize + i * slize + j + 1] = v[j];
                        }
                    }
                }
                Opcode::Closure => {
                    // closure
                    let p = self.frames[frame_no].read_byte();
                    let Runnable::Function(f) = self.function_segment[p as usize].clone() else {
                        runtime_error!(self, "Operand must be a pointer to a function");
                        return InterpretResult::RuntimeError;
                    };

                    let _start = self.function_segment.len();

                    let mut closure =
                        Closure::new(Pointer::Function(p as usize), f.upvalue_count as u32);

                    for _ in 0..closure.upvalue_count {
                        let Some(ref mut upvals) = closure.upvalues else {
                            runtime_error!(self, "upvalue must be a pointer to a function");
                            return InterpretResult::RuntimeError;
                        };
                        let is_local = self.frames[frame_no].read_byte() != 0;
                        let index = self.frames[frame_no].read_byte();
                        if is_local {
                            let u = self.capture_upvalue(
                                self.frames[frame_no].frame_pointer + index as usize,
                            );
                            upvals.push(u.clone());
                            self.open_upvalues.push(u.clone());
                        } else {
                            let old_upvalue = self.frames[frame_no].closure.upvalues.as_mut().unwrap()
                                [index as usize]
                                .clone();
                            upvals.push(old_upvalue);
                        }
                    }
                    // self.function_segment.push(Object::Closure(closure.clone()));
                    self.push_many(&closure.to_words());
                }
                Opcode::GetUpvalue => {
                    // getupcal
                    let idx = self.frames[frame_no].read_byte();
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let upval =
                        self.frames[frame_no].closure.upvalues.as_ref().unwrap()[idx as usize].clone();
                    if let Some(idx) = upval.borrow().idx {
                        let v = self.stack[idx..idx + slize].to_vec();
                        self.push_many(&v);
                    } else {
                        let cap = &upval.borrow().closed;
                        self.push_many(cap);
                    };
                }
                Opcode::SetUpvalue => {
                    let idx = self.frames[frame_no].read_byte();
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let v = self.pop_many(slize);
                    if let Some(idx) = self.frames[frame_no].closure.upvalues.as_ref().unwrap()[idx as usize]
                        .borrow()
                        .idx
                    {
                        for i in 0..slize {
                            self.stack[idx + i] = v[i];
                        }
                    } else {
                        let upval =
                            self.frames[frame_no].closure.upvalues.as_ref().unwrap()[idx as usize].clone();
                        upval.as_ref().borrow_mut().closed = v;
                    }
                }
                Opcode::CloseUpvalue => {
                    // close value
                    let slize = self.frames[frame_no].read_byte() as usize;
                    self.close_upvalues(self.stack_top - slize);
                    self.pop();
                }
                Opcode::CastIntToFloat => {
                    // cast int to float
                    let n = self.pop().to_i64();
                    self.push(Value::Float(n as f64).to_word());
                }
                Opcode::CastFloatToInt => {
                    // cast float to int
                    let n = self.pop().to_float();
                    self.push(Value::Integer(n as i64).to_word());
                }
                Opcode::CastIntToString => {
                    // cast int to string
                    let n = self.pop().to_i64();
                    // self.push(Value::Object(Object::String(n.to_string().into())));
                    self.push(Value::Pointer(Pointer::Heap(self.brk)).to_word());
                    self.heap[self.brk] = Value::Integer(1).to_word();
                    self.brk += 1;
                    let s = n.to_string();
                    for i in 0..s.len() {
                        self.heap[self.brk] = Value::Char(s.as_bytes()[i]).to_word();
                        self.brk += 1;
                    }
                }
                Opcode::CastFloatToString => {
                    // cast float to string
                    let n = self.pop().to_float();
                    // self.push(Value::Object(Object::String(n.to_string().into())));
                    self.push(Value::Pointer(Pointer::Heap(self.brk)).to_word());
                    self.heap[self.brk] = Value::Integer(1).to_word();
                    self.brk += 1;
                    let s = n.to_string();
                    for i in 0..s.len() {
                        self.heap[self.brk] = Value::Char(s.as_bytes()[i]).to_word();
                        self.brk += 1;
                    }
                }
                Opcode::CastBoolToFloat => {
                    // cast bool to float
                    let b = self.pop().to_bool();
                    self.push(Value::Float(b as i64 as f64).to_word());
                }
                Opcode::CastBoolToInt => {
                    // cast bool to int
                    let b = self.pop().to_bool();
                    self.push(Value::Integer(b as i64).to_word());
                }
                Opcode::CastBoolToString => {
                    // cast bool to string
                    let b = self.pop().to_bool();
                    // self.push(Value::Object(Object::String(b.to_string().into())));
                    self.push(Value::Pointer(Pointer::Heap(self.brk)).to_word());
                    self.heap[self.brk] = Value::Integer(1).to_word();
                    self.brk += 1;
                    let s = b.to_string();
                    for i in 0..s.len() {
                        self.heap[self.brk] = Value::Char(s.as_bytes()[i]).to_word();
                        self.brk += 1;
                    }
                }
                Opcode::CastIntToBool => {
                    // cast int to bool
                    let n = self.pop().to_i64();
                    self.push(Value::Bool(n != 0).to_word());

                    self.push(Bool(n != 0).to_word());
                }
                Opcode::GetField => {
                    // get field
                    let slize = self.frames[frame_no].read_byte() as usize;
                    let offset = self.frames[frame_no].read_byte() as usize;
                    let f_slize = self.frames[frame_no].read_byte() as usize;
                    let s = self.pop_many(slize);
                    self.push_many(&s[offset..offset + f_slize]);
                }
                // Opcode::SetField => {
                //     // set field
                //     let slize = self.frames[frame_no].read_byte() as usize;
                //     let offset = self.frames[frame_no].read_byte() as usize;
                //     let f_slize = self.frames[frame_no].read_byte() as usize;
                //     let v = self.pop_many(f_slize);
                //     let mut s = self.pop_many(slize);
                //     for i in 0..f_slize {
                //         s[offset + i] = v[i];
                //     }
                //     self.push_many(&v);
                //     self.push_many(&s);
                // }
                _ => {
                    println!("Unknown opcode {}", instruction);
                }
            }
        }
    }

    fn close_upvalues(&mut self, last: usize) {
        while !self.open_upvalues.is_empty()
            && self.open_upvalues.last().unwrap().borrow().idx.unwrap() >= last
        {
            let upval = self.open_upvalues.pop().unwrap();
            let idx = upval.as_ref().borrow().idx.unwrap();
            let slize = upval.as_ref().borrow().slize;
            upval.as_ref().borrow_mut().closed = self.stack[idx..idx + slize].to_vec();
            upval.as_ref().borrow_mut().idx = None;
        }
    }

    fn capture_upvalue(&mut self, local_idx: usize) -> Rc<RefCell<Upvalue>> {
        let u = Rc::new(
            Upvalue {
                slize: self.stack_top - local_idx,
                closed: vec![],
                idx: Some(local_idx),
            }
            .into(),
        );

        u
    }

    const FRAMES_MAX: usize = 255;

    fn call(&mut self, c: Closure, arg_count: usize) -> bool {
        let Pointer::Function(idx) = c.func else {
            runtime_error!(self, "Can only call closures and classes dvs {:?}", c.func);
            return false;
        };

        let Runnable::Function(func) = self.function_segment[idx].clone() else {
            if let Runnable::NativeFunction(f) = self.function_segment[idx].clone() {
                let mut args = Vec::with_capacity(arg_count);
                for i in 0..arg_count {
                    args.push(self.peek(i));
                }
                let result = f(&args);
                self.pop_many(arg_count);
                self.pop_many(3);
                self.push_many(&result.to_words());
                return true;
            }
            runtime_error!(
                self,
                "Can only call closures and classes {:?}",
                self.function_segment[idx].clone()
            );
            return false;
        };

        // if func.arity != arg_count {
        //     runtime_error!(self, "Expected {} arguments but got {}", func.arity, arg_count);
        //     return false;
        // }
        if self.frames.len() == Self::FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return false;
        }
        self.frames.push(CallFrame {
            closure: c,
            function: func,
            ip: 0,
            frame_pointer: self.stack_top - arg_count - 3,
        });
        return true;
    }

    fn define_native(
        &mut self,
        name: &str,
        function: fn(&[Word]) -> Value,
        function_type: ValueType,
    ) {
        self.function_segment.push(Runnable::NativeFunction(function));
        self.natives.insert(
            name.to_string(),
            (self.function_segment.len() - 1, function_type),
        );
    }

    fn push(&mut self, value: Word) {
        if self.stack_top >= STACK_MAX {
            runtime_error!(self, "Stack overflow.");
            return;
        }

        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn push_many(&mut self, values: &[Word]) {
        for v in values {
            self.push(*v);
        }
    }

    fn pop(&mut self) -> Word {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    fn pop_many(&mut self, count: usize) -> Vec<Word> {
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.pop());
        }
        values.reverse();
        values
    }

    fn peek(&self, distance: usize) -> Word {
        self.stack[self.stack_top - 1 - distance]
    }

    fn _peek_range(&self, range: std::ops::Range<usize>) -> Vec<Word> {
        let mut values = Vec::with_capacity(range.end - range.start);
        for i in range {
            values.push(self.stack[self.stack_top - 1 - i]);
        }
        values.reverse();
        values
    }

    fn place(&mut self, distance: usize, value: Word) {
        self.stack[self.stack_top - 1 - distance] = value;
    }

    fn get_string(&mut self, p: Pointer) -> String {
        match p {
            Pointer::Heap(idx) => {
                let mut s = String::new();
                let len = self.heap[idx].to_i64();
                for i in 0..len {
                    s.push(self.heap[idx + 1 + i as usize].to_char() as char);
                }
                s
            }
            Pointer::Static(idx) => {
                let mut s = String::new();
                let len = self.static_segment[idx].to_i64();
                for i in 0..len {
                    s.push(self.static_segment[idx + 1 + i as usize].to_char() as char);
                }
                s
            }
            _ => {
                runtime_error!(self, "Operand must be a pointer to a string");
                String::new()
            }
        }
    }
}
