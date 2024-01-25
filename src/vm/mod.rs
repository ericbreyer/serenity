
use std::borrow::BorrowMut;

use std::collections::{HashMap, VecDeque};


use std::rc::Rc;

use crate::chunk::Opcode;
use crate::compiler::compile;
use crate::value::object::{self, is_string, Closure, Function, Object, Upvalue};
use crate::value::value_type::{ValueType, ValueTypeK};
use crate::value::Value::{self, Bool, Nil, Number};
use crate::value::number::{self};
use crate::value::pointer::Pointer;


const STACK_MAX: usize = 256;
const HEAP_MAX: usize = 2048;
pub struct VM {
    debug_trace_execution: bool,
    stack: [Value; STACK_MAX],
    stack_top: usize,
    active_objects: VecDeque<Object>,
    natives: HashMap<String, (usize, ValueType)>,
    heap: [Value; HEAP_MAX],
    static_segment: Vec<Value>,
    brk : usize,
    open_upvalues: Option<Rc<Upvalue>>,

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

macro_rules! exec_bin_op_number {
    ($self:ident, $return_type:ident, $op:tt) => {
        let Number(b) = $self.pop() else {
            runtime_error!($self, "Right Operand must be numbers op: {}", stringify!($op));
            return InterpretResult::RuntimeError;
        };
        let Number(a) = $self.stack[$self.stack_top - 1] else {
            runtime_error!($self, "Left Operand must be numbers op: {} {:?} {:?}", stringify!($op), $self.stack[$self.stack_top - 1], b);
            return InterpretResult::RuntimeError;
        };
        $self.stack[$self.stack_top - 1] = $return_type(a $op b);
    };
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
            $self.open_upvalues = None;
        }
    };
}

fn clock_native(_args: &[Value]) -> Value {
    let now = std::time::SystemTime::now();
    let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
    Number(number::Number::Float(duration.as_secs_f64()))
}

fn printf_native(args: &[Value]) -> Value {
    let Value::Object(Object::String(fmt)) = &args[0] else {
        // runtime_error!(self, "Operand must be a string");
        // return InterpretResult::RuntimeError;
        return Nil
    };
    let mut args = args[1..].iter();
    let mut fmt = fmt.clone().to_string();
    while let Some(arg) = args.next() {
        let Some(idx) = fmt.find("{}") else {
            break;
        };
        fmt.replace_range(idx..idx + 2, &format!("{:?}", arg));
    }
    print!("{}", fmt);

    Nil
}

impl VM {
    pub fn new(debug_mode: bool) -> VM {
        let mut vm = VM {
            debug_trace_execution: debug_mode,
            stack: std::array::from_fn(|_| Nil),
            stack_top: 0,
            active_objects: VecDeque::new(),
            natives: HashMap::new(),
            frames: Vec::new(),
            heap: std::array::from_fn(|_| Nil),
            brk: 0,
            open_upvalues: None,
            static_segment: Vec::new(),
        };
        vm.define_native("clock", clock_native, ValueTypeK::Function(Box::new([ValueTypeK::Float.intern()])).intern());
        // vm.define_native("printf", printf_native, ValueTypeK::Function(Box::new([ValueTypeK::String.intern(), ValueTypeK::AnyFunction.intern()])).intern());
        vm
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        let (maybe_func, static_segment) = compile(source, &mut self.natives);
        self.static_segment.extend(static_segment);
        let Some(func) = maybe_func else  {
            return InterpretResult::CompileError;
        };
        let f = Rc::new(func);
        self.static_segment.push(Value::Object(Object::Function(f.clone())));
        let c = Closure::new(Pointer::Static(self.static_segment.len() - 1), 0);
        self.brk += 1;
        self.push(Value::Object(Object::Closure(c.clone())));

        self.call(c, 0);

        return self.run();
    }

    fn run(&mut self) -> InterpretResult {
        let mut frame_no = self.frames.len() - 1;
        loop {
            

            let instruction = self.frames[frame_no].read_byte();

            if self.debug_trace_execution {
                print!("          ");
                for i in 0..self.stack_top {
                    print!("[ {:?} ]", self.stack[i]);
                }
                println!();

                self.frames[frame_no]
                .function
                    .chunk
                    .dissassemble_instruction(self.frames[frame_no].ip - 1);
            }

            match Opcode::from(instruction) {
                Opcode::NoOp => { // NoOp
                }
                Opcode::Return => {
                    // Return
                    let result = self.pop();
                    self.close_upvalues(self.frames[frame_no].frame_pointer);
                    let Some(old_frame) = self.frames.pop() else {
                        return InterpretResult::RuntimeError;
                    };
                    if self.frames.is_empty() {
                        // self.pop();
                        return InterpretResult::Ok;
                    }
                    frame_no = self.frames.len() - 1;

                    self.stack_top = old_frame.frame_pointer;
                    self.push(result);
                }
                Opcode::Constant => {
                    // Constant
                    let constant_idx = self.frames[frame_no].read_byte();
                    let constant = self.frames[frame_no].get_constant(constant_idx as usize);
                    self.push(constant);
                }
                Opcode::ConstantLong => { // ConstantLong
                }
                Opcode::Negate => {
                    // Negate
                    let operand = self.peek(0);
                    let Number(n) = operand else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.place(0, Number(-n));
                }
                Opcode::AddInt | Opcode::AddFloat => {
                    exec_bin_op_number!(self, Number, +);
                }
                Opcode::Concat => {
                    // Concat
                    let b = self.pop();
                    let a = self.pop();
                    if is_string(&a).is_some() && is_string(&b).is_some() {
                        let Value::Object(Object::String(s1)) = a else {
                            runtime_error!(self, "Operands must be strings");
                            return InterpretResult::RuntimeError;
                        };
                        let Value::Object(Object::String(s2)) = b else {
                            runtime_error!(self, "Operands must be strings");
                            return InterpretResult::RuntimeError;
                        };
                        let s = s1.to_string() + &s2.to_string();
                        let s = Value::Object(Object::String(s.into()));
                        self.push(s);
                    } else {
                        runtime_error!(self, "Operands must be strings");
                        return InterpretResult::RuntimeError;
                    }
                }
                Opcode::PointerAdd => {
                    // PointerAdd
                    let Number(number::Number::Integer(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    let Value::Pointer(p) = self.pop() else {
                        runtime_error!(self, "Operand must be a pointer");
                        return InterpretResult::RuntimeError;
                    };
                    match p {
                        Pointer::Local(slot) => {
                            self.push(Value::Pointer(Pointer::Local(slot + n as usize)));
                        }
                        Pointer::Heap(idx) => {
                            self.push(Value::Pointer(Pointer::Heap(idx + n as usize)));
                        }
                        Pointer::Static(idx) => {
                            self.push(Value::Pointer(Pointer::Static(idx + n as usize)));
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::SubtractInt => {
                    // Subtract
                    exec_bin_op_number!(self, Number, -);
                }
                Opcode::SubtractFloat => {
                    // Subtract
                    exec_bin_op_number!(self, Number, -);
                }
                Opcode::PointerSubtract => {
                    // PointerSubtract
                    let Number(number::Number::Integer(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    let Value::Pointer(p) = self.pop() else {
                        runtime_error!(self, "Operand must be a pointer");
                        return InterpretResult::RuntimeError;
                    };
                    match p {
                        Pointer::Local(slot) => {
                            self.push(Value::Pointer(Pointer::Local(slot - n as usize)));
                        }
                        Pointer::Heap(idx) => {
                            self.push(Value::Pointer(Pointer::Heap(idx - n as usize)));
                        }
                        Pointer::Static(idx) => {
                            self.push(Value::Pointer(Pointer::Static(idx - n as usize)));
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::MultiplyInt => {
                    // Multiply
                    exec_bin_op_number!(self, Number, *);
                }
                Opcode::MultiplyFloat => {
                    // Multiply
                    exec_bin_op_number!(self, Number, *);
                }
                Opcode::DivideInt => {
                    // Divide
                    exec_bin_op_number!(self, Number, /);
                }
                Opcode::DivideFloat => {
                    // Divide
                    exec_bin_op_number!(self, Number, /);
                }
                Opcode::Ternary => {
                    // Ternary
                    let Bool(_) = self.peek(2) else {
                        runtime_error!(self, "Operand must be a boolean");
                        return InterpretResult::RuntimeError;
                    };

                    let alt = self.pop();
                    let conseq = self.pop();
                    let Bool(cond) = self.pop() else {
                        runtime_error!(self, "Operand must be a boolean");
                        return InterpretResult::RuntimeError;
                    };

                    if cond == true {
                        self.push(conseq);
                    } else {
                        self.push(alt);
                    }
                }
                Opcode::False => {
                    // False
                    self.push(Bool(false));
                }
                Opcode::Nil => {
                    // Nil
                    self.push(Nil);
                }
                Opcode::True => {
                    // True
                    self.push(Bool(true));
                }
                Opcode::Not => {
                    // Not
                    let operand = self.peek(0);
                    if operand.is_falsy() {
                        self.place(0, Bool(true));
                    } else {
                        self.place(0, Bool(false));
                    }
                }
                Opcode::Equal => {
                    // Equal
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Bool(a == b));
                }
                Opcode::GreaterInt => {
                    // Greater
                    exec_bin_op_number!(self, Bool, >);
                }
                Opcode::GreaterFloat => {
                    // Greater
                    exec_bin_op_number!(self, Bool, >);
                }
                Opcode::LessInt => {
                    // Less
                    exec_bin_op_number!(self, Bool, <);
                }
                Opcode::LessFloat => {
                    // Less
                    exec_bin_op_number!(self, Bool, <);
                }
                Opcode::PrintInt => {
                    // Print
                    let v = self.pop();
                    println!("{:?}", v);
                }
                Opcode::PrintFloat => {
                    // Print
                    let v = self.pop();
                    println!("{:?}", v);
                }
                Opcode::PrintBool => {
                    // Print
                    let v = self.pop();
                    println!("{:?}", v);
                }
                Opcode::PrintString => {
                    // Print
                    let v = self.pop();
                    println!("{:?}", v);
                }
                Opcode::PrintNil => {
                    // Print
                    let v = self.pop();
                    println!("{:?}", v);
                }
                Opcode::PrintPointer => {
                    // Print
                    let v = self.pop();
                    println!("{:?}", v);
                }

                Opcode::Pop => {
                    // Pop
                    self.pop();
                }
                Opcode::DefineGlobal => {
                    // DefineGlobal
                    let global_id = self.frames[frame_no].read_byte() as u8;
                    let v = self.pop();
                    self.static_segment[global_id as usize] = v.clone();
                    self.heap[self.brk] = v;
                    self.brk += 1;
                }
                Opcode::DefineGlobalLong => {
                    // DefineGlobalLong
                }
                Opcode::GetGlobal => {
                    // GetGlobal
                    let global_id = self.frames[frame_no].read_byte() as u8;

                    let v = self.static_segment[global_id as usize].clone();
                    self.push(v);
                }
                Opcode::GetGlobalLong => {
                    // GetGlobalLong
                }
                Opcode::SetGlobal => {
                    // SetGlobal
                    let global_id = self.frames[frame_no].read_byte() as u8;

                    let v = self.peek(0);
                    self.static_segment[global_id as usize] = v.clone();
                    self.heap[self.brk] = v;
                    self.brk += 1;
                }
                Opcode::SetGlobalLong => {
                    // SetGlobalLong
                }
                Opcode::GetLocal => {
                    // GetLocal
                    let slot = self.frames[frame_no].read_byte() as usize;
                    self.push(self.stack[self.frames[frame_no].frame_pointer + slot].clone());
                }
                Opcode::SetLocal => {
                    // SetLocal
                    let slot = self.frames[frame_no].read_byte() as usize;
                    self.stack[self.frames[frame_no].frame_pointer + slot] = self.peek(0);
                }
                Opcode::JumpIfFalse => {
                    // JumpIfFalse
                    let offset = self.frames[frame_no].read_short() as usize;
                    if self.peek(0).is_falsy() {
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
                    if !self.call_value(self.peek(arg_count), arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                    frame_no = self.frames.len() - 1;
                }
                Opcode::RefLocal => {
                    // Ref
                    let Number(number::Number::Integer(slot)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Value::Pointer(Pointer::Local(
                        self.frames[frame_no].frame_pointer + slot as usize)));
                }
                Opcode::RefGlobal => {
                    // global ref
                    let p = self.frames[frame_no].read_byte();
                    self.push(Value::Pointer(Pointer::Static(p as usize)));
                }
                Opcode::DerefGet => {
                    // deref
                    let Value::Pointer(p) = self.pop() else {
                        runtime_error!(self, "Operand must be a pointer");
                        return InterpretResult::RuntimeError;
                    };
                    match p {
                        Pointer::Local(slot) => {
                            self.push(self.stack[slot].clone());
                        }
                        Pointer::Heap(idx) => {
                            self.push(self.heap[idx].clone());
                        }
                        Pointer::Static(idx) => {
                            let v = self.static_segment.get(idx);
                            if let Some(v) = v {
                                self.push(v.clone());
                            } else {
                                runtime_error!(self, "Static segment out of bounds");
                                return InterpretResult::RuntimeError;
                            }
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::DerefAssign => {
                    // deref Assign mem
                    let val = self.pop();
                    let Value::Pointer(p) = self.pop() else {
                        runtime_error!(self, "Operand must be a pointer");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(val.clone());
                    match p {
                        Pointer::Local(slot) => {
                            self.stack[slot] = val;
                        }
                        Pointer::Heap(idx) => {
                            self.heap[idx] = val;
                        }
                        Pointer::Static(idx) => {
                            self.static_segment[idx] = val;
                        }
                        _ => {
                            runtime_error!(self, "Operand must be a pointer");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Opcode::DefineStackArray => {
                    // push stack top
                    self.push(Value::Pointer(Pointer::Local(self.stack_top + 1)));
                }
                Opcode::DefineGlobalArray => {
                    // define global array
                    let glob_id = self.frames[frame_no].read_byte();
                    let num = self.frames[frame_no].read_byte();
                    for i in 0..num {
                        self.static_segment[glob_id as usize + i as usize + 1] = self.pop();
                    }
                }
                Opcode::Closure => {
                    // closure
                    let p = self.frames[frame_no].read_byte();
                    let Value::Object(Object::Function(f)) = self.static_segment[p as usize].clone() else {
                        runtime_error!(self, "Operand must be a pointer to a function");
                        return InterpretResult::RuntimeError;
                    };

                    let mut closure = Closure::new(Pointer::Static(p as usize), f.upvalue_count as u32);
                    self.brk += 1;

                    for _ in 0..closure.upvalue_count {
                        let is_local = self.frames[frame_no].read_byte() != 0;
                        let index = self.frames[frame_no].read_byte();
                        if is_local {
                            closure.upvalues.push(self.capture_upvalue(self.frames[frame_no].frame_pointer + index as usize))
                        }
                        else {
                            closure.upvalues.push(self.frames[frame_no].closure.upvalues[index as usize].clone())
                        }
                    }

                    self.push(Value::Object(object::Object::Closure(closure)));
                    
                }
                Opcode::GetUpvalue => {
                    // getupcal
                    let idx = self.frames[frame_no].read_byte();
                    let upval = self.frames[frame_no].closure.upvalues[idx as usize].clone();
                    if let Some(cap) = upval.closed.borrow().as_ref() {
                        self.push(cap.clone());
                    } else {
                    let upv = self.stack[upval.index].clone();
                    self.push(upv)
                    };
                }
                Opcode::SetUpvalue => {
                    //setupval
                    let idx = self.frames[frame_no].read_byte();
                    let upval = self.frames[frame_no].closure.upvalues[idx as usize].clone();
                    if let Some(cap) = upval.closed.borrow_mut().as_mut() {
                        *cap.borrow_mut() = self.peek(0);
                    } else {
                        self.stack[upval.index] = self.peek(0);
                    };
                }
                Opcode::CloseUpvalue => {
                    // close value
                    self.close_upvalues(self.stack_top - 1);
                    self.pop();
                }
                Opcode::CastIntToFloat => {
                    // cast int to float
                    let Number(number::Number::Integer(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Number(number::Number::Float(n as f64)));
                }
                Opcode::CastFloatToInt => {
                    // cast float to int
                    let Number(number::Number::Float(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Number(number::Number::Integer(n as i64)));
                }
                Opcode::CastIntToString => {
                    // cast int to string
                    let Number(number::Number::Integer(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Value::Object(Object::String(n.to_string().into())));
                }
                Opcode::CastFloatToString => {
                    // cast float to string
                    let Number(number::Number::Float(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Value::Object(Object::String(n.to_string().into())));
                }
                Opcode::CastBoolToFloat => {
                    // cast bool to float
                    let Bool(b) = self.pop() else {
                        runtime_error!(self, "Operand must be a boolean");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Number(number::Number::Float(b as i64 as f64)));
                }
                Opcode::CastBoolToInt => {
                    // cast bool to int
                    let Bool(b) = self.pop() else {
                        runtime_error!(self, "Operand must be a boolean");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Number(number::Number::Integer(b as i64)));
                }
                Opcode::CastBoolToString => {
                    // cast bool to string
                    let Bool(b) = self.pop() else {
                        runtime_error!(self, "Operand must be a boolean");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Value::Object(Object::String(b.to_string().into())));
                }
                Opcode::CastIntToBool => {
                    // cast int to bool
                    let Number(number::Number::Integer(n)) = self.pop() else {
                        runtime_error!(self, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    };
                    self.push(Bool(n != 0));
                }

                _ => {
                    println!("Unknown opcode {}", instruction);
                }
            }
        }
    }

    fn close_upvalues(&mut self, last: usize) {
        while let Some(upval) = self.open_upvalues.clone()  {
            if upval.index < last {
                break;
            }
            let closed_var = Some(self.stack[upval.index].clone());
            *upval.closed.borrow_mut() = closed_var;
            *(self.open_upvalues.borrow_mut()) = upval.next.clone().take();
        }
    }

    fn capture_upvalue(&mut self, local_idx: usize) -> Rc<Upvalue> {

        let mut prev_upval = None;
        let mut maybe_upval = self.open_upvalues.clone();
        while let Some(upval) = maybe_upval.clone() {
            if upval.as_ref().index <= local_idx {
                break;
            }
            prev_upval = maybe_upval;
            maybe_upval = upval.next.clone().take();
        }

        if let Some(upval) = maybe_upval.clone() {
            if upval.index == local_idx {
                return upval;
            }
        }

        let u = Rc::new(Upvalue {
            index: local_idx,
            next: maybe_upval.into(),
            closed: None.into()
        });

        if let Some(p) = prev_upval {
            *p.next.borrow_mut() = Some(u.clone());
        } else {
            self.open_upvalues = Some(u.clone());
        }

        u

    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> bool {
        match callee {
            Value::Object(Object::NativeFunction(f)) => {
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(self.pop());
                }
                args.reverse();
                let result = f(&args);
                self.pop();
                self.push(result);
                return true;
            }
            Value::Pointer(Pointer::Static(idx)) => {
                return self.call(Closure::new(Pointer::Static(idx), 0), arg_count);
            }
            Value::Object(Object::Closure(c)) => {
                return self.call(c, arg_count);
            }
            _ => {
                runtime_error!(self, "Can only call closures and classes {:?}", callee);
                return false;
            }
        }
    }

    const FRAMES_MAX: usize = 255;

    fn call(&mut self, c: Closure, arg_count: usize) -> bool {
        let Pointer::Static(idx) = c.func else {
            runtime_error!(self, "Can only call closures and classes {:?}", c.func);
            return false;
        };

        let Value::Object(Object::Function(func)) = self.static_segment[idx].clone() else {
            runtime_error!(self, "Can only call closures and classes {:?}", idx);
            return false;
        };

        if func.arity != arg_count {
            runtime_error!(self, "Expected {} arguments but got {}", func.arity, arg_count);
            return false;
        }
        if self.frames.len() == Self::FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return false;
        }
        self.frames.push(CallFrame {
            closure: c,
            function: func,
            ip: 0,
            frame_pointer: self.stack_top - arg_count - 1,
        });
        return true;
    }

    fn define_native(&mut self, name: &str, function:fn(&[Value]) -> Value, function_type: ValueType) {
        self.static_segment.push(Value::Object(Object::NativeFunction(function))); 
        self.natives.insert(name.to_string(), (self.static_segment.len() - 1, function_type));
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top].clone()
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack_top - 1 - distance].clone()
    }

    fn place(&mut self, distance: usize, value: Value) {
        self.stack[self.stack_top - 1 - distance] = value;
    }
}
