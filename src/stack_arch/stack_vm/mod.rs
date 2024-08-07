use std::collections::HashMap;
use tracing::{ debug, event, span, Level };

use std::rc::Rc;

use crate::chunk::Opcode;

use crate::common::ParseResult;
use crate::stack_compiler;



use crate::common::runnable::{ StackVMFunction, Runnable };

use crate::value::pointer::Pointer;
use crate::typing::{ UValueType, ValueType };
use crate::value::Value::{ self, Bool, Nil };
use crate::value::Word;

const STACK_MAX: usize = 256;
const HEAP_MAX: usize = 2048;
pub struct VM {
  stack: [Word; STACK_MAX],
  stack_top: usize,
  natives: HashMap<SharedString, (usize, UValueType)>,
  heap: [Word; HEAP_MAX],
  static_segment: Vec<Word>,
  function_segment: Vec<Runnable>,
  brk: usize,

  frames: Vec<CallFrame>,
}

struct CallFrame {
  // closure: Closure,
  function: Rc<StackVMFunction>,
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
    (u16::from(b1) << 8) | u16::from(b2)
  }

  fn get_constant(&mut self, constant_idx: usize) -> Value {
    self.function.chunk.constants[constant_idx].clone()
  }
}

#[derive(Debug)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
}

macro_rules! runtime_error {
  ($self:expr, $fmt:literal $(, $args:expr)*) => {
        {
            print!("Line {}: Runtime Error: ", line!());
            println!($fmt $(, $args)*);
            for frame in $self.frames.iter().rev() {
                let function = &frame.function;
                let instruction = frame.ip - 1;
                let line = function.chunk.code[instruction].1;
                println!("[line {}] in {}()", line, function.name);
            }

            $self.stack_top = 0;
        }
  };
}

fn clock_native(_vm: &mut VM, _args: &[Word]) -> Value {
  let now = std::time::SystemTime::now();
  let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
  Value::Float(duration.as_secs_f64())
}

fn sbrk_native(vm: &mut VM, args: &[Word]) -> Value {
  let n = args[0].to_i64();
  let ret = Value::Pointer(Pointer::Heap(vm.brk));
  vm.brk += n as usize;
  ret
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
      static_segment: Vec::new(),
      function_segment: Vec::new(),
    };
    vm.define_native(
      "clock",
      clock_native,
      ValueType::Closure(Box::new([ValueType::Float.intern()]), 0).intern()
    );
    vm.define_native(
      "sbrk",
      sbrk_native,
      ValueType::Closure(
        Box::new([
          ValueType::Integer.intern(),
          ValueType::Pointer(ValueType::Nil.intern(), true).intern(),
        ]), 0
      ).intern()
    );
    vm
  }

  pub fn interpret(&mut self, parsed: ParseResult) -> InterpretResult {
    let compiled = stack_compiler::compile(parsed, &self.natives);

    let Some(func) = compiled.function else {
      return InterpretResult::CompileError;
    };
    let f = Rc::new(func);

    self.static_segment = compiled.static_data_segment;
    self.function_segment.extend(compiled.function_segment);

    self.function_segment.push(Runnable::StackVMFunction(f.clone()));
    let f = 
      Pointer::Function(self.function_segment.len() - 1);
    // self.brk += 1;
    self.push(f.clone().to_word());

    self.call(f, 0, 0);

    self.run()
  }

  fn run(&mut self) -> InterpretResult {
    let mut frame_no = self.frames.len() - 1;
    let lspan = span!(Level::TRACE, "vm::run");
    loop {
      let _guard = lspan.enter();

      event!(Level::TRACE, "stack: {}", self.frames[frame_no].frame_pointer);
      event!(Level::TRACE, "stack: {:?}", &self.stack[0..self.stack_top]);

      let instruction = self.frames[frame_no].read_byte();

      // only run this if the tracing level allows debug statemnts
      if tracing::event_enabled!(Level::DEBUG) {
        self.frames[frame_no].function.chunk.disassemble_instruction(
          self.frames[frame_no].ip - 1,
          Level::DEBUG
        );
      }

      match Opcode::from(instruction) {
        Opcode::NoOp => {
          // NoOp
        }
        Opcode::Return => {
          // Return
          let num_bytes = self.frames[frame_no].read_byte();
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
        Opcode::ConstantLong => {
          // ConstantLong
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
        Opcode::AddUint => {
          let b = self.pop().to_u64();
          let a = self.peek(0).to_u64();
          self.stack[self.stack_top - 1] = Value::UInteger(a.wrapping_add(b)).to_word();
        }
        Opcode::AddFloat => {
          let b = self.pop().to_float();
          let a = self.peek(0).to_float();
          self.stack[self.stack_top - 1] = Value::Float(a + b).to_word();
        }
        #[allow(clippy::cast_possible_wrap)]
        Opcode::Concat => {
          // Concat
          let b = self.pop().to_pointer();
          let a = self.pop().to_pointer();
          let s1 = self.get_SharedString(a);
          let s2 = self.get_SharedString(b);
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
          let n = self.pop().to_usize();
          let p = self.pop().to_pointer();
          let slize = self.frames[frame_no].read_byte() as usize;
          match p {
            Pointer::Local(slot) => {
              self.push(Value::Pointer(Pointer::Local(slot + n * slize)).to_word());
            }
            Pointer::Heap(idx) => {
              self.push(Value::Pointer(Pointer::Heap(idx + n * slize)).to_word());
            }
            Pointer::Static(idx) => {
              self.push(Value::Pointer(Pointer::Static(idx + n * slize)).to_word());
            }
            Pointer::Function(_) => {
              runtime_error!(self, "Operand must be a pointer");
              return InterpretResult::RuntimeError;
            }
          }
        }

        #[allow(clippy::cast_possible_wrap)]
        Opcode::FrameBase => {
          let base = self.frames[frame_no].frame_pointer;
          self.push(Value::Integer(base as i64).to_word());
        }

        Opcode::SubtractInt => {
          let b = self.pop().to_i64();
          let a = self.peek(0).to_i64();
          self.stack[self.stack_top - 1] = Value::Integer(a - b).to_word();
        }
        Opcode::SubtractUint => {
          let b = self.pop().to_u64();
          let a = self.peek(0).to_u64();
          self.stack[self.stack_top - 1] = Value::UInteger(a.wrapping_sub(b)).to_word();
        }
        Opcode::SubtractFloat => {
          let b = self.pop().to_float();
          let a = self.peek(0).to_float();
          self.stack[self.stack_top - 1] = Value::Float(a - b).to_word();
        }
        Opcode::PointerSubtract => {
          // PointerSubtract
          let n = self.pop().to_usize();
          let p = self.pop().to_pointer();
          let slize = self.frames[frame_no].read_byte() as usize;
          match p {
            Pointer::Local(slot) => {
              self.push(Value::Pointer(Pointer::Local(slot - n * slize)).to_word());
            }
            Pointer::Heap(idx) => {
              self.push(Value::Pointer(Pointer::Heap(idx - n * slize)).to_word());
            }
            Pointer::Static(idx) => {
              self.push(Value::Pointer(Pointer::Static(idx - n * slize)).to_word());
            }
            Pointer::Function(_) => {
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
        Opcode::MultiplyUint => {
          let b = self.pop().to_u64();
          let a = self.peek(0).to_u64();
          self.stack[self.stack_top - 1] = Value::UInteger(a * b).to_word();
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
        Opcode::DivideUint => {
          let a = self.pop().to_u64();
          let b = self.peek(0).to_u64();
          self.stack[self.stack_top - 1] = Value::UInteger(b / a).to_word();
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

          if cond {
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
        Opcode::GreaterUint => {
          // Greater
          let b = self.pop().to_u64();
          let a = self.peek(0).to_u64();
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
        Opcode::LessUint => {
          // Less
          let b = self.pop().to_u64();
          let a = self.peek(0).to_u64();
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
          println!("{v:?}");
        }
        Opcode::PrintUint => {
          // Print
          let v = self.pop().to_u64();
          println!("{v:?}");
        }
        Opcode::PrintFloat => {
          // Print
          let v = self.pop().to_float();
          println!("{v:?}");
        }
        Opcode::PrintBool => {
          // Print
          let v = self.pop().to_bool();
          println!("{v:?}");
        }
        Opcode::PrintSharedString => {
          // Print
          let v = self.pop();
          let s = self.get_SharedString(v.to_pointer());
          println!("{s}");
        }
        Opcode::PrintChar => {
          // Print
          let v = self.pop().to_char();
          println!("{v}");
        }
        Opcode::PrintNil => {
          // Print
          let _ = self.pop();
          println!("{Nil:?}");
        }
        Opcode::PrintPointer => {
          // Print
          let v = self.pop().to_pointer();
          println!("{v:?}");
        }

        Opcode::Pop => {
          // Pop
          let slize = self.frames[frame_no].read_byte() as usize;
          self.pop_many(slize);
        }
        Opcode::DefineGlobal => {
          // DefineGlobal
          let global_id = self.frames[frame_no].read_byte() as usize;
          let slize = self.frames[frame_no].read_byte() as usize;
          let v = self.pop_many(slize);

          self.static_segment[global_id..global_id + slize].copy_from_slice(&v);
          self.heap[self.brk..self.brk + slize].copy_from_slice(&v);
          self.brk += slize;
          // self.push_many(&v);
        }
        Opcode::DefineGlobalLong => {
          // DefineGlobalLong
        }
        Opcode::GetGlobal => {
          // GetGlobal
          let global_id = self.frames[frame_no].read_byte();
          let slize = self.frames[frame_no].read_byte() as usize;
          let v = self.static_segment[global_id as usize..(global_id as usize) + slize].to_vec();
          self.push_many(&v);
        }
        // Opcode::CopyClosure => {
        //   unsafe {
        //     Rc::increment_strong_count(self.peek(0).to_u64() as *const Vec<Word>);
        //   }
        // }
        Opcode::GetGlobalLong => {
          // GetGlobalLong
        }

        Opcode::GetLocal => {
          // GetLocal
          let slot = self.frames[frame_no].read_byte() as i8 as i64;
          let slize = self.frames[frame_no].read_byte() as usize;
          debug!("GetLocal: slot: {slot}, slize: {slize}");
          // self.push(self.stack[self.frames[frame_no].frame_pointer + slot].clone());
          let v =
            self.stack[
              (self.frames[frame_no].frame_pointer as i64 + slot) as usize..(self.frames[frame_no].frame_pointer as i64 +
                slot +
                slize as i64) as usize
            ].to_vec();

          debug!("GetLocal: v: {v:?}");

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
          let capture_size = self.frames[frame_no].read_byte() as usize;


          // let args = self.peek_many(0, arg_count);
          let f = self.peek(arg_count).to_pointer();
          let captures = self.peek_many(arg_count + 1, capture_size);
          
          if !self.call(f, arg_count, capture_size) {
            return InterpretResult::RuntimeError;
          }
          self.push_many(&captures);
          frame_no = self.frames.len() - 1;
        }
        Opcode::RefLocal => {
          // Ref
          let slot = self.pop().to_usize();
          self.push(Value::Pointer(Pointer::Local(slot)).to_word());
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
              let v = self.stack[slot..slot + slize].to_vec();
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

            Pointer::Function(_) => {
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
              self.stack[slot..slize + slot].copy_from_slice(&v[..slize]);
            }
            Pointer::Heap(idx) => {
              self.heap[idx..slize + idx].copy_from_slice(&v[..slize]);
            }
            Pointer::Static(idx) => {
              self.static_segment[idx..slize + idx].copy_from_slice(&v[..slize]);
            }

            Pointer::Function(_) => {
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
          for i in (0..num).rev() {
            let v = self.pop_many(slize);
            let static_place = (glob_id as usize) + i * slize;
            self.static_segment[static_place..static_place + slize].copy_from_slice(&v);
          }
        }
        Opcode::CastIntToFloat => {
          // cast int to float
          let n = self.pop().to_i64();
          self.push(Value::Float(n as f64).to_word());
        }
        #[allow(clippy::cast_possible_truncation)]
        Opcode::CastFloatToInt => {
          // cast float to int
          let n = self.pop().to_float();
          self.push(Value::Integer(n as i64).to_word());
        }
        Opcode::CastIntToSharedString => {
          // cast int to SharedString
          let n = self.pop().to_i64();
          // self.push(Value::Object(Object::SharedString(n.to_string().into())));
          self.push(Value::Pointer(Pointer::Heap(self.brk)).to_word());
          self.heap[self.brk] = Value::Integer(1).to_word();
          self.brk += 1;
          let s = n.to_string();
          for i in 0..s.len() {
            self.heap[self.brk] = Value::Char(s.as_bytes()[i]).to_word();
            self.brk += 1;
          }
        }
        Opcode::CastFloatToSharedString => {
          // cast float to SharedString
          let n = self.pop().to_float();
          // self.push(Value::Object(Object::SharedString(n.to_string().into())));
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
          self.push(Value::Float(i64::from(b) as f64).to_word());
        }
        Opcode::CastBoolToInt => {
          // cast bool to int
          let b = self.pop().to_bool();
          self.push(Value::Integer(i64::from(b)).to_word());
        }
        Opcode::CastBoolToSharedString => {
          // cast bool to SharedString
          let b = self.pop().to_bool();
          // self.push(Value::Object(Object::SharedString(b.to_string().into())));
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
        Opcode::CastBoolToUint => {
          // cast bool to uint
          let b = self.peek(0).to_bool();
          self.place(0, Value::UInteger(i64::from(b) as u64).to_word());
        }
        Opcode::CastUintToBool => {
          // cast uint to bool
          let n = self.peek(0).to_u64();
          self.place(0, Value::Bool(n != 0).to_word());
        }
        Opcode::CastUintToInt => {
          // cast uint to int
          let n = self.pop().to_u64();
          self.push(Value::Integer(n as i64).to_word());
        }
        Opcode::CastIntToUint => {
          // cast int to uint
          let n = self.pop().to_i64();
          self.push(Value::UInteger(n as u64).to_word());
        }
        Opcode::CastUintToFloat => {
          // cast uint to float
          let n = self.pop().to_u64();
          self.push(Value::Float(n as f64).to_word());
        }
        Opcode::Break | Opcode::Continue =>
          unreachable!(
            "Break and Continue are 'sudo instructions' and should be stripped by the compiler"
          ),
      }
    }
  }

  const FRAMES_MAX: usize = 255;

  fn call(&mut self, f: Pointer, arg_count: usize, capture_size : usize) -> bool {
    let Pointer::Function(idx) = f else {
      runtime_error!(self, "Can only call closures and classes dvs {:?}", f);
      return false;
    };

    let Runnable::StackVMFunction(func) = self.function_segment[idx].clone() else {
      if let Runnable::NativeFunction(f) = self.function_segment[idx].clone() {
        let mut args = Vec::with_capacity(arg_count);
        for i in 0..arg_count {
          args.push(self.peek(i));
        }
        let result = f(self, &args);
        self.pop_many(arg_count);
        self.pop_many(1);

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
      function: func,
      ip: 0,
      frame_pointer: self.stack_top - arg_count - capture_size - 1,
    });
    true
  }

  fn define_native(
    &mut self,
    name: &str,
    function: fn(&mut Self, &[Word]) -> Value,
    function_type: UValueType
  ) {
    self.function_segment.push(Runnable::NativeFunction(function));
    self.natives.insert(name.to_string(), (self.function_segment.len() - 1, function_type));
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

  fn peek_many(&self, distance: usize, count: usize) -> Vec<Word> {
    self.peek_range(distance..count+distance)
  }

  fn peek_range(&self, range: std::ops::Range<usize>) -> Vec<Word> {
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

  fn get_SharedString(&mut self, p: Pointer) -> SharedString {
    match p {
      Pointer::Heap(idx) => {
        let mut s = String::new();
        let len = self.heap[idx].to_usize();
        for i in 0..len {
          s.push(self.heap[idx + 1 + i].to_char());
        }
        s
      }
      Pointer::Static(idx) => {
        let mut s = String::new();
        let len = self.static_segment[idx].to_usize();
        for i in 0..len {
          s.push(self.static_segment[idx + 1 + i].to_char());
        }
        s
      }
      _ => {
        runtime_error!(self, "Operand must be a pointer to a SharedString");
        String
      }
    }
  }
}
