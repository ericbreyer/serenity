use std::{io::Write, ops::ControlFlow};

use tracing::debug;

use crate::{
    prelude::*,
    reg_compiler::iasm::{IASMCommandType, IASMLabel, IASMLine, IASMParam, IASMProg},
    value::{pointer::Pointer, Value, Word},
};

const STATIC_SIZE: usize = 2048;
const STACK_SIZE: usize = 2048;
const REGISTER_COUNT: usize = 2048;

pub enum ExitReason {
    Err(SharedString),
    Halt,
}

pub struct VM<'a, T>  where T: Write {
    static_data: [Word; STATIC_SIZE],
    text_segment: IASMProg,
    stack: [Word; STACK_SIZE],
    ip: usize,
    fp: usize,
    sp: usize,
    registers: [Word; 2048],
    jump: usize,
    output: &'a mut T,
}

impl<'a, T> VM<'a, T> where T: Write {
    pub fn new(prog: IASMProg, output: &mut T) -> VM<T> {
        const N: Word = Word(0);
        VM {
            static_data: [N; STATIC_SIZE],
            stack: [N; STACK_SIZE],
            ip: 0,
            fp: 0,
            sp: 3,
            jump: 0,
            registers: [N; REGISTER_COUNT],
            text_segment: prog,
            output,
        }
    }

    pub fn run(&mut self) -> ExitReason {
        loop {
            let s = self.step();
            if let ControlFlow::Break(e) = s {
                return e;
            }
        }
    }

    pub fn step(&mut self) -> ControlFlow<ExitReason, ()> {
        let line = &self.text_segment.text[self.ip];

        let IASMLine::Statement(raw_statement) = line else {
            self.ip += 1;
            return ControlFlow::Continue(());
        };

        let mut statement = raw_statement.clone();
        for param in statement.params.iter_mut() {
            match param {
                IASMParam::Immediate(Value::Pointer(Pointer::Local(i))) => {
                    *param = IASMParam::Immediate(Value::Pointer(Pointer::Local(*i + self.fp)));
                }
                _ => {}
            }
        }

        let p = &statement.params;
        let p0 = p
            .get(0)
            .unwrap_or(&IASMParam::Immediate(Value::UInteger(0)));
        let p1 = p
            .get(1)
            .unwrap_or(&IASMParam::Immediate(Value::UInteger(0)));
        let p2 = p
            .get(2)
            .unwrap_or(&IASMParam::Immediate(Value::UInteger(0)));

        debug!("------");
        debug!("{:?}", statement);
        // print value of all registers in statement
        for param in statement.params.iter() {
            match param {
                IASMParam::Register(r) => {
                    debug!("Register {}: {:?}", r.0, self.registers[r.0]);
                }
                _ => {}
            }
        }
        debug!("Stack: {:?}", &self.stack[0..self.sp]);
        debug!("static: {:?}", &self.static_data[0..15]);

        match statement.command {
            IASMCommandType::PrintFloat => {
                let value = self.get_val(p0)?.to_float();
                if let Err(e) = writeln!(self.output, "{:?}", value) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintInt => {
                // PrintInt reg
                let value = self.get_val(p0)?.to_i64();
                if let Err(e) = writeln!(self.output, "{:?}", value) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintUint => {
                // PrintInt reg
                let value = self.get_val(p0)?.to_u64();
                if let Err(e) = writeln!(self.output, "{:?}", value) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintBool => {
                let value = self.get_val(p0)?.to_bool();
                if let Err(e) = writeln!(self.output, "{:?}", value) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintNil => {
                if let Err(e) = writeln!(self.output, "nil") {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintPointer => {
                let value = self.get_val(p0)?.to_pointer();
                if let Err(e) = writeln!(self.output, "{:?}", value) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintChar => {
                let value = self.get_val(p0)?.to_char();
                if let Err(e) = write!(self.output, "{}", value) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::PrintClosure => {
                // printclosure reg num_upvals
                let loc = self.get_val(p0)?.to_pointer();
                let Value::UInteger(upvals) = self.get_immediate(p1)? else {
                    return ControlFlow::Break(ExitReason::Err("Bad".into()));
                };
                let value = self.access_mem(loc, upvals as usize);
                if let Err(e) = writeln!(self.output, "{:?} {:?}:{}", loc, value.to_pointer(), upvals) {
                    return ControlFlow::Break(ExitReason::Err(e.to_string().into()));
                }
            }
            IASMCommandType::JumpIfFalse => {
                // jumpiffalse boolreg label
                let pred = self.get_val(p0)?.to_bool();
                let jump_to = self.get_label(p1)?;
                if !pred {
                    self.ip = jump_to;
                    return ControlFlow::Continue(());
                }
            }
            IASMCommandType::JumpIfTrue => {
                let pred = self.get_val(p0)?.to_bool();
                let jump_to = self.get_label(p1)?;
                if pred {
                    self.ip = jump_to;
                    return ControlFlow::Continue(());
                }
            }
            IASMCommandType::Jump => {
                let jump_to = self.get_label(p0)?;
                self.ip = jump_to;
                return ControlFlow::Continue(());
            }
            IASMCommandType::Return => {
                // Return reg
                let return_val = self.get_val(p0)?;
                let tnum_words = self.get_immediate(p1)?;

                let return_place = &mut self.registers[self.stack[self.fp + 2].to_u64() as usize];

                let Value::UInteger(num_words) = tnum_words else {
                    return ControlFlow::Break(ExitReason::Err(
                        format!("Expected UInteger in Return").into(),
                    ));
                };

                // returning an aggregate type
                if num_words >= 1 {
                    let fromp = return_val.to_pointer();
                    let top = return_place.to_pointer();

                    match (fromp, top) {
                        (Pointer::Local(from), Pointer::Local(to)) => {
                            let t = self.stack[from..from + (num_words as usize)].to_vec();
                            self.stack[to..to + (num_words as usize)].copy_from_slice(t.as_slice());
                        }
                        (Pointer::Static(from), Pointer::Static(to)) => {
                            let t = self.static_data[from..from + (num_words as usize)].to_vec();
                            self.static_data[to..to + (num_words as usize)]
                                .copy_from_slice(t.as_slice());
                        }
                        (Pointer::Local(from), Pointer::Static(to)) => {
                            self.static_data[to..to + (num_words as usize)]
                                .copy_from_slice(&self.stack[from..from + (num_words as usize)]);
                        }
                        (Pointer::Static(from), Pointer::Local(to)) => {
                            self.stack[to..to + (num_words as usize)].copy_from_slice(
                                &self.static_data[from..from + (num_words as usize)],
                            );
                        }
                    }
                } else {
                    *return_place = return_val;
                }

                let return_address = self.stack[self.fp + 1].to_u64();

                self.ip = return_address as usize;

                let old_fp = self.stack[self.fp - 1].to_i64();

                self.sp = self.fp - 1;
                self.fp = old_fp as usize;
                return ControlFlow::Continue(());
            }
            IASMCommandType::LoadImediate => {
                // LoadImmediate to_reg value
                let reg = self.get_register(p0)?;
                let value = self.get_immediate(p1)?;

                self.registers[reg] = value.to_word();
            }
            IASMCommandType::NegateFloat => {
                let reg = self.get_register(p0)?;
                let value = self.get_val(p1)?.to_float();
                self.registers[reg] = Value::Float(-value).to_word()
            }
            IASMCommandType::NegateInt => {
                let reg = self.get_register(p0)?;
                let value = self.get_val(p1)?.to_i64();
                self.registers[reg] = Value::Integer(-value).to_word()
            }
            IASMCommandType::NegateUint => {
                let reg = self.get_register(p0)?;
                let value = self.get_val(p1)?.to_u64();
                self.registers[reg] = Value::Integer(-(value as i64)).to_word()
            }
            IASMCommandType::Not => {
                let reg = self.get_register(p0)?;
                let value = self.get_val(p1)?.to_bool();
                self.registers[reg] = Value::Bool(!value).to_word();
            }
            IASMCommandType::Load => {
                // Load to_reg from_loc_reg
                let to_reg = self.get_register(p0)?;
                let ptr = self.get_val(p1)?.to_pointer();

                match ptr {
                    Pointer::Static(loc) => {
                        self.registers[to_reg] = self.static_data[loc];
                    }
                    Pointer::Local(loc) => {
                        self.registers[to_reg] = self.stack[loc];
                    }
                }
            }
            IASMCommandType::PointerAdd => {
                // PointerAdd to_reg from_reg offset
                let to_reg = self.get_register(p0)?;
                let ptr = self.get_val(p1)?.to_pointer();
                let offset = self.get_val(p2)?.to_u64();

                self.registers[to_reg] = match ptr {
                    Pointer::Local(loc) => {
                        Value::Pointer(Pointer::Local(loc + (offset as usize))).to_word()
                    }
                    Pointer::Static(loc) => {
                        Value::Pointer(Pointer::Static(loc + (offset as usize))).to_word()
                    }
                };
            }
            IASMCommandType::AddInt => {
                // AddInt to_reg from_reg1 from_reg2
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_i64();
                let i2 = self.get_val(p2)?.to_i64();

                self.registers[to_reg] = Value::Integer(i1 + i2).to_word();
            }
            IASMCommandType::AddUint => {
                // AddInt to_reg from_reg1 from_reg2
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_u64();
                let i2 = self.get_val(p2)?.to_u64();

                self.registers[to_reg] = Value::UInteger(i1 + i2).to_word();
            }
            IASMCommandType::AddFloat => {
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_float();
                let i2 = self.get_val(p2)?.to_float();

                self.registers[to_reg] = Value::Float(i1 + i2).to_word();
            }
            IASMCommandType::SubInt => {
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_i64();
                let i2 = self.get_val(p2)?.to_i64();

                self.registers[to_reg] = Value::Integer(i1 - i2).to_word();
            }
            IASMCommandType::SubUint => todo!(),
            IASMCommandType::SubFloat => todo!(),
            IASMCommandType::PointerSub => todo!(),
            IASMCommandType::MulInt => {
                // AddInt to_reg from_reg1 from_reg2
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_i64();
                let i2 = self.get_val(p2)?.to_i64();

                self.registers[to_reg] = Value::Integer(i1 * i2).to_word();
            }
            IASMCommandType::MulUint => todo!(),
            IASMCommandType::MulFloat => todo!(),
            IASMCommandType::DivInt => {
                // AddInt to_reg from_reg1 from_reg2
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_i64();
                let i2 = self.get_val(p2)?.to_i64();

                self.registers[to_reg] = Value::Integer(i1 / i2).to_word();
            },
            IASMCommandType::DivUint => todo!(),
            IASMCommandType::DivFloat => {
                let to_reg = self.get_register(p0)?;
                let i1 = self.get_val(p1)?.to_float();
                let i2 = self.get_val(p2)?.to_float();

                self.registers[to_reg] = Value::Float(i1 / i2).to_word();
            },
            IASMCommandType::Equal => {
                // move to_reg l_reg r_reg
                let to_reg = self.get_register(p0)?;
                let left = self.get_val(p1)?;
                let right = self.get_val(p2)?;

                self.registers[to_reg] = Value::Bool(left == right).to_word();
            }
            IASMCommandType::NotEqual => {
                let to_reg = self.get_register(p0)?;
                let left = self.get_val(p1)?;
                let right = self.get_val(p2)?;
                self.registers[to_reg] = Value::Bool(left != right).to_word();
            }
            IASMCommandType::GreaterInt => {
                let to_reg = self.get_register(p0)?;
                let left = self.get_val(p1)?.to_i64();
                let right = self.get_val(p2)?.to_i64();
                self.registers[to_reg] = Value::Bool(left > right).to_word();
            }
            IASMCommandType::GreaterUint => todo!(),
            IASMCommandType::GreaterFloat => todo!(),
            IASMCommandType::GreaterEqualInt => todo!(),
            IASMCommandType::GreaterEqualUint => todo!(),
            IASMCommandType::GreaterEqualFloat => todo!(),
            IASMCommandType::LessInt => {
                // move to_reg l_reg r_reg
                let to_reg = self.get_register(p0)?;
                let left = self.get_val(p1)?.to_i64();
                let right = self.get_val(p2)?.to_i64();
                self.registers[to_reg] = Value::Bool(left < right).to_word();
            }
            IASMCommandType::LessUint => todo!(),
            IASMCommandType::LessFloat => todo!(),
            IASMCommandType::LessEqualInt => {
                // move to_reg l_reg r_reg
                let to_reg = self.get_register(p0)?;
                let left = self.get_val(p1)?.to_i64();
                let right = self.get_val(p2)?.to_i64();
                self.registers[to_reg] = Value::Bool(left <= right).to_word();
            }
            IASMCommandType::LessEqualUint => todo!(),
            IASMCommandType::LessEqualFloat => todo!(),
            IASMCommandType::Move => {
                // Move to_reg from_reg
                let to_reg = self.get_register(p0)?;
                let from_val = self.get_val(p1)?;
                self.registers[to_reg] = from_val;
            }
            IASMCommandType::MemCpy => {
                // MemCpy to_loc_reg from_loc_reg num_words
                let to_ptr = self.get_val(p0)?.to_pointer();
                let from_ptr = self.get_val(p1)?.to_pointer();
                let inum_words = self.get_immediate(p2)?;
                let Value::UInteger(num_words) = inum_words else {
                    return ControlFlow::Break(ExitReason::Err(
                        format!("Expected UInteger in MemCpy").into(),
                    ));
                };

                let data = (match from_ptr {
                    Pointer::Static(loc) => &self.static_data[loc..loc + (num_words as usize)],
                    Pointer::Local(loc) => &self.stack[loc..loc + (num_words as usize)],
                })
                .to_vec();

                match to_ptr {
                    Pointer::Static(loc) => {
                        self.static_data[loc..loc + (num_words as usize)].copy_from_slice(&data);
                    }
                    Pointer::Local(loc) => {
                        self.stack[loc..loc + (num_words as usize)].copy_from_slice(&data);
                    }
                }
            }
            IASMCommandType::Store => {
                // Store to_loc_reg from_reg
                let ptr = self.get_val(p0)?.to_pointer();
                let val = self.get_val(p1)?;

                match ptr {
                    Pointer::Static(loc) => {
                        self.static_data[loc] = val;
                    }
                    Pointer::Local(loc) => {
                        self.stack[loc] = val;
                    }
                }
            }
            IASMCommandType::PreCall => {
                // Call return_reg closure_loc_reg [param_reg...] (num_captures)
                let return_place = p0;
                let closure_loc = self.get_val(p1)?;
                let tnum_caps = self.get_immediate(p2)?;
                let Value::UInteger(num_caps) = tnum_caps else {
                    return ControlFlow::Break(ExitReason::Err(
                        format!("Expected UInteger in Call").into(),
                    ));
                };

                // push old fp onto stack
                self.stack[self.sp] = Value::Integer(self.fp as i64).to_word();
                self.sp += 1;

                // set new fp
                self.fp = self.sp;

                // push closure_loc_reg onto stack
                self.stack[self.sp] = closure_loc;
                self.sp += 1;

                // push (placeholder) return address onto stack
                self.stack[self.sp] = Value::UInteger((self.ip + 1) as u64).to_word();
                self.sp += 1;

                // push return location onto stack
                self.stack[self.sp] =
                    Value::UInteger(self.get_register(return_place)? as u64).to_word();
                self.sp += 1;

                //find the closure
                let closure_ptr = closure_loc.to_pointer();

                let loc = (match closure_ptr {
                    Pointer::Static(loc) => loc,
                    Pointer::Local(loc) => loc,
                }) + (num_caps as usize);

                let mem = match closure_ptr {
                    Pointer::Static(_) => self.static_data.clone(),
                    Pointer::Local(_) => self.stack,
                };

                self.jump = match mem[loc].to_pointer() {
                    Pointer::Static(loc) => loc,
                    _ => {
                        return ControlFlow::Break(ExitReason::Err(
                            format!(
                                "Expected Static Pointer in Call got {:?} line {:?} {:?}",
                                mem[loc], statement.line, loc
                            )
                            .into(),
                        ));
                    }
                };
            }
            IASMCommandType::Call => {
                self.stack[self.fp + 1] = Value::UInteger((self.ip + 1) as u64).to_word();
                self.ip = self.jump;
                return ControlFlow::Continue(());
            }
            IASMCommandType::Push => {
                // Push reg
                self.stack[self.sp] = self.get_val(p0)?;
                self.sp += 1;
            }
            IASMCommandType::Pop => {
                // Pop

                self.sp -= 1;
            }
            IASMCommandType::CastIntToFloat => {
                let to_reg = self.get_register(p0)?;
                let from_val = self.get_val(p1)?.to_i64();
                self.registers[to_reg] = Value::Float(from_val as f64).to_word();
            },
            IASMCommandType::CastUintToFloat => todo!(),
            IASMCommandType::CastFloatToInt => {
                let to_reg = self.get_register(p0)?;
                let from_val = self.get_val(p1)?.to_float();
                self.registers[to_reg] = Value::Integer(from_val as i64).to_word();
            },
            IASMCommandType::CastUintToInt => todo!(),
            IASMCommandType::CastIntToUint => todo!(),
            IASMCommandType::CastBoolToInt => todo!(),
            IASMCommandType::CastBoolToUint => todo!(),
            IASMCommandType::CastBoolToFloat => todo!(),
            IASMCommandType::CastIntToBool => todo!(),
            IASMCommandType::CastUintToBool => todo!(),
            IASMCommandType::Halt => {
                // Halt
                return ControlFlow::Break(ExitReason::Halt);
            }
            IASMCommandType::Nop => todo!(),
        }

        self.ip += 1;
        return ControlFlow::Continue(());
    }

    fn access_mem(&self, ptr: Pointer, offset: usize) -> Word {
        match ptr {
            Pointer::Local(i) => self.stack[i + offset],
            Pointer::Static(i) => self.static_data[i + offset],
        }
    }

    fn get_register(&self, param: &IASMParam) -> ControlFlow<ExitReason, usize> {
        param
            .get_register()
            .map(|r| ControlFlow::Continue(r))
            .unwrap_or(ControlFlow::Break(ExitReason::Err(
                format!("Param was not a register at line {}", self.ip).into(),
            )))
    }


    fn get_immediate(&self, param: &IASMParam) -> ControlFlow<ExitReason, Value> {
        param
            .get_immediate()
            .map(|r| ControlFlow::Continue(r))
            .unwrap_or(ControlFlow::Break(ExitReason::Err(
                format!("Param was not a register at line {}", self.ip).into(),
            )))
    }
    fn get_label(&self, param: &IASMParam) -> ControlFlow<ExitReason, usize> {
        param
            .get_label()
            .and_then(|l| match l {
                IASMLabel::Resolved(u) => Ok(u),
                _ => Err(()),
            })
            .map(|r| ControlFlow::Continue(r))
            .unwrap_or(ControlFlow::Break(ExitReason::Err(
                format!("Param was not a resolved label at line {}", self.ip).into(),
            )))
    }
    fn get_val(&self, param: &IASMParam) -> ControlFlow<ExitReason, Word> {
        match param {
            IASMParam::Register(r) => ControlFlow::Continue(self.registers[r.0]),
            IASMParam::Label(l) => match l {
                &IASMLabel::Resolved(r) => {
                    ControlFlow::Continue(Value::Pointer(Pointer::Static(r)).to_word())
                }
                IASMLabel::Named(_) => {
                    ControlFlow::Break(ExitReason::Err("Labels must be resolved".into()))
                }
            },
            IASMParam::Immediate(v) => ControlFlow::Continue(v.to_word()),
        }
    }
}
