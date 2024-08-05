use std::{collections::HashMap, ops::ControlFlow};

use crate::value::{pointer::Pointer, Value};

use super::iasm::{self, IASMCommandType, IASMParam, IASMProg};

pub fn optimize(prog: IASMProg) -> IASMProg {
    let mut optimized = prog;
    while let ControlFlow::Continue(new_prog) = unused_registers(&optimized) {
        optimized = new_prog
    }

    optimized = immediate_registers(&optimized);

    return optimized;
}

fn immediate_registers(prog: &IASMProg) -> IASMProg {
    let mut new_prog = IASMProg::default();

    let mut imm_values = HashMap::new();

    for (_i, line) in prog.text.iter().enumerate() {
        let iasm::IASMLine::Statement(st) = line else {
            new_prog.text.push_back(line.clone());
            continue;
        };

        let IASMCommandType::LoadImediate = st.command else {
            let mut new_line = st.clone();
            for param in &mut new_line.params {
                let Ok(r) = param.get_register() else {
                    continue;
                };

                let Some(v) = imm_values.get(&r) else {
                    continue;
                };

                *param = IASMParam::Immediate(*v);
            }
            new_prog.text.push_back(iasm::IASMLine::Statement(new_line));
            continue;
        };

        let reg_num = st.params[0].get_register().expect("register");
        let val = st.params[1].get_immediate().expect("register");

        if let Value::Pointer(Pointer::Local(_)) = val {
            new_prog.text.push_back(line.clone());
            continue;
        }

        imm_values.insert(reg_num, val);
        new_prog.text.push_back(iasm::IASMLine::Comment(format!(
            "imediate killed {} for {:?}",
            reg_num, val
        ).into()));
    }

    new_prog
}

fn unused_registers(prog: &IASMProg) -> ControlFlow<(), IASMProg> {
    let mut new_prog = IASMProg::default();

    let use_counts = use_counts(&prog);
    if *use_counts.values().into_iter().min().unwrap() > 0 {
        return ControlFlow::Break(());
    }

    'a: for (_i, line) in prog.text.iter().enumerate() {
        let iasm::IASMLine::Statement(st) = line else {
            new_prog.text.push_back(line.clone());
            continue;
        };

        for mreg in &st.params {
            let IASMParam::Register(reg) = mreg else {
                continue;
            };
            if use_counts[&reg.0] == 0 {
                continue 'a;
            }
        }

        new_prog.text.push_back(line.clone());
    }

    ControlFlow::Continue(new_prog)
}

fn use_counts(prog: &IASMProg) -> HashMap<usize, usize> {
    let mut reg_uses: HashMap<usize, usize> = HashMap::new();

    for line in prog.text.iter() {
        let iasm::IASMLine::Statement(st) = line else {
            continue;
        };

        let first_arg_assign = matches!(
            st.command,
            iasm::IASMCommandType::LoadImediate
                | iasm::IASMCommandType::NegateFloat
                | iasm::IASMCommandType::NegateInt
                | iasm::IASMCommandType::NegateUint
                | iasm::IASMCommandType::Not
                | iasm::IASMCommandType::Load
                | iasm::IASMCommandType::PointerAdd
                | iasm::IASMCommandType::AddInt
                | iasm::IASMCommandType::AddUint
                | iasm::IASMCommandType::AddFloat
                | iasm::IASMCommandType::SubInt
                | iasm::IASMCommandType::SubUint
                | iasm::IASMCommandType::SubFloat
                | iasm::IASMCommandType::PointerSub
                | iasm::IASMCommandType::MulInt
                | iasm::IASMCommandType::MulUint
                | iasm::IASMCommandType::MulFloat
                | iasm::IASMCommandType::DivInt
                | iasm::IASMCommandType::DivUint
                | iasm::IASMCommandType::DivFloat
                | iasm::IASMCommandType::Equal
                | iasm::IASMCommandType::NotEqual
                | iasm::IASMCommandType::GreaterInt
                | iasm::IASMCommandType::GreaterUint
                | iasm::IASMCommandType::GreaterFloat
                | iasm::IASMCommandType::GreaterEqualInt
                | iasm::IASMCommandType::GreaterEqualUint
                | iasm::IASMCommandType::GreaterEqualFloat
                | iasm::IASMCommandType::LessInt
                | iasm::IASMCommandType::LessUint
                | iasm::IASMCommandType::LessFloat
                | iasm::IASMCommandType::LessEqualInt
                | iasm::IASMCommandType::LessEqualUint
                | iasm::IASMCommandType::LessEqualFloat
                | iasm::IASMCommandType::CastIntToFloat
                | iasm::IASMCommandType::CastUintToFloat
                | iasm::IASMCommandType::CastFloatToInt
                | iasm::IASMCommandType::CastUintToInt
                | iasm::IASMCommandType::CastIntToUint
                | iasm::IASMCommandType::CastBoolToInt
                | iasm::IASMCommandType::CastBoolToUint
                | iasm::IASMCommandType::CastBoolToFloat
                | iasm::IASMCommandType::CastIntToBool
                | iasm::IASMCommandType::CastUintToBool
        );

        if first_arg_assign {
            reg_uses.insert(
                st.params[0].get_register().unwrap(),
                *reg_uses
                    .get(&st.params[0].get_register().unwrap())
                    .unwrap_or(&0),
            );
        }

        for mreg in &st.params[if first_arg_assign { 1 } else { 0 }..st.params.len()] {
            let IASMParam::Register(reg) = mreg else {
                continue;
            };
            reg_uses.insert(reg.0, reg_uses.get(&reg.0).unwrap_or(&0) + 1);
        }
    }
    reg_uses
}
