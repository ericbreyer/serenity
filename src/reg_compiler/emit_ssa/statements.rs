use tracing::instrument;

use crate::{
    prelude::*,
    rvalue,
    value::Value,
};

use super::{
    EmitRegWalker,
    iasm::{self, IASMCommandType, IASMLabel, IASMParam},
};

use crate::error;

impl EmitRegWalker {
    #[instrument(level = "trace", skip_all)]
    pub fn print(&mut self, e: Expression, line: usize) -> bool {
        let loc = rvalue!(self, self.visit_expression(e).0, line);
        let t = loc.get_type();
        let _line = 0;
        let reg = iasm::IASMParam::Register(loc);
        match t.as_ref() {
            ValueType::Float => {
                self.write_statement(iasm::IASMCommandType::PrintFloat, vec![reg], line);
            }
            ValueType::Integer => {
                self.write_statement(iasm::IASMCommandType::PrintInt, vec![reg], line)
            }
            ValueType::UInteger => {
                self.write_statement(iasm::IASMCommandType::PrintUint, vec![reg], line)
            }
            ValueType::Bool => {
                self.write_statement(iasm::IASMCommandType::PrintBool, vec![reg], line)
            }
            ValueType::Nil => {
                self.write_statement(iasm::IASMCommandType::PrintNil, vec![reg], line)
            }
            ValueType::Pointer(_, _) => {
                self.write_statement(iasm::IASMCommandType::PrintPointer, vec![reg], line)
            }
            ValueType::Char => {
                self.write_statement(iasm::IASMCommandType::PrintChar, vec![reg], line)
            }
            ValueType::Closure(_, upvals) => self.write_statement(
                iasm::IASMCommandType::PrintClosure,
                vec![reg, IASMParam::Immediate(Value::UInteger(*upvals as u64))],
                line,
            ),
            _ => {
                error!(
                    self,
                    format!("[line {line}] Cannot print this type. {t:?}").as_str()
                );
            }
        }
        false
    }

    #[instrument(level = "trace", skip_all)]
    pub fn while_statement(&mut self, c: Expression, b: Statement, line: usize) -> bool {
        let loop_start = IASMLabel::new_mangled("loop_start");
        let loop_end = IASMLabel::new_mangled("loop_end");
        let continue_to = IASMLabel::new_mangled("continue_to");
        self.write_label(&loop_start);
        self.write_label(&continue_to);
        let reg_loc = rvalue!(self, self.visit_expression(c).0, line);
        let t = reg_loc.get_type();
        if ValueType::Bool != *t.as_ref() {
            error!(self, &format!("[line {line}] Expected boolean expression."));
        }

        self.write_statement(
            IASMCommandType::JumpIfFalse,
            vec![
                IASMParam::Register(reg_loc),
                IASMParam::Label(loop_end.clone()),
            ],
            line,
        );
        let has_return = self.visit_statement(b);

        self.write_statement(
            IASMCommandType::Jump,
            vec![IASMParam::Label(loop_start)],
            line,
        );

        self.write_label(&loop_end);

        has_return
    }

    #[instrument(level = "trace", skip_all)]
    pub fn for_statement(
        &mut self,
        initializer: Option<ASTNode>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Statement,
        line: usize,
    ) -> bool {
        self.begin_scope();

        initializer.map(|i| self.visit(i));

        let loop_start = IASMLabel::new_mangled("loop_start");
        let loop_end = IASMLabel::new_mangled("loop_end");
        let continue_to = IASMLabel::new_mangled("continue_to");

        self.write_label(&loop_start);

        condition
            .map(|c| rvalue!(self, self.visit_expression(c).0, line))
            .map(|n| {
                let t = n.get_type();
                if ValueType::Bool != *t.as_ref() {
                    error!(self, &format!("[line {line}] Expected boolean expression."));
                }
                n
            })
            .and_then(|loc| {
                self.write_statement(
                    IASMCommandType::JumpIfFalse,
                    vec![IASMParam::Register(loc), IASMParam::Label(loop_end.clone())],
                    line,
                );
                Some(loc)
            });

        let has_return = self.visit_statement(body);

        self.write_label(&continue_to);
        increment.map(|i| rvalue!(self, self.visit_expression(i).0, line));

        self.write_statement(
            IASMCommandType::Jump,
            vec![IASMParam::Label(loop_start)],
            line,
        );
        self.write_label(&loop_end);
        self.end_scope();
        has_return
    }

    #[instrument(level = "trace", skip_all)]
    pub fn if_statement(
        &mut self,
        c: Expression,
        t: Statement,
        e: Option<Statement>,
        line: usize,
    ) -> bool {
        let loc = rvalue!(self, self.visit_expression(c).0, line);
        let tp = loc.get_type();
        if ValueType::Bool != *tp.as_ref() {
            error!(self, &format!("[line {line}] Expected boolean expression."));
        }
        let else_label = IASMLabel::new_mangled("else");
        let than_label = IASMLabel::new_mangled("than");

        self.write_statement(
            IASMCommandType::JumpIfFalse,
            vec![
                IASMParam::Register(loc),
                IASMParam::Label(else_label.clone()),
            ],
            line,
        );

        let has_return = self.visit_statement(t);
        self.write_statement(
            IASMCommandType::Jump,
            vec![IASMParam::Label(than_label.clone())],
            line,
        );
        self.write_label(&else_label);
        let has_return_else = e.map(|e| self.visit_statement(e)).unwrap_or(false);
        self.write_label(&than_label);

        has_return || has_return_else
    }
}
