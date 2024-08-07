use std::collections::HashMap;

use tracing::instrument;

use crate::{chunk::Opcode, common::declared_ast::{Expression, Local, NDASTNode, Statement}, typing::ValueType};

use super::EmitWalker;

use crate::error;

impl EmitWalker {

    #[instrument(level = "trace", skip_all)]
    pub fn print(&mut self, e: Expression, _line : usize) {
        let t = self.visit_expression(e, false);
        let line = 0;
        match t.as_ref() {
            ValueType::Float => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintFloat.into(), line);
            }
            ValueType::Integer => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintInt.into(), line);
            }
            ValueType::UInteger => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintUint.into(), line);
            }
            ValueType::Bool => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintBool.into(), line);
            }
            
            ValueType::Nil => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintNil.into(), line);
            }
            ValueType::Pointer(_, _) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintPointer.into(), line);
            }
            ValueType::Array(_, _) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintPointer.into(), line);
            }
            ValueType::Closure(_, _) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintPointer.into(), line);
            }
            ValueType::Char => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::PrintChar.into(), line);
            }
            _ => {
                error!(self, format!("[line {line}] Cannot print this type. {t:?}").as_str());
            }
        }
    }
    
    #[instrument(level = "trace", skip_all)]
    pub fn while_statement(&mut self, c: Expression, b: Statement, _line : usize) {
        let loop_start = self.function_compiler.func.chunk.code.len();
        let line = 0;
        let t = self.visit_expression(c, false);
        
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse.into());
        
        self.function_compiler.func.chunk.write_pop(t, line);
        self.visit_statement(b);
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.function_compiler.func.chunk.write_pop(t, line);
        self.fill_break_continue(loop_start);
    }

    #[instrument(level = "trace", skip_all)]
    pub fn for_statement(&mut self, locals: HashMap<usize, Local>, initializer: Option<NDASTNode>, condition: Option<Expression>, increment: Option<Expression>, body: Statement, _line :usize) {
        self.begin_scope(locals);
        
        let line = 0;

        let mut exit_jump = None;
        let mut body_jump = None;
        
        if let Some(initializer) = initializer {
            self.visit(initializer);
        }

        let loop_start = self.function_compiler.func.chunk.code.len();

        if let Some(condition) = condition {
            let t = self.visit_expression(condition, false);
            exit_jump = Some(self.emit_jump(Opcode::JumpIfFalse.into()));
            self.function_compiler.func.chunk.write_pop(t, line);
            body_jump = Some(self.emit_jump(Opcode::Jump.into()));
        }
        
        let mut increment_start = 0;
        
        if let Some(increment) = increment {
                increment_start = self.function_compiler.func.chunk.code.len();

            let t = self.visit_expression(increment, false);
            self.function_compiler.func.chunk.write_pop(t, line);

            // loop
                self.emit_loop(loop_start);
        }

        if let Some(b) = body_jump {
                self.patch_jump(b);
        }

        self.visit_statement(body);

        self.emit_loop(increment_start);
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.function_compiler
                .func
                .chunk
                .write_pop(ValueType::Bool.intern(), line);
        }
        self.fill_break_continue(increment_start);
        self.end_scope();
        
    }

    fn fill_break_continue(&mut self, loop_start: usize) {
        let mut i = self.function_compiler.func.chunk.code.len() - 3;
        while i > loop_start {
            let op = self.function_compiler.func.chunk.code[i].0;
            if op == Opcode::Break.into()
                && self.function_compiler.func.chunk.code[i + 1].0 == 255
                && self.function_compiler.func.chunk.code[i + 2].0 == 255
            {
                self.function_compiler.func.chunk.code[i].0 = Opcode::Jump.into();
                self.patch_jump(i + 1);
            } else if op == Opcode::Continue.into()
                && self.function_compiler.func.chunk.code[i + 1].0 == 255
                && self.function_compiler.func.chunk.code[i + 2].0 == 255
            {
                let offset = i - loop_start + 3;
                self.function_compiler.func.chunk.code[i].0 = Opcode::Loop.into();
                self.function_compiler.func.chunk.code[i + 1].0 = ((offset >> 8) & 0xff) as u8;
                self.function_compiler.func.chunk.code[i + 2].0 = (offset & 0xff) as u8;
            }
            i -= 1;
        }
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let line = 0;
        self.function_compiler.func.chunk.write(Opcode::Loop.into(), line);
        let offset = self.function_compiler.func.chunk.code.len() - loop_start + 2;
        if offset > std::u16::MAX as usize {
            error!(self, "Loop body too large.");
        }

        self.function_compiler
            .func
            .chunk
            .write(((offset >> 8) & 0xff) as u8, line);
        self.function_compiler.func.chunk.write((offset & 0xff) as u8, line);
    }

    #[instrument(level = "trace", skip_all)]
    pub fn if_statement(&mut self, c: Expression, t: Statement, e: Option<Statement>, _line : usize) {
        let line = 0;
        let tp = self.visit_expression(c, false);
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse.into());
        self.function_compiler.func.chunk.write_pop(tp, line);
        self.visit_statement(t);
        let else_jump = self.emit_jump(Opcode::Jump.into());
        self.patch_jump(exit_jump);
        self.function_compiler.func.chunk.write_pop(tp, line);
        if let Some(e) = e {
            self.visit_statement(e);
        }
        self.patch_jump(else_jump);
    }

    
}
