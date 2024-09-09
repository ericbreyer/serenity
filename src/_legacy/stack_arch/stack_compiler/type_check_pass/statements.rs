use tracing::instrument;

use crate::{
  common::{ parsed_ast::{ ASTNode, Expression, Statement }, declared_ast },
  typing::ValueType,
};

use super::TypeCheckWalker;

use crate::error;

impl TypeCheckWalker {
  #[instrument(level = "trace", skip_all)]
  pub fn print(&mut self, e: Expression, line: usize) -> (declared_ast::Statement, bool) {
    let (nde, t) = self.visit_expression(e, false);
    let _line = 0;
    match t.as_ref() {
      | ValueType::Float
      | ValueType::Integer
      | ValueType::UInteger
      | ValueType::Bool
      | ValueType::Nil
      | ValueType::Pointer(_, _)
      | ValueType::Array(_, _)
      | ValueType::Closure(_, _)
      | ValueType::Char => {}
      _ => {
        error!(self, format!("[line {line}] Cannot print this type. {t:?}").as_str());
      }
    }
    (declared_ast::Statement::Print(Box::new(nde), line), false)
  }

  #[instrument(level = "trace", skip_all)]
  pub fn while_statement(
    &mut self,
    c: Expression,
    b: Statement,
    line: usize
  ) -> (declared_ast::Statement, bool) {
    let _line = 0;
    let (ndc, t) = self.visit_expression(c, false);
    if ValueType::Bool != *t.as_ref() {
      error!(self, &format!("[line {line}] Expected boolean expression."));
    }

    let (nds, r) = self.visit_statement(b);
    (declared_ast::Statement::While(Box::new(ndc), Box::new(nds), line), r)
  }

  #[instrument(level = "trace", skip_all)]
  pub fn for_statement(
    &mut self,
    initializer: Option<ASTNode>,
    condition: Option<Expression>,
    increment: Option<Expression>,
    body: Statement,
    line: usize
  ) -> (declared_ast::Statement, bool) {
    self.begin_scope();

    let _line = 0;

    let ndinit = initializer.map(|i| self.visit(i).0);

    let ndc = condition
      .map(|c| self.visit_expression(c, false))
      .map(|(n, t)| {
        if ValueType::Bool != *t.as_ref() {
          error!(self, &format!("[line {line}] Expected boolean expression."));
        }
        n
      });

    let ndinc = increment.map(|i| self.visit_expression(i, false).0);

    let (nds, r) = self.visit_statement(body);

    

    let r = (
      declared_ast::Statement::For(
        self.function_compiler.locals.iter().map(|(i, l)| (*i, declared_ast::Local{ name: l.name.clone(), depth: l.depth, local_type:  l.local_type })).collect(),
        ndinit.map(Box::new),
        ndc.map(Box::new),
        ndinc.map(Box::new),
        Box::new(nds),
        line
      ),
      r,
    );
    self.end_scope();
    r
  }

  #[instrument(level = "trace", skip_all)]
  pub fn if_statement(
    &mut self,
    c: Expression,
    t: Statement,
    e: Option<Statement>,
    line: usize
  ) -> (declared_ast::Statement, bool) {
    let _line = 0;
    let (nde, tp) = self.visit_expression(c, false);
    if ValueType::Bool != *tp.as_ref() {
      error!(self, &format!("[line {line}] Expected boolean expression."));
    }
    let (ndt, mut both_return) = self.visit_statement(t);
    let mut nda = None;
    if let Some(e) = e {
      let (snda, er) = self.visit_statement(e);
      nda = Some(Box::new(snda));
      both_return = both_return && er;
    } else {
      both_return = false;
    }
    (declared_ast::Statement::If(Box::new(nde), Box::new(ndt), nda, line), both_return)
  }
}
