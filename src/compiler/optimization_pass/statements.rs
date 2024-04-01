use tracing::instrument;

use crate::{chunk::Opcode, common::ast::{ASTNode, Expression, Statement}, typing::ValueTypeK};

use super::OptimizationWalker;

use crate::error;

impl OptimizationWalker {

    #[instrument(level = "trace", skip_all)]
    pub fn print(&mut self, e: &mut Expression) {
        let m_e = self.visit_expression(e.clone(), false);
        *e = m_e;
    }
    
    #[instrument(level = "trace", skip_all)]
    pub fn while_statement(&mut self, c: Expression, b: Statement) {
        
    }

    #[instrument(level = "trace", skip_all)]
    pub fn for_statement(&mut self, initializer: Option<ASTNode>, condition: Option<Expression>, increment: Option<Expression>, body: Statement) {
        
    }

    #[instrument(level = "trace", skip_all)]
    pub fn if_statement(&mut self, c: Expression, t: Statement, e: Option<Statement>) {

    }

    
}
