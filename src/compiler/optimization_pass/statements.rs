use tracing::instrument;

use crate::{common::ast::{ASTNode, Expression, Statement}};

use super::OptimizationWalker;



impl OptimizationWalker {

    #[instrument(level = "trace", skip_all)]
    pub fn print(&mut self, e: &mut Expression) {
        let m_e = self.visit_expression(e.clone(), false);
        *e = m_e;
    }
    
    #[instrument(level = "trace", skip_all)]
    pub fn while_statement(&mut self, _c: Expression, _b: Statement) {
        
    }

    #[instrument(level = "trace", skip_all)]
    pub fn for_statement(&mut self, _initializer: Option<ASTNode>, _condition: Option<Expression>, _increment: Option<Expression>, _body: Statement) {
        
    }

    #[instrument(level = "trace", skip_all)]
    pub fn if_statement(&mut self, _c: Expression, _t: Statement, _e: Option<Statement>) {

    }

    
}
