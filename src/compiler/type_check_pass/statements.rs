use tracing::instrument;

use crate::{common::ast::{ASTNode, Expression, Statement}, typing::ValueType};

use super::TypeCheckWalker;

use crate::error;

impl TypeCheckWalker {

    #[instrument(level = "trace", skip_all)]
    pub fn print(&mut self, e: Expression) -> bool {
        let t = self.visit_expression(e, false);
        let _line = 0;
        match t.as_ref() {
            ValueType::Float |
            ValueType::Integer |
            ValueType::Bool |
            ValueType::String |
            ValueType::Nil |
            ValueType::Pointer(_, _) |
            ValueType::Array(_, _) |
            ValueType::Closure(_) |
            ValueType::Char => {}
            _ => {
                error!(self, format!("Cannot print this type. {:?}", t).as_str());
            }
        };
        false
    }
    
    #[instrument(level = "trace", skip_all)]
    pub fn while_statement(&mut self, c: Expression, b: Statement) -> bool {
        let _line = 0;
        let t = self.visit_expression(c, false);
        if ValueType::Bool != *t.as_ref() {
            error!(self, "Expected boolean expression.");
        }
        
        self.visit_statement(b)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn for_statement(&mut self, initializer: Option<ASTNode>, condition: Option<Expression>, increment: Option<Expression>, body: Statement) -> bool {
        self.begin_scope();
        
        let _line = 0;
        
        if let Some(initializer) = initializer {
            self.visit(initializer);
        }

        if let Some(condition) = condition {
            let t = self.visit_expression(condition, false);
            if ValueType::Bool != *t.as_ref() {
                error!(self, "Expected boolean expression.");
            }
        }
                
        if let Some(increment) = increment {

            self.visit_expression(increment, false);

        }

        let r = self.visit_statement(body);

        self.end_scope();

        r
        
    }

    #[instrument(level = "trace", skip_all)]
    pub fn if_statement(&mut self, c: Expression, t: Statement, e: Option<Statement>) -> bool {
        let _line = 0;
        let tp = self.visit_expression(c, false);
        if ValueType::Bool != *tp.as_ref() {
            error!(self, "Expected boolean expression.");
        }
        let mut both_return = self.visit_statement(t);
        if let Some(e) = e {
            both_return = both_return && self.visit_statement(e);
        }
        both_return
    }

    
}
