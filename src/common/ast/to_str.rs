use super::{ASTNode, ArrayDeclaration, Declaration, Expression, FunctionDeclaration, FunctionExpression, Statement, StructDeclaration, Token, VarDeclaration};

pub trait ToStr {
    fn to_string(&self, depth: usize) -> String;
}

impl ToStr for ASTNode {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        match self {
            ASTNode::Err => {
                s.push_str("Err");
            },
            ASTNode::Module(v) => {
                for n in v {
                    s.push_str(&n.to_string(depth));
                };
            }
            ASTNode::Statement(st, line) => {
                s.push_str(&format!("Line {}: {}", line, st.to_string(depth)));
            }
            ASTNode::Expression(e, line) => {
                s.push_str(&format!("Line {}: {}", line, e.to_string(depth)));
            }
            ASTNode::Declaration(d, line) => {
                s.push_str(&format!("Line {}: {}", line, d.to_string(depth)));
            }
        }
        s
    }
}

impl ToStr for Expression {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        match self {
            Expression::Literal(v) => {
                s.push_str(&format!("Literal: {v:?}"));
            }
            Expression::StringLiteral(v) => {
                s.push_str(&format!("StringLiteral: {v:?}"));
            }
            Expression::Unary(t, e) => {
                s.push_str(&format!("Unary: {:?} {}", t, e.to_string(depth + 1)));
            }
            Expression::Deref(e) => {
                s.push_str(&format!("Deref: {}", e.to_string(depth + 1)));
            }
            Expression::Ref(t) => {
                s.push_str(&format!("Ref: {}", t.to_string(depth + 1)));
            }
            Expression::Index(l, r) => {
                s.push_str("Index:");
                s.push_str(&l.to_string(depth + 1));
                s.push_str(&r.to_string(depth + 1));
            }
            Expression::Binary(l, t, r) => {
                s.push_str(&format!("Binary: {t:?} "));
                s.push_str(&l.to_string(depth + 1));
                s.push_str(&r.to_string(depth + 1));
            }
            Expression::Ternary(c, t, e) => {
                s.push_str("Ternary:");
                s.push_str(&c.to_string(depth + 1));
                s.push_str(&t.to_string(depth + 1));
                s.push_str(&e.to_string(depth + 1));
            }
            Expression::Variable(t) => {
                s.push_str(&format!("Variable: {t:?}"));
            }
            Expression::Assign(t, e) => {
                s.push_str("Assign:");
                s.push_str(&t.to_string(depth + 1));
                s.push_str(&e.to_string(depth + 1));
            }
            Expression::Logical(l, t, r) => {
                s.push_str(&format!("Logical: {t:?} "));
                s.push_str(&l.to_string(depth + 1));
                s.push_str(&r.to_string(depth + 1));
            }
            Expression::Call(f, a) => {
                s.push_str("Call:");
                s.push_str(&f.to_string(depth + 1));
                for n in a {
                    s.push_str(&n.to_string(depth + 1));
                }
            }
            Expression::Dot(e, t) => {
                s.push_str(&format!("Get: {t:?}"));
                s.push_str(&e.to_string(depth + 1));
            }
            Expression::Function(f) => {
                s.push_str(&format!("Function: {}", f.to_string(depth + 1)));
            }
            Expression::Cast(e, t, _) => {
                s.push_str(&format!("Cast: {t:?}"));
                s.push_str(&e.to_string(depth + 1));
            }
            Expression::ArrayLiteral(v) => {
                s.push_str("ArrayLiteral: ");
                for n in v {
                    s.push_str(&n.to_string(depth + 1));
                }
            }
            Expression::StructInitializer(c, v) => {
                s.push_str(&format!("StructInitializer: {c:?}"));
                for (t, e) in v {
                    s.push_str(t);
                    s.push_str(&e.to_string(depth + 1));
                }
            }
            Expression::Empty => {
                s.push_str("Empty");
            }
        }
        s
    }
}

impl ToStr for Statement {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        match self {
            Statement::Print(e) => {
                s.push_str(&format!("Print: {}", e.to_string(depth + 1)));
            }
            Statement::Block(v) => {
                s.push_str("Block: ");
                for n in v {
                    s.push_str(&n.to_string(depth + 1));
                }
            }
            Statement::If(c, t, e) => {
                s.push_str("If:");
                s.push_str(&c.to_string(depth + 1));
                s.push_str(&t.to_string(depth + 1));
                if let Some(e) = e {
                    s.push_str(&e.to_string(depth + 1));
                }
            }
            Statement::While(c, b) => {
                s.push_str("While:");
                s.push_str(&c.to_string(depth + 1));
                s.push_str(&b.to_string(depth + 1));
            }
            Statement::For(i, c, u, b) => {
                s.push_str("For:");
                if let Some(i) = i {
                    s.push_str(&i.to_string(depth + 1));
                }
                if let Some(c) = c {
                    s.push_str(&c.to_string(depth + 1));
                }
                if let Some(u) = u {
                    s.push_str(&u.to_string(depth + 1));
                }
                s.push_str(&b.to_string(depth + 1));
            }
            Statement::Break => {
                s.push_str("Break");
            }
            Statement::Continue => {
                s.push_str("Continue");
            }
            Statement::Return(e) => {
                s.push_str("Return:");
                if let Some(e) = e {
                    s.push_str(&e.to_string(depth + 1));
                }
            }
            Statement::Expression(e) => {
                s.push_str(&format!("Expression: {}", e.to_string(depth + 1)));
            }
        }
        s
    }
}

impl ToStr for FunctionExpression {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str(&format!("return_type: {:?}", self.return_type));
        s.push('\n');
            for _ in 0..=depth {
                s.push_str("  ");
            }
        s.push_str("Params:");
        for (str, t) in &self.params {
            s.push('\n');
            for _ in 0..depth+2 {
                s.push_str("  ");
            }
            s.push_str(&format!("{str}: {t:?}"));
        }
        for n in &self.body {
            s.push_str(&n.to_string(depth + 1));
        }
        s
    }
}

impl ToStr for Declaration {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str("Declaration: ");
        match self {
            Declaration::Var(v) => {
                s.push_str(&format!("Var: {}", v.to_string(depth + 1)));
            }
            Declaration::Function(f) => {
                s.push_str(&format!("Fun: {}", f.to_string(depth + 1)));
            }
            Declaration::Array(i) => {
                s.push_str("Array:");
                s.push_str(&i.to_string(depth + 1));
            }
            Declaration::Struct(c) => {
                s.push_str(&format!("Struct: {}", c.to_string(depth + 1)));
            }
        }
        s
    }
}

impl ToStr for VarDeclaration {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str(&format!("VarDeclaration: {:?}", self.name));
        if let Some(e) = &self.initializer {
            s.push_str(&e.to_string(depth + 1));
        }
        if let Some(t) = &self.tipe {
            s.push('\n');
            for _ in 0..=depth {
                s.push_str("  ");
            }
            s.push_str(&format!("Type: {t:?}"));
        }
        s
    }
}

impl ToStr for ArrayDeclaration {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str(&format!("ArrayDeclaration: {:?}", self.name));

        if let Some(t) = &self.elem_tipe {
            s.push_str(&format!("  Elem Type: {t:?}"));
        }

        let e = &self.elements;
                for n in e {
                    s.push_str(&n.to_string(depth + 1));
                }
        
        s
    }
}

impl ToStr for StructDeclaration {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str(&format!("StructDeclaration: {:?}", self.s));
        s
    }
}

impl ToStr for FunctionDeclaration {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str(&format!("FunctionDeclaration: {:?}", self.name));
        s.push_str(&self.body.to_string(depth + 1));
        s
    }
}

impl ToStr for Token {
    fn to_string(&self, depth: usize) -> String {
        let mut s = String::new();
        s.push('\n');
        for _ in 0..depth {
            s.push_str("  ");
        }
        s.push_str(&format!("{self:?}"));
        s
    }
    
}