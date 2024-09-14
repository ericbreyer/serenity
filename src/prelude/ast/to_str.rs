use std::cell::RefCell;

use crate::prelude::shared_strings::SharedString;

use super::{
    ASTNode, Acceptor, Declaration, DeclarationVisitor, Expression, ExpressionVisitor,
    FunctionExpression, NodeVisitor, Prototype, Statement, StatementVisitor,
};

const PIPE_END_CHAR: &str = "╰";
const PIPE_CHAR: &str = "│";
const _MEMBER_CHAR: &str = "─";
const T_CHAR: &str = "├";

const INDENT: &str = "     ";
const INDENT_SUFFIX: &str = "    ";
const INDENT_PIPE: &str = "│    ";
const INDENT_PIPE_END: &str = "╰    ";
const INDENT_MEMBER: &str = "─── ";

pub(super) struct ToStrVisitor<'a> {
    depth_has_scope_open: &'a RefCell<[bool; 100]>,
    depth: usize,
    close_scope: Option<usize>,
    indent: String,
    indent_member: String,
}

impl<'a> ToStrVisitor<'a> {
    pub fn new(
        depth_has_scope_open: &'a RefCell<[bool; 100]>,
        depth: usize,
        close_scope: Option<usize>,
    ) -> Self {
        Self {
            depth_has_scope_open,
            depth,
            close_scope,
            indent: String::new(),
            indent_member: String::new(),
        }
    }

    pub fn with_indents(&'a self, indent: String, indent_member: String) -> Self {
        Self {
            depth_has_scope_open: self.depth_has_scope_open,
            depth: self.depth,
            close_scope: self.close_scope,
            indent,
            indent_member,
        }
    }
    fn visit(&'a self, node: &impl Acceptor<String, ToStrVisitor<'a>>) -> String {
        let depth = self.depth;
        let close_scope = self.close_scope;

        let mut s = String::new();
        let mut indent = String::new();
        s.push('\n');
        for i in 0..depth {
            if self.depth_has_scope_open.borrow()[i] {
                if let Some(c) = close_scope {
                    if c == i {
                        indent.push_str(INDENT_PIPE_END);
                    } else {
                        indent.push_str(INDENT_PIPE);
                    }
                } else {
                    indent.push_str(INDENT_PIPE);
                }
            } else {
                indent.push_str(INDENT);
            }
        }
        let indent_member = indent
            .strip_suffix(INDENT_SUFFIX)
            .unwrap_or(&indent)
            .trim_end_matches(PIPE_CHAR)
            .to_owned()
            + if close_scope.is_none() && depth > 0 {
                T_CHAR
            } else {
                ""
            }
            + if depth > 0 { INDENT_MEMBER } else { "" };
        if let Some(c) = close_scope { self.depth_has_scope_open.borrow_mut()[c] = false; }
        self.depth_has_scope_open.borrow_mut()[depth] = true;

        s.push_str(
            node.accept(&self.with_indents(indent, indent_member))
                .as_str(),
        );
        s
    }
}
impl<'a> NodeVisitor<String> for ToStrVisitor<'a> {
    fn visit_statement(&self, statement: &Statement) -> String {
        self.visit(statement)
    }

    fn visit_expression(&self, expression: &Expression) -> String {
        self.visit(expression)
    }

    fn visit_declaration(&self, declaration: &Declaration) -> String {
        self.visit(declaration)
    }
}

impl<'a> ExpressionVisitor<String> for ToStrVisitor<'a> {
    fn visit_literal_expression(&self, expression: &super::LiteralExpression) -> String {
        format!(
            "[line {:>4}] {}Literal: {:?}",
            expression.line_no, self.indent_member, expression.value
        )
    }

    fn visit_string_literal_expression(
        &self,
        expression: &super::StringLiteralExpression,
    ) -> String {
        format!(
            "[line {:>4}] {}StringLiteral: {:?}",
            expression.line_no, self.indent_member, expression.value
        )
    }

    fn visit_unary_expression(&self, expression: &super::UnaryExpression) -> String {
        format!(
            "[line {:>4}] {}Unary: {:?} {}",
            expression.line_no,
            self.indent_member,
            expression.operator,
            expression.operand.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                Some(self.depth)
            ))
        )
    }

    fn visit_deref_expression(&self, expression: &super::DerefExpression) -> String {
        format!(
            "[line {:>4}] {}Deref: {}",
            expression.line_no,
            self.indent_member,
            expression.operand.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                Some(self.depth)
            ))
        )
    }

    fn visit_ref_expression(&self, expression: &super::RefExpression) -> String {
        format!(
            "[line {:>4}] {}Ref: {}",
            expression.line_no,
            self.indent_member,
            expression.operand.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                Some(self.depth)
            ))
        )
    }

    fn visit_index_expression(&self, expression: &super::IndexExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Index:",
            expression.line_no, self.indent_member
        ));
        s.push_str(&expression.array.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&expression.index.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_binary_expression(&self, expression: &super::BinaryExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Binary: {:?}",
            expression.line_no, self.indent_member, expression.operator
        ));
        s.push_str(&expression.left.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&expression.right.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_ternary_expression(&self, expression: &super::TernaryExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Ternary:",
            expression.line_no, self.indent_member
        ));
        s.push_str(&expression.condition.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&expression.then_branch.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&expression.else_branch.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_variable_expression(&self, expression: &super::VariableExpression) -> String {
        self.depth_has_scope_open.borrow_mut()[self.depth] = false;
        format!(
            "[line {:>4}] {}Variable: {:?}",
            expression.line_no, self.indent_member, expression.token
        )
    }

    fn visit_assign_expression(&self, expression: &super::AssignExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Assign:",
            expression.line_no, self.indent_member
        ));
        s.push_str(&expression.variable.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&expression.value.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_logical_expression(&self, expression: &super::LogicalExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Logical: {:?} ",
            expression.line_no, self.indent_member, expression.operator
        ));
        s.push_str(&expression.left.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&expression.right.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_call_expression(&self, expression: &super::CallExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Call:",
            expression.line_no, self.indent_member
        ));
        s.push_str(&expression.callee.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            if expression.arguments.is_empty() {
                Some(self.depth)
            } else {
                None
            },
        )));
        for (i, n) in expression.arguments.iter().enumerate() {
            s.push_str(&n.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                if i == expression.arguments.len() - 1 {
                    Some(self.depth)
                } else {
                    None
                },
            )));
        }
        s
    }

    fn visit_dot_expression(&self, expression: &super::DotExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Get: {:?}",
            expression.line_no, self.indent_member, expression.field
        ));
        s.push_str(&expression.object.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_function_expression(&self, expression: &FunctionExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Function: {}",
            expression.line_no, self.indent_member, expression.prototype.name
        ));
        s.push_str(&expression.prototype.stringify(
            self.depth_has_scope_open,
            self.depth + 1,
            expression.line_no,
            Some(self.depth),
            expression.body.clone(),
        ));
        s
    }

    fn visit_cast_expression(&self, expression: &super::CastExpression) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Cast: {:?}",
            expression.line_no,
            self.indent_member,
            expression.target_type
        ));
        s.push_str(&expression.expression.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_struct_initializer_expression(
        &self,
        expression: &super::StructInitializerExpression,
    ) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}StructInitializer: {:?}",
            expression.line_no, self.indent_member, expression.struct_type
        ));
        let indent = self
            .indent
            .strip_suffix("    ")
            .unwrap()
            .to_owned()
            .trim_end_matches(PIPE_END_CHAR)
            .to_owned()
            + INDENT;

        for (i, (t, e)) in expression.fields.iter().enumerate() {
            if i as i64 > (expression.fields.len() as i64 - 2) {
                self.depth_has_scope_open.borrow_mut()[self.depth] = false;
            }

            s.push_str(
                format!(
                    "\n[line {:>4}] {indent}{}{INDENT_MEMBER} {}: ",
                    expression.line_no,
                    if i == expression.fields.len() - 1 {
                        PIPE_END_CHAR
                    } else {
                        T_CHAR
                    },
                    t
                )
                .as_str(),
            );

            self.depth_has_scope_open.borrow_mut()[self.depth + 1] = true;
            s.push_str(&e.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 2,
                Some(&self.depth + 1),
            )));
        }
        s
    }
    
    fn visit_sizeof_expression(&self, expression: &super::SizeofExpression) -> String {
        format!(
            "[line {:>4}] {}Sizeof: {:?}",
            expression.line_no, self.indent_member, expression.tipe
        )
    }
}

impl<'a> StatementVisitor<String> for ToStrVisitor<'a> {
    fn visit_block_statement(&self, statement: &super::BlockStatement) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Block: ",
            statement.line_no, self.indent_member
        ));
        for (i, n) in statement.statements.iter().enumerate() {
            s.push_str(&n.accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                if i == statement.statements.len() - 1 {
                    Some(self.depth)
                } else {
                    None
                },
            )));
        }
        s
    }

    fn visit_if_statement(&self, statement: &super::IfStatement) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}If: ",
            statement.line_no, self.indent_member
        ));
        s.push_str(&statement.condition.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&statement.then_branch.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            if statement.else_branch.is_none() {
                Some(self.depth)
            } else {
                None
            },
        )));
        if let Some(e) = &statement.else_branch {
            s.push_str(&e.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                Some(self.depth),
            )));
        }
        s
    }

    fn visit_while_statement(&self, statement: &super::WhileStatement) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}While: ",
            statement.line_no, self.indent_member
        ));
        s.push_str(&statement.condition.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            None,
        )));
        s.push_str(&statement.body.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_for_statement(&self, statement: &super::ForStatement) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}For: ",
            statement.line_no, self.indent_member
        ));
        if let Some(i) = &statement.init {
            s.push_str(&i.accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                None,
            )));
        }
        if let Some(c) = &statement.condition {
            s.push_str(&c.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                None,
            )));
        }
        if let Some(u) = &statement.increment {
            s.push_str(&u.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                None,
            )));
        }
        s.push_str(&statement.body.as_node().accept(&ToStrVisitor::new(
            self.depth_has_scope_open,
            self.depth + 1,
            Some(self.depth),
        )));
        s
    }

    fn visit_break_statement(&self, statement: &super::BreakStatement) -> String {
        format!(
            "[line {:>4}] {}Break",
            statement.line_no, self.indent_member
        )
    }

    fn visit_continue_statement(&self, statement: &super::ContinueStatement) -> String {
        format!(
            "[line {:>4}] {}Continue",
            statement.line_no, self.indent_member
        )
    }

    fn visit_return_statement(&self, statement: &super::ReturnStatement) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Return:",
            statement.line_no, self.indent_member
        ));
        if let Some(e) = &statement.value {
            s.push_str(&e.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                Some(self.depth),
            )));
        } else {
            self.depth_has_scope_open.borrow_mut()[self.depth] = false;
        }
        s
    }

    fn visit_expression_statement(&self, statement: &super::ExpressionStatement) -> String {
        format!(
            "[line {:>4}] {}Expression: {}",
            statement.line_no,
            self.indent_member,
            statement.expr.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 1,
                Some(self.depth),
            ))
        )
    }
}

impl<'a> DeclarationVisitor<String> for ToStrVisitor<'a> {
    fn visit_var_declaration(&self, declaration: &super::VarDeclaration) -> String {
        let mindent = self.indent.strip_suffix(INDENT_PIPE_END);
        let indent = if let Some(indent) = mindent {
            indent.to_owned() + INDENT
        } else {
            self.indent.to_owned()
        };
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Var Declaration: {}",
            declaration.line_no, self.indent_member, &declaration.name
        ));
        if let Some(e) = &declaration.initializer {
            s.push_str(&format!(
                "\n[line {:>4}] {}{PIPE_END_CHAR}{INDENT_MEMBER} Initializer",
                declaration.line_no, indent
            ));
            self.depth_has_scope_open.borrow_mut()[self.depth + 1] = true;
            self.depth_has_scope_open.borrow_mut()[self.depth] = false;
            s.push_str(&e.as_node().accept(&ToStrVisitor::new(
                self.depth_has_scope_open,
                self.depth + 2,
                Some(&self.depth + 1),
            )));
        } else {
            self.depth_has_scope_open.borrow_mut()[self.depth] = false;
        }
        s
    }

    fn visit_function_declaration(&self, declaration: &super::FunctionDeclaration) -> String {
        let mut s = String::new();
        s.push_str(&format!(
            "[line {:>4}] {}Function Declaration: {}",
            declaration.line_no, self.indent_member, &declaration.prototype.name
        ));
        s.push_str(&declaration.prototype.stringify(
            self.depth_has_scope_open,
            self.depth + 1,
            declaration.line_no,
            Some(self.depth),
            declaration.body.clone(),
        ));
        s
    }
}

impl Prototype {
    fn stringify(
        &self,
        depth_has_scope_open: &RefCell<[bool; 100]>,
        depth: usize,
        line: usize,
        close_scope: Option<usize>,
        body: Option<Vec<ASTNode>>,
    ) -> SharedString {
        let mut s = String::new();
        s.push('\n');
        let mut indent = String::new();
        for i in 0..depth {
            if depth_has_scope_open.borrow()[i] {
                if let Some(c) = close_scope {
                    if c == i {
                        indent.push_str(INDENT_PIPE_END);
                    } else {
                        indent.push_str(INDENT_PIPE);
                    }
                } else {
                    indent.push_str(INDENT_PIPE);
                }
            } else {
                indent.push_str(INDENT);
            }
        }
        let indent_member = indent
            .strip_suffix(INDENT_SUFFIX)
            .unwrap()
            .trim_end_matches(PIPE_CHAR)
            .to_owned()
            + if close_scope.is_none() { T_CHAR } else { "" }
            + INDENT_MEMBER;
        indent = indent
            .strip_suffix(INDENT_SUFFIX)
            .unwrap()
            .to_owned()
            .trim_end_matches(PIPE_END_CHAR)
            .to_owned()
            + INDENT;
        if let Some(c) = close_scope { depth_has_scope_open.borrow_mut()[c] = false; }

        s.push_str(&format!(
            "[line {line:>4}] {indent_member}Function Expression:"
        ));
        s.push_str(&format!(
            "\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Name: {name}",
            name = self.name
        ));
        s.push_str(&format!(
            "\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Return Type: {:?}",
            self.return_type
        ));

        if !self.captures.is_empty() {
            s.push_str(&format!(
                "\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Captures:"
            ));
            for (i, str) in self.captures.iter().enumerate() {
                s.push('\n');
                s.push_str(&format!(
                    "[line {line:>4}] {indent}{INDENT_PIPE}{}{INDENT_MEMBER}{str}",
                    if i == self.captures.len() - 1 {
                        PIPE_END_CHAR
                    } else {
                        T_CHAR
                    }
                ));
            }
        }

        if !self.params.is_empty() {
            s.push_str(&format!(
                "\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Params:"
            ));
            for (i, (str, t, _mut)) in self.params.iter().enumerate() {
                s.push('\n');
                s.push_str(&format!(
                    "[line {line:>4}] {indent}{INDENT_PIPE}{}{INDENT_MEMBER}{str}: {t:?}",
                    if i == self.params.len() - 1 {
                        PIPE_END_CHAR
                    } else {
                        T_CHAR
                    }
                ));
            }
        }

        depth_has_scope_open.borrow_mut()[depth + 1] = true;
        if let Some(body) = body {
            s.push_str(&format!(
                "\n[line {line:>4}] {indent}{PIPE_END_CHAR}{INDENT_MEMBER}Body: "
            ));
            for (i, n) in body.iter().enumerate() {
                s.push_str(&n.accept(&ToStrVisitor::new(
                    depth_has_scope_open,
                    depth + 2,
                    if i == body.len() - 1 {
                        Some(depth + 1)
                    } else {
                        None
                    },
                )));
            }
        }
        s.into()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::lexer::{Token, TokenType};
    use crate::prelude::*;
    use insta::assert_snapshot;

    use test_case::test_case;

    #[test_case(Expression::Literal(LiteralExpression {
        line_no: 1,
        value: Value::Integer(1),
    }), "literal"; "test_literal")]
    #[test_case(Expression::StringLiteral(StringLiteralExpression {
        line_no: 1,
        value: "test".into(),
    }), "string_literal"; "test_string_literal")]
    #[test_case(Expression::Unary(UnaryExpression {
        line_no: 1,
        operator: TokenType::Minus,
        operand: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "unary"; "test_unary")]
    #[test_case(Expression::Deref(DerefExpression {
        line_no: 1,
        operand: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "deref"; "test_deref")]
    #[test_case(Expression::Ref(RefExpression {
        line_no: 1,
        operand: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "ref"; "test_ref")]
    #[test_case(Expression::Index(IndexExpression {
        line_no: 1,
        array: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
        index: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "index"; "test_index")]
    #[test_case(Expression::Binary(BinaryExpression {
        line_no: 1,
        operator: TokenType::Plus,
        left: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
        right: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "binary"; "test_binary")]
    #[test_case(Expression::Ternary(TernaryExpression {
        line_no: 1,
        condition: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
        then_branch: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
        else_branch: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "ternary"; "test_ternary")]
    #[test_case(Expression::Variable(VariableExpression {
        line_no: 1,
        token: Token {
            lexeme: "test".into(),
            token_type: TokenType::Identifier,
            line: 1,
        },
    }), "variable"; "test_variable")]
    #[test_case(Expression::Assign(AssignExpression {
        line_no: 1,
        variable: Box::new(Expression::Variable(VariableExpression {
            line_no: 1,
            token: Token {
                lexeme: "test".into(),
                token_type: TokenType::Identifier,
                line: 1,
            },
        })),
        value: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "assign"; "test_assign")]
    #[test_case(Expression::Logical(LogicalExpression {
        line_no: 1,
        operator: TokenType::And,
        left: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
        right: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "logical"; "test_logical")]
    #[test_case(Expression::Call(CallExpression {
        line_no: 1,
        callee: Box::new(Expression::Variable(VariableExpression {
            line_no: 1,
            token: Token {
                lexeme: "test".into(),
                token_type: TokenType::Identifier,
                line: 1,
            },
        })),
        arguments: vec![
            Expression::Literal(LiteralExpression {
                line_no: 1,
                value: Value::Integer(1),
            }),
            Expression::Literal(LiteralExpression {
                line_no: 1,
                value: Value::Integer(1),
            }),
        ],
    }), "call"; "test_call")]
    #[test_case(Expression::Dot(DotExpression {
        line_no: 1,
        object: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
        field: "test".into(),
    }), "dot"; "test_dot")]
    // #[test_case(Expression::Function(FunctionExpression {
    //     line_no: 1,
    //     name: "test".to_owned(),
    //     return_type: ValueType::UValueType(UValueType::Integer),
    //     captures: vec!["test".to_owned()],
    //     params: vec![("test".to_owned(), ValueType::UValueType(UValueType::Integer), false)],
    //     body: vec![Statement::Expression(ExpressionStatement {
    //         line_no: 1,
    //         expression: Expression::Literal(LiteralExpression {
    //             line_no: 1,
    //             value: Value::Integer(1),
    //         }),
    //     })],
    // }), "function"; "test_function")]
    #[test_case(Expression::Cast(CastExpression {
        line_no: 1,
        target_type: ValueType::Integer.intern(),
        expression: Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        })),
    }), "cast"; "test_cast")]
    fn test_to_str_for_expr(expr: Expression, name: &str) {
        let depth_has_scope_open = RefCell::new([false; 100]);

        let visitor = ToStrVisitor::new(&depth_has_scope_open, 0, None);
        let result = expr.as_node().accept(&visitor);

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(name);
        let _guard = settings.bind_to_scope();
        assert_snapshot!(result);
    }

    
    #[test_case(Statement::Break(BreakStatement {
        line_no: 1,
    }), "break"; "test_break")]
    #[test_case(Statement::Continue(ContinueStatement {
        line_no: 1,
    }), "continue"; "test_continue")]
    #[test_case(Statement::Return(ReturnStatement {
        line_no: 1,
        value: Some(Box::new(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        }))),
    }), "return"; "test_return")]
    #[test_case(Statement::Expression(ExpressionStatement {
        line_no: 1,
        expr: Box::new(Expression::Literal(LiteralExpression{line_no:1,value:Value::Integer(1),})),
    }), "expression"; "test_expression")]
    fn test_to_str_for_stmt(stmt: Statement, name: &str) {
        let depth_has_scope_open = [false; 100].into();

        let visitor = ToStrVisitor::new(&depth_has_scope_open, 0, None);
        let result = stmt.as_node().accept(&visitor);

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(name);
        let _guard = settings.bind_to_scope();
        assert_snapshot!(result);
    }

    #[test_case(Declaration::Var(VarDeclaration {
        line_no: 1,
        name: "test".into(),
        initializer: Some(Expression::Literal(LiteralExpression {
            line_no: 1,
            value: Value::Integer(1),
        }).into()),
        tipe: ValueType::Integer.intern(),
        mutable: false,
    }), "var"; "test_var")]
    #[test_case(Declaration::Function(FunctionDeclaration {
        line_no: 1,
        prototype: Prototype {
            line_no: 1,
            name: "test".into(),
            return_type: ValueType::Integer.intern(),
            captures: vec!["test".into()],
            params: vec![("test".into(), ValueType::Integer.intern(), false)],
        },
        body: vec![Statement::Expression(ExpressionStatement {
            line_no: 1,
            expr: Expression::Literal(LiteralExpression {
                line_no: 1,
                value: Value::Integer(1),
            }).into(),
        }).as_node()].into(),    
    }), "function"; "test_function")]
    fn test_to_str_for_decl(decl: Declaration, name: &str) {
        let depth_has_scope_open = [false; 100].into();

        let visitor = ToStrVisitor::new(&depth_has_scope_open, 0, None);
        let result = decl.as_node().accept(&visitor);

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(name);
        let _guard = settings.bind_to_scope();
        assert_snapshot!(result);
    }
}
