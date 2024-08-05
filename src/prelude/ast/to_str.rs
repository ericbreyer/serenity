use crate::prelude::shared_strings::SharedString;

use super::{
    Declaration, Expression, FunctionExpression, Node, NodeVisitor,
    Statement,
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
    depth_has_scope_open: &'a mut [bool; 100],
    depth: usize,
    close_scope: Option<usize>,
}

impl<'a> ToStrVisitor<'a> {
    pub fn new(
        depth_has_scope_open: &'a mut [bool; 100],
        depth: usize,
        close_scope: Option<usize>,
    ) -> Self {
        Self {
            depth_has_scope_open,
            depth,
            close_scope,
        }
    }
}

impl NodeVisitor<String> for ToStrVisitor<'_> {
    fn visit_expression(self, expression: &Expression) -> String {
        let depth = self.depth;
        let close_scope = self.close_scope;

        let mut s = String::new();
        let mut indent = String::new();
        s.push('\n');
        for i in 0..depth {
            if self.depth_has_scope_open[i] {
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
            + if close_scope.is_none() { T_CHAR } else { "" }
            + INDENT_MEMBER;
        close_scope.and_then(|c| {
            self.depth_has_scope_open[c] = false;
            Some(())
        });
        self.depth_has_scope_open[depth] = true;
        match expression {
            Expression::Literal(v, l) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Literal: {v:?}", l));
            }
            Expression::StringLiteral(v, l) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}StringLiteral: {v:?}",
                    l
                ));
            }
            Expression::Unary(t, e, l) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}Unary: {:?} {}",
                    l,
                    t,
                    e.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        Some(depth)
                    ))
                ));
            }
            Expression::Deref(e, l) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}Deref: {}",
                    l,
                    e.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        Some(depth)
                    ))
                ));
            }
            Expression::Ref(t, l) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}Ref: {}",
                    l,
                    t.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        Some(depth)
                    ))
                ));
            }
            Expression::Index(l, r, line) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Index:", line));
                s.push_str(&l.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&r.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::Binary(l, t, r, line) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Binary: {t:?} ", line));
                s.push_str(&l.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&r.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::Ternary(c, t, e, line) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Ternary:", line));
                s.push_str(&c.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&t.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&e.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::Variable(t, l) => {
                self.depth_has_scope_open[depth] = false;
                s.push_str(&format!("[line {:>4}] {indent_member}Variable: {t:?}", l));
            }
            Expression::Assign(t, e, l) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Assign:", l));

                s.push_str(&t.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&e.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::Logical(l, t, r, line) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}Logical: {t:?} ",
                    line
                ));
                s.push_str(&l.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&r.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::Call(f, a, l) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Call:", l));
                s.push_str(&f.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    if a.len() == 0 { Some(depth) } else { None },
                )));
                for (i, n) in a.iter().enumerate() {
                    s.push_str(&n.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        if i == a.len() - 1 { Some(depth) } else { None },
                    )));
                }
            }
            Expression::Dot(e, t, l) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Get: {t:?}", l));
                s.push_str(&e.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::Function(f, l) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}Function: {}",
                    l,
                    f.stringify(self.depth_has_scope_open, depth + 1, *l, Some(depth))
                ));
            }
            Expression::Cast(e, t, l) => {
                s.push_str(&format!("[line {:>4}] {indent_member}Cast: {t:?}", l));
                s.push_str(&e.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    Some(depth),
                )));
            }
            Expression::StructInitializer(c, v, l) => {
                s.push_str(&format!(
                    "[line {:>4}] {indent_member}StructInitializer: {c:?}",
                    l
                ));
                let indent = indent
                    .strip_suffix("    ")
                    .unwrap()
                    .to_owned()
                    .trim_end_matches(PIPE_END_CHAR)
                    .to_owned()
                    + INDENT;

                for (i, (t, e)) in v.iter().enumerate() {
                    if i as i64 > (v.len() as i64 - 2) {
                        self.depth_has_scope_open[depth] = false;
                    }

                    s.push_str(
                        format!(
                            "\n[line {:>4}] {indent}{}{INDENT_MEMBER} {}: ",
                            l,
                            if i == v.len() - 1 {
                                PIPE_END_CHAR
                            } else {
                                T_CHAR
                            },
                            t
                        )
                        .as_str(),
                    );

                    self.depth_has_scope_open[depth + 1] = true;
                    s.push_str(&e.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 2,
                        Some(depth + 1),
                    )));
                }
            }
            Expression::Empty => {
                // s.push_str(format!("[line {:>4}]  Empty").as_str());
            }
        }
        s.into()
    }

    fn visit_statement(self, statement: &Statement) -> String {
        let depth = self.depth;
        let close_scope = self.close_scope;

        let mut s = String::new();
        let mut indent = String::new();
        s.push('\n');
        for i in 0..depth {
            if self.depth_has_scope_open[i] {
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
        indent = indent
            .strip_suffix(INDENT_SUFFIX)
            .unwrap_or(&indent)
            .trim_end_matches(PIPE_CHAR)
            .to_owned()
            + if close_scope.is_none() { T_CHAR } else { "" }
            + INDENT_MEMBER;
        close_scope.and_then(|c| {
            self.depth_has_scope_open[c] = false;
            Some(())
        });
        self.depth_has_scope_open[depth] = true;
        match statement {
            Statement::Print(e, line) => {
                s.push_str(&format!(
                    "[line {line:>4}] {indent}Print: {}",
                    e.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        Some(depth)
                    ))
                ));
            }
            Statement::Block(v, line) => {
                s.push_str(format!("[line {line:>4}] {indent}Block: ").as_str());
                for (i, n) in v.iter().enumerate() {
                    s.push_str(&n.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        if i == v.len() - 1 { Some(depth) } else { None },
                    )));
                }
            }
            Statement::If(c, t, e, line) => {
                s.push_str(&format!("[line {line:>4}] {indent}If:"));
                s.push_str(&c.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&t.accept(ToStrVisitor::new(
                    self.depth_has_scope_open,
                    depth + 1,
                    if e.is_none() { Some(depth) } else { None },
                )));
                if let Some(e) = e {
                    s.push_str(&e.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        Some(depth),
                    )));
                }
            }
            Statement::While(c, b, line) => {
                s.push_str(&format!("[line {line:>4}] {indent}While:"));
                
                s.push_str(&c.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                s.push_str(&b.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, Some(depth))));
            }
            Statement::For(i, c, u, b, line) => {
                s.push_str(&format!("[line {line:>4}] {indent}For:"));
                if let Some(i) = i {
                    s.push_str(&i.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));

                }
                if let Some(c) = c {
                    s.push_str(&c.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                }
                if let Some(u) = u {
                    s.push_str(&u.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, None)));
                }
                s.push_str(&b.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, Some(depth))));
            }
            Statement::Break(line) => {
                s.push_str(&format!("[line {line:>4}] {indent}Break"));
            }
            Statement::Continue(line) => {
                s.push_str(&format!("[line {line:>4}] {indent}Continue"));
            }
            Statement::Return(e, line) => {
                s.push_str(&format!("[line {line:>4}] {indent}Return:"));
                if let Some(e) = e {
                    s.push_str(&e.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, Some(depth))));
                } else {
                    self.depth_has_scope_open[depth] = false;
                }
            }
            Statement::Expression(e, line) => {
                s.push_str(&format!(
                    "[line {line:>4}] {indent}Expression: {}",
                    e.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 1, Some(depth)))
                ));
            }
        }
        s.into()
    }

    fn visit_declaration(self, declaration: &Declaration) -> String {
        let depth = self.depth;
        let close_scope = self.close_scope;

        let mut s = String::new();
        let mut indent = String::new();
        s.push('\n');
        for i in 0..depth {
            if self.depth_has_scope_open[i] {
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
            + if close_scope.is_none() { T_CHAR } else { "" }
            + INDENT_MEMBER;
        close_scope.and_then(|c| {
            self.depth_has_scope_open[c] = false;
            Some(())
        });
        self.depth_has_scope_open[depth] = true;
        match declaration {
            Declaration::Var(v, line) => {
                let mindent = indent.strip_suffix(INDENT_PIPE_END);
                let indent = if mindent.is_some() {
                    mindent.unwrap().to_owned() + INDENT
                } else {
                    indent.to_owned()
                };
                s.push_str(&format!(
                    "[line {line:>4}] {indent_member}Var Declaration: {}",
                    &v.name
                ));

                if let Some(e) = &v.initializer {
                    s.push_str(&format!(
                        "\n[line {line:>4}] {indent}{PIPE_END_CHAR}{INDENT_MEMBER} Initializer"
                    ));
                    self.depth_has_scope_open[depth + 1] = true;
                    self.depth_has_scope_open[depth] = false;
                    s.push_str(&e.accept(ToStrVisitor::new(self.depth_has_scope_open, depth + 2, Some(depth + 1))));
                } else {
                    self.depth_has_scope_open[depth] = false;
                }
            }
            Declaration::Function(f, line) => {
                s.push_str(&format!(
                    "[line {line:>4}] {indent_member}Fun Declaration: {}",
                    &f.name
                ));

                s.push_str(&f.body.stringify(self.depth_has_scope_open, depth + 1, *line, Some(depth)));
            }
            Declaration::Array(i, line) => {
                s.push_str(&format!(
                    "[line {line:>4}] {indent_member}Array Declaration:\n"
                ));
                s.push_str(&format!(
                    "[line {line:>4}] {indent}{INDENT_PIPE}Name {}\n",
                    &i.name
                ));
                if let Some(t) = &i.elem_tipe {
                    s.push_str(&format!(
                        "[line {line:>4}] {indent}{INDENT_PIPE}Elem Type: {t:?}"
                    ));
                }
                for (j, n) in i.elements.iter().enumerate() {
                    s.push_str(&n.accept(ToStrVisitor::new(
                        self.depth_has_scope_open,
                        depth + 1,
                        if j == i.elements.len() - 1 {
                            Some(depth)
                        } else {
                            None
                        },
                    )));
                }
            }
        }
        s.into()
    }
}

impl FunctionExpression {
    fn stringify(
        &self,
        depth_has_scope_open: &mut [bool; 100],
        depth: usize,
        line: usize,
        close_scope: Option<usize>,
    ) -> SharedString {
        let mut s = String::new();
        s.push('\n');
        let mut indent = String::new();
        for i in 0..depth {
            if depth_has_scope_open[i] {
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
        close_scope.and_then(|c| {
            depth_has_scope_open[c] = false;
            Some(())
        });

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

        if self.captures.len() > 0 {
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

        if self.params.len() > 0 {
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

        depth_has_scope_open[depth] = true;

        s.push_str(&format!(
            "\n[line {line:>4}] {indent}{PIPE_END_CHAR}{INDENT_MEMBER}Body: "
        ));
        depth_has_scope_open[depth + 1] = true;
        for (i, n) in self.body.iter().enumerate() {
            // s.push_str(&n.to_string_scopes(
            //     depth + 2,
            //     if i == self.body.len() - 1 {
            //         Some(depth + 1)
            //     } else {
            //         None
            //     },
            // ));
            s.push_str(&n.accept(ToStrVisitor::new(
                depth_has_scope_open,
                depth + 1,
                if i == self.body.len() - 1 {
                    Some(depth)
                } else {
                    None
                },
            )));
        }
        s.into()
    }
}