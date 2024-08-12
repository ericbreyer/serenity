use std::sync::atomic::AtomicBool;

use super::{ ArrayDeclaration, Declaration, Expression, FunctionExpression, NDASTNode, Statement };

const PIPE_END_CHAR: &str = "╰";
const PIPE_CHAR: &str = "│";
const _MEMBER_CHAR: &str = "─";
const T_CHAR: &str = "├";

const INDENT: &str = "     ";
const INDENT_SUFFIX: &str = "    ";
const INDENT_PIPE: &str = "│    ";
const INDENT_PIPE_END: &str = "╰    ";
const INDENT_MEMBER: &str = "─── ";
const ARRAY_DEFAULT_VALUE: AtomicBool = AtomicBool::new(false);
static DEPTH_HAS_SCOPE_OPEN: [AtomicBool; 100] = [ARRAY_DEFAULT_VALUE; 100];

pub trait ToStr {
  fn to_string_scopes(&self, depth: usize, close_scope: Option<usize>) -> SharedString;

  fn to_string(&self) -> SharedString {
    self.to_string_scopes(0, None)
  }
}

impl ToStr for NDASTNode {
  fn to_string_scopes(&self, depth: usize, close_scope: Option<usize>) -> SharedString {
    let mut s = String::new();

    match self {
      NDASTNode::Err => {
        s.push_str("Err");
      }
      NDASTNode::Group(v) => {
        for n in v {
          s.push_str(&n.to_string_scopes(depth, close_scope));
        }
      }
      NDASTNode::Module(name, v) => {
        let mut indent = String::new();
        s.push('\n');
        for i in 0..depth {
          if DEPTH_HAS_SCOPE_OPEN[i].load(std::sync::atomic::Ordering::SeqCst) {
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
        indent = if indent == "" {
          "".to_owned()
        } else {
          indent.strip_suffix(INDENT_SUFFIX).unwrap().trim_end_matches(PIPE_CHAR).to_owned() +
            (if close_scope.is_none() { T_CHAR } else { "" }) +
            INDENT_MEMBER
        };
        close_scope.and_then(|c| {
          DEPTH_HAS_SCOPE_OPEN[c].store(false, std::sync::atomic::Ordering::SeqCst);
          Some(())
        });
        DEPTH_HAS_SCOPE_OPEN[depth].store(true, std::sync::atomic::Ordering::SeqCst);
        s.push_str(&format!("[         ] {indent}Module: {name}"));
        for (i, n) in v.iter().enumerate() {
          s.push_str(
            &n.to_string_scopes(depth + 1, if i == v.len() - 1 { Some(depth) } else { None })
          );
        }
      }
      NDASTNode::Statement(st) => {
        s.push_str(&format!("{}", st.to_string_scopes(depth, close_scope)));
      }
      NDASTNode::Expression(e) => {
        s.push_str(&format!("{}", e.to_string_scopes(depth, close_scope)));
      }
      NDASTNode::Declaration(d) => {
        s.push_str(&format!("{}", d.to_string_scopes(depth, close_scope)));
      }
    }
    s
  }
}

impl ToStr for Expression {
  fn to_string_scopes(&self, depth: usize, close_scope: Option<usize>) -> SharedString {
    let mut s = String::new();
    let mut indent = String::new();
    s.push('\n');
    for i in 0..depth {
      if DEPTH_HAS_SCOPE_OPEN[i].load(std::sync::atomic::Ordering::SeqCst) {
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
    let indent_member =
      indent.strip_suffix(INDENT_SUFFIX).unwrap().trim_end_matches(PIPE_CHAR).to_owned() +
      (if close_scope.is_none() { T_CHAR } else { "" }) +
      INDENT_MEMBER;
    close_scope.and_then(|c| {
      DEPTH_HAS_SCOPE_OPEN[c].store(false, std::sync::atomic::Ordering::SeqCst);
      Some(())
    });
    DEPTH_HAS_SCOPE_OPEN[depth].store(true, std::sync::atomic::Ordering::SeqCst);
    match self {
     Expression::Literal(LiteralExpression{ value: v, line_no:  l}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Literal: {v:?}", l));
      }
      Expression::StringLiteral(v, l) => {
        s.push_str(&format!("[line {:>4}] {indent_member}StringLiteral: {v:?}", l));
      }
      Expression::Unary(t, e, l) => {
        s.push_str(
          &format!(
            "[line {:>4}] {indent_member}Unary: {:?} {}",
            l,
            t,
            e.to_string_scopes(depth + 1, Some(depth))
          )
        );
      }
      Expression::Deref(DerefExpression{ operand: e, line_no:  l}) => {
        s.push_str(
          &format!(
            "[line {:>4}] {indent_member}Deref: {}",
            l,
            e.to_string_scopes(depth + 1, Some(depth))
          )
        );
      }
      Expression::Ref(t, l) => {
        s.push_str(
          &format!(
            "[line {:>4}] {indent_member}Ref: {}",
            l,
            t.to_string_scopes(depth + 1, Some(depth))
          )
        );
      }
     Expression::Index(IndexExpression{ array: l, index:  r, line_no:  line}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Index:", line));
        s.push_str(&l.to_string_scopes(depth + 1, None));
        s.push_str(&r.to_string_scopes(depth + 1, Some(depth)));
      }
     Expression::Binary(BinaryExpression{ left: l, operator:  t, right:  r, line_no:  line}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Binary: {t:?} ", line));
        s.push_str(&l.to_string_scopes(depth + 1, None));
        s.push_str(&r.to_string_scopes(depth + 1, Some(depth)));
      }
     Expression::Ternary(TernaryExpression{ condition: c, then_branch:  t, else_branch:  e, line_no:  line}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Ternary:", line));
        s.push_str(&c.to_string_scopes(depth + 1, None));
        s.push_str(&t.to_string_scopes(depth + 1, None));
        s.push_str(&e.to_string_scopes(depth + 1, Some(depth)));
      }
      Expression::Variable(t, l) => {
        DEPTH_HAS_SCOPE_OPEN[depth].store(false, std::sync::atomic::Ordering::SeqCst);
        s.push_str(&format!("[line {:>4}] {indent_member}Variable: {t:?}", l));
      }
     Expression::Assign(AssignExpression{ variable: t, value:  e, line_no:  l}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Assign:", l));
        s.push_str(&t.to_string_scopes(depth + 1, None));
        s.push_str(&e.to_string_scopes(depth + 1, Some(depth)));
      }
     Expression::Logical(LogicalExpression{ left: l, operator:  t, right:  r, line_no:  line}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Logical: {t:?} ", line));
        s.push_str(&l.to_string_scopes(depth + 1, None));
        s.push_str(&r.to_string_scopes(depth + 1, Some(depth)));
      }
     Expression::Call(CallExpression{ callee: f, arguments:  a, line_no:  l}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Call:", l));
        s.push_str(&f.to_string_scopes(depth + 1, if a.len() == 0 { Some(depth) } else { None }));
        for (i, n) in a.iter().enumerate() {
          s.push_str(
            &n.to_string_scopes(depth + 1, if i == a.len() - 1 { Some(depth) } else { None })
          );
        }
      }
     Expression::Dot(DotExpression{ object: e, field:  t, line_no:  l}) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Get: {t:?}", l));
        s.push_str(&e.to_string_scopes(depth + 1, Some(depth)));
      }
      Expression::Function(f, l) => {
        s.push_str(
          &format!(
            "[line {:>4}] {indent_member}Function: {}",
            l,
            f.stringify(depth + 1, *l, Some(depth))
          )
        );
      }
      Expression::Cast(e, t, _, l) => {
        s.push_str(&format!("[line {:>4}] {indent_member}Cast: {t:?}", l));
        s.push_str(&e.to_string_scopes(depth + 1, Some(depth)));
      }
      Expression::StructInitializer(c, v, l) => {
        s.push_str(&format!("[line {:>4}] {indent_member}StructInitializer: {c:?}", l));
        let indent =
          indent
            .strip_suffix("   ")
            .unwrap()
            .to_owned()
            .trim_end_matches(PIPE_END_CHAR)
            .to_owned() + INDENT;
        for (i, (t, e)) in v.iter().enumerate() {
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
            ).as_str()
          );

          DEPTH_HAS_SCOPE_OPEN[depth + 1].store(true, std::sync::atomic::Ordering::SeqCst);
          s.push_str(&e.to_string_scopes(depth + 2, Some(depth + 1)));

          if i > 1 && i == v.len() - 2 {
            DEPTH_HAS_SCOPE_OPEN[depth].store(false, std::sync::atomic::Ordering::SeqCst);
          }
        }
      }
      Expression::Empty => {
        s.push_str("[line {:>4}]  Empty");
      }
    }
    s
  }
}

impl ToStr for Statement {
  fn to_string_scopes(&self, depth: usize, close_scope: Option<usize>) -> SharedString {
    let mut s = String::new();
    let mut indent = String::new();
    s.push('\n');
    for i in 0..depth {
      if DEPTH_HAS_SCOPE_OPEN[i].load(std::sync::atomic::Ordering::SeqCst) {
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
    indent =
      indent.strip_suffix(INDENT_SUFFIX).unwrap().trim_end_matches(PIPE_CHAR).to_owned() +
      (if close_scope.is_none() { T_CHAR } else { "" }) +
      INDENT_MEMBER;
    close_scope.and_then(|c| {
      DEPTH_HAS_SCOPE_OPEN[c].store(false, std::sync::atomic::Ordering::SeqCst);
      Some(())
    });
    DEPTH_HAS_SCOPE_OPEN[depth].store(true, std::sync::atomic::Ordering::SeqCst);
    match self {
      Statement::Print(e, line) => {
        s.push_str(
          &format!("[line {line:>4}] {indent}Print: {}", e.to_string_scopes(depth + 1, Some(depth)))
        );
      }
      Statement::Block(v, _, line) => {
        s.push_str(format!("[line {line:>4}] {indent}Block: ").as_str());
        for (i, n) in v.iter().enumerate() {
          s.push_str(
            &n.to_string_scopes(depth + 1, if i == v.len() - 1 { Some(depth) } else { None })
          );
        }
      }
      Statement::If(c, t, e, line) => {
        s.push_str(&format!("[line {line:>4}] {indent}If:"));
        s.push_str(&c.to_string_scopes(depth + 1, None));
        s.push_str(&t.to_string_scopes(depth + 1, if e.is_none() { Some(depth) } else { None }));
        if let Some(e) = e {
          s.push_str(&e.to_string_scopes(depth + 1, Some(depth)));
        }
      }
      Statement::While(c, b, line) => {
        s.push_str(&format!("[line {line:>4}] {indent}While:"));
        s.push_str(&c.to_string_scopes(depth + 1, None));
        s.push_str(&b.to_string_scopes(depth + 1, Some(depth)));
      }
      Statement::For(_, i, c, u, b, line) => {
        s.push_str(&format!("[line {line:>4}] {indent}For:"));
        if let Some(i) = i {
          s.push_str(&i.to_string_scopes(depth + 1, None));
        }
        if let Some(c) = c {
          s.push_str(&c.to_string_scopes(depth + 1, None));
        }
        if let Some(u) = u {
          s.push_str(&u.to_string_scopes(depth + 1, None));
        }
        s.push_str(&b.to_string_scopes(depth + 1, Some(depth)));
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
          s.push_str(&e.to_string_scopes(depth + 1, Some(depth)));
        } else {
          DEPTH_HAS_SCOPE_OPEN[depth].store(false, std::sync::atomic::Ordering::SeqCst);
        }
      }
      Statement::Expression(e, line) => {
        s.push_str(
          &format!(
            "[line {line:>4}] {indent}Expression: {}",
            e.to_string_scopes(depth + 1, Some(depth))
          )
        );
      }
    }
    s
  }
}

impl FunctionExpression {
  fn stringify(&self, depth: usize, line: usize, close_scope: Option<usize>) -> SharedString {
    let mut s = String::new();
    s.push('\n');
    let mut indent = String::new();
    for i in 0..depth {
      if DEPTH_HAS_SCOPE_OPEN[i].load(std::sync::atomic::Ordering::SeqCst) {
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
    let indent_member =
      indent.strip_suffix(INDENT_SUFFIX).unwrap().trim_end_matches(PIPE_CHAR).to_owned() +
      (if close_scope.is_none() { T_CHAR } else { "" }) +
      INDENT_MEMBER;
    indent =
      indent
        .strip_suffix(INDENT_SUFFIX)
        .unwrap()
        .to_owned()
        .trim_end_matches(PIPE_END_CHAR)
        .to_owned() + INDENT;
    close_scope.and_then(|c| {
      DEPTH_HAS_SCOPE_OPEN[c].store(false, std::sync::atomic::Ordering::SeqCst);
      Some(())
    });

    s.push_str(&format!("[line {line:>4}] {indent_member}Function Expression:"));

    if self.captures.len() > 0 {
      s.push_str(&format!("\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Captures:"));
      for (i, str) in self.captures.iter().enumerate() {
        s.push('\n');
        s.push_str(
          &format!("[line {line:>4}] {indent}{INDENT_PIPE}{}{INDENT_MEMBER}{str}", if
            i == self.captures.len() - 1
          {
            PIPE_END_CHAR
          } else {
            T_CHAR
          })
        );
      }
    }

    if self.params.len() > 0 {
      s.push_str(&format!("\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Params:"));
      for (i, (str, t, _mut)) in self.params.iter().enumerate() {
        s.push('\n');
        s.push_str(
          &format!("[line {line:>4}] {indent}{INDENT_PIPE}{}{INDENT_MEMBER}{str}: {t:?}", if
            i == self.params.len() - 1
          {
            PIPE_END_CHAR
          } else {
            T_CHAR
          })
        );
      }
    }
    if self.locals.len() > 0 {
      s.push_str(&format!("\n[line {line:>4}] {indent}{T_CHAR}{INDENT_MEMBER}Locals:"));
      let mut iter = self.locals.iter().collect::<Vec<_>>();
      iter.sort_unstable_by_key(|t| t.0);
      for (i, (place, l)) in iter.iter().enumerate() {
        s.push('\n');
        s.push_str(
          &format!("[line {line:>4}] {indent}{INDENT_PIPE}{}{INDENT_MEMBER}[{place:4}] {l:?}", if
            i == self.locals.len() - 1
          {
            PIPE_END_CHAR
          } else {
            T_CHAR
          })
        );
      }
    }

    DEPTH_HAS_SCOPE_OPEN[depth].store(false, std::sync::atomic::Ordering::SeqCst);

    s.push_str(&format!("\n[line {line:>4}] {indent}{PIPE_END_CHAR}{INDENT_MEMBER}Body: "));
    DEPTH_HAS_SCOPE_OPEN[depth + 1].store(true, std::sync::atomic::Ordering::SeqCst);
    for (i, n) in self.body.iter().enumerate() {
      s.push_str(
        &n.to_string_scopes(depth + 2, if i == self.body.len() - 1 {
          Some(depth + 1)
        } else {
          None
        })
      );
    }
    s
  }
}

impl ToStr for Declaration {
  fn to_string_scopes(&self, depth: usize, close_scope: Option<usize>) -> SharedString {
    let mut s = String::new();
    let mut indent = String::new();
    s.push('\n');
    for i in 0..depth {
      if DEPTH_HAS_SCOPE_OPEN[i].load(std::sync::atomic::Ordering::SeqCst) {
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
    let indent_member =
      indent.strip_suffix(INDENT_SUFFIX).unwrap().trim_end_matches(PIPE_CHAR).to_owned() +
      (if close_scope.is_none() { T_CHAR } else { "" }) +
      INDENT_MEMBER;
    close_scope.and_then(|c| {
      DEPTH_HAS_SCOPE_OPEN[c].store(false, std::sync::atomic::Ordering::SeqCst);
      Some(())
    });
    DEPTH_HAS_SCOPE_OPEN[depth].store(true, std::sync::atomic::Ordering::SeqCst);
    match self {
      Declaration::Var(v, line) => {
        let mindent = indent.strip_suffix(INDENT_PIPE_END);
        let indent = if mindent.is_some() {
          mindent.unwrap().to_owned() + INDENT
        } else {
          indent.to_owned()
        };
        s.push_str(&format!("[line {line:>4}] {indent_member}Var Declaration: {}", &v.name));

        if let Some(e) = &v.initializer {
          s.push_str(
            &format!("\n[line {line:>4}] {indent}{PIPE_END_CHAR}{INDENT_MEMBER} Initializer")
          );
          DEPTH_HAS_SCOPE_OPEN[depth + 1].store(true, std::sync::atomic::Ordering::SeqCst);
          DEPTH_HAS_SCOPE_OPEN[depth].store(false, std::sync::atomic::Ordering::SeqCst);
          s.push_str(&e.to_string_scopes(depth + 2, Some(depth + 1)));
        } else {
          DEPTH_HAS_SCOPE_OPEN[depth].store(false, std::sync::atomic::Ordering::SeqCst);
        }
      }
      Declaration::Function(f, line) => {
        s.push_str(&format!("[line {line:>4}] {indent_member}Fun Declaration: {}", &f.name));

        s.push_str(&f.body.stringify(depth + 1, *line, Some(depth)));
      }
      Declaration::Array(i, line) => {
        s.push_str(&format!("[line {line:>4}] {indent_member}Array Declaration:\n"));
        s.push_str(&format!("[line {line:>4}] {indent}{INDENT}Name {}", &i.name));

        s.push_str(&format!("[line {line:>4}] {indent}{INDENT}Elem Type: {:?}", i.elem_tipe));

        for (j, n) in i.elements.iter().enumerate() {
          s.push_str(
            &n.to_string_scopes(depth + 1, if j == i.elements.len() - 1 {
              Some(depth)
            } else {
              None
            })
          );
        }
      }
        Declaration::Empty => {
          s.push_str("[line {line:>4}]  Empty");
        }
    }
    s
  }
}

impl ToStr for ArrayDeclaration {
  fn to_string_scopes(&self, depth: usize, _close_scope: Option<usize>) -> SharedString {
    let mut s = String::new();
    s.push('\n');
    for _ in 0..depth {
      s.push_str(INDENT);
    }
    s.push_str(&format!("ArrayDeclaration: {:?}", self.name));

    s.push_str(&format!("  Elem Type: {:?}", self.elem_tipe));

    let e = &self.elements;
    for n in e {
      s.push_str(&n.to_string_scopes(depth + 1, None));
    }

    s
  }
}
