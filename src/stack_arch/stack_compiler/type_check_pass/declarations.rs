use tracing::instrument;

use crate::{
  common::{
    parsed_ast::{ ArrayDeclaration, Expression, FunctionDeclaration, StructDeclaration, VarDeclaration },
    declared_ast::{ self },
  },
  typing::ValueType,
};

use super::{ TypeCheckWalker, Local };

use crate::error;

impl TypeCheckWalker {
  #[instrument(level = "trace", skip(self))]
  pub fn var_declaration(
    &mut self,
    mut v: VarDeclaration,
    line: usize
  ) -> declared_ast::Declaration {
    let name = v.name.clone();
    let global_id = self.parse_variable(v.name, v.mutable);

    if !v.mutable && v.initializer.is_none() {
      error!(self, &format!("[line {line}] Immutable variable must be initialized"));
      return declared_ast::Declaration::Empty;
    }

    if let Some(var_type) = v.tipe {
      if global_id == -1 {
        let last_local = self.last_local();
        self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = var_type;
        // self.function_compiler.local_count += var_type.num_words() as usize;
      } else {
        self.gloabls.insert(name.clone(), global_id as usize);
        self.global_types.insert(global_id as usize, (var_type, v.mutable));
      }
    }
    let mut ndinit = None;
    if let Some(expr) = v.initializer {
      let (ndexpr, expr_type) = self.visit_expression(*expr, false);
      ndinit = Some(ndexpr);
      if let Some(var_type) = v.tipe {
        if expr_type != var_type {
          error!(self, &format!("Type mismatch, expected {var_type:?} got {expr_type:?}"));
        }
      }

      v.tipe = Some(expr_type);

      if global_id == -1 {
        let last_local = self.last_local();
        self.function_compiler.locals.get_mut(&last_local).unwrap().assigned = true;
        self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = expr_type;
        // self.function_compiler.local_count += expr_type.num_words() as usize;
      } else {
        self.gloabls.insert(name.clone(), global_id as usize);
        self.global_types.insert(global_id as usize, (expr_type, v.mutable));
      }
    }

    self.define_variable(global_id as u32, v.tipe.unwrap());


    declared_ast::Declaration::Var(
      declared_ast::VarDeclaration {
        name: name.clone(),
        tipe: v.tipe.expect("tipe is none"),
        initializer: ndinit.map(Box::new),
      },
      line
    )
  }

  pub fn parse_variable(&mut self, name: SharedString, mutable: bool) -> i64 {
    self.declare_variable(name.clone(), mutable);
    if self.function_compiler.scope_depth > 0 {
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().depth =
        self.function_compiler.scope_depth;
      return -1;
    }

    let id = self.global_types.iter().map(|(_, t)| t.0.num_words()).sum::<usize>();
    self.gloabls.insert(name.clone(), id);
    self.global_types.insert(id, (ValueType::Undef.intern(), mutable));
    return *self.gloabls.get(&name).unwrap() as i64;
  }

  pub fn declare_variable(&mut self, name: SharedString, mutable: bool) {
    if self.function_compiler.scope_depth == 0 {
      return;
    }

    for i in self.function_compiler.locals.keys() {
      let local = &self.function_compiler.locals.get(i).unwrap();
      if local.depth != -1 && local.depth < self.function_compiler.scope_depth {
        break;
      }

      if name == local.name {
        error!(self, &format!("Variable {name} already declared in this scope"));
      }
    }

    self.add_local(name, mutable);
  }

  fn add_local(&mut self, name: SharedString, mutable: bool) {
    let local = Local {
      name: name.clone(),
      depth: -1,
      mutable,
      assigned: false,
      captured: false,
      local_type: ValueType::Undef.intern(),
    };
    self.function_compiler.locals.insert(self.function_compiler.local_count, local);
  }

  #[instrument(level = "trace", skip(self))]
  pub fn fun_declaration(
    &mut self,
    f: FunctionDeclaration,
    line: usize
  ) -> declared_ast::Declaration {
    let name = f.name;
    let global_id = self.parse_variable(name.clone(), false);
    if self.function_compiler.scope_depth > 0 {
      // self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().depth =
        self.function_compiler.scope_depth;
    }

    let (ndf, t) = self.function(f.body, line);

    if global_id == -1 {
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = t;
    } else {
      self.gloabls.insert(name.clone(), global_id as usize);
      self.global_types.insert(global_id as usize, (t, false));
    }
    self.define_variable(global_id as u32, t);

    declared_ast::Declaration::Function(
      declared_ast::FunctionDeclaration {
        name,
        body: match ndf {
            declared_ast::Expression::Function(f, _) => f,
            _ => declared_ast::FunctionExpression::default()
        }
      },

      line
    )
  }

  #[instrument(level = "trace", skip(self))]
  pub fn struct_declaration(
    &mut self,
    sd: StructDeclaration,
    _line: usize
  ) -> declared_ast::Declaration {
    let s = sd.s;
    let _name = s.name;
    let _fields = s.fields;
    let _offset = 0;
    declared_ast::Declaration::Empty
  }

  #[instrument(level = "trace", skip(self))]
  pub fn array_declaration(
    &mut self,
    a: ArrayDeclaration,
    line: usize
  ) -> declared_ast::Declaration {
    let global_id = self.parse_variable(a.name.clone(), false);
    let var_type = a.elem_tipe.expect("elem_tipe is none");
    let n = a.elements.len();

    self.mark_initialized();

    if global_id == -1 {
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = ValueType::Array(
        var_type,
        n
      ).intern();
      self.function_compiler.locals.get_mut(&last_local).unwrap().mutable = false;
      self.function_compiler.locals.get_mut(&last_local).unwrap().assigned = true;
    } else {
      self.global_types.insert(global_id as usize, (ValueType::Array(var_type, n).intern(), false));
    }

    let name = a.name.clone();
    let init = self.define_array(global_id, Some(n as u32), a);
    
    declared_ast::Declaration::Array(declared_ast::ArrayDeclaration{ name: name, elements: init, elem_tipe: var_type }, line)
  }

  fn define_array(
    &mut self,
    global_id: i64,
    size: Option<u32>,
    a: ArrayDeclaration
  ) -> Vec<declared_ast::Expression> {
    let _line = 0;

    if global_id == -1 {
      let last_local = self.last_local();
      let elem_type = self.function_compiler.locals.get(&last_local).unwrap().local_type;
      let mut pointee_type = match elem_type.decay(self.custom_structs.clone().into()).as_ref() {
        ValueType::Pointer(t, _) => *t,
        _ => ValueType::Undef.intern(),
      };

      if
        let ValueType::Array(_, n) = self.function_compiler.locals
          .get(&last_local)
          .unwrap()
          .local_type.as_ref()
      {
        if *n != a.elements.len() {
          error!(
            self,
            &format!("Args in initializer don't match, expected {} got {}", n, a.elements.len())
          );
        }

        self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = ValueType::Array(
          pointee_type,
          *n
        ).intern();
        self.function_compiler.local_count += 1;
      } else {
        let n = size.expect("size is none");
        for _ in 0..n {
          for _ in 0..elem_type.num_words() {
            self.function_compiler.local_count += 1;
          }
        }
      }

      if let Expression::Empty = a.elements[0] {
        for _ in 0..size.unwrap() {
          for _ in 0..pointee_type.num_words() {
            self.function_compiler.local_count += 1;
          }
        }
      }
      let mut ndelems = Vec::new();
      for v in &a.elements {
        let (nde, pt) = self.visit_expression(v.clone(), false);
        ndelems.push(nde);
        if pointee_type == ValueType::Undef.intern() {
          pointee_type = pt;
        }
        self.function_compiler.local_count += pointee_type.num_words();

        if pt != pointee_type && pt != ValueType::Nil.intern() {
          error!(
            self,
            format!("b Element must have type {pointee_type:?}, got {pt:?} instead").as_str()
          );
        }
      }
      ndelems
    } else {
      // global array
      let elem_type = self.global_types.get(&(global_id as usize)).unwrap().0;
      let mut pointee_type = match elem_type.decay(self.custom_structs.clone().into()).as_ref() {
        ValueType::Pointer(t, _) => *t,
        _ => ValueType::Undef.intern(),
      };

      let mut ndelems = Vec::new();
      for v in a.elements.clone() {
        let (nde, pt) = self.visit_expression(v, false);
        ndelems.push(nde);
        if pointee_type == ValueType::Undef.intern() {
          pointee_type = pt;
        }

        if pt != pointee_type && pt != ValueType::Nil.intern() {
          error!(
            self,
            format!("a Element must have type {pointee_type:?}, got {pt:?} instead").as_str()
          );
        }
      }

      if
        let ValueType::Array(_, n) = self.global_types
          .get(&(global_id as usize))
          .unwrap()
          .0.as_ref()
      {
        if *n != a.elements.len() {
          error!(
            self,
            &format!("Args in initializer don't match, expected {} got {}", n, a.elements.len())
          );
        }
      }
      ndelems
    }
  }
}
