use tracing::{ instrument, span, Level };

use crate::{
  chunk::Opcode,
  common::declared_ast::{ ArrayDeclaration, Expression, FunctionDeclaration, VarDeclaration },
  typing::ValueType,
  value::{ pointer::Pointer, Value },
  error,
};

use super::EmitWalker;

impl EmitWalker {
  #[instrument(level = "trace", skip(self))]
  pub fn var_declaration(&mut self, v: VarDeclaration, line: usize) {
    let _guard = span!(Level::TRACE, "var_declaration");

    let name = v.name.clone();
    let global_id = if self.resolve_local(0, &name).0.is_some() {
      None
    } else {
      
    self.gloabls.get(&name).copied()
    };

    // if global_id.is_some() {
    //     println!("defining global variable {} with type {:?} with id {}", name, v.tipe, global_id.unwrap());

    //     for _ in 0..v.tipe.num_words() {
    //         self.static_data_segment.push(Value::Nil.to_word());
    //       }
    // }

    if let Some(expr) = v.initializer {
      let _expr_type = self.visit_expression(*expr, false);
    } else {
      for _ in 0..v.tipe.num_words() {
        self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
      }
    };

    if let Some(id) = global_id {
      self.function_compiler.func.chunk.define_global(id as u32, line, v.tipe);
    } else {
      self.function_compiler.local_count+= v.tipe.num_words();
    }
  }

  #[instrument(level = "trace", skip(self))]
  pub fn fun_declaration(&mut self, f: FunctionDeclaration, line: usize) {
    let name = f.name;
    let global_id = if self.resolve_local(0, &name).0.is_some() {
        None
      } else {
        
      self.gloabls.get(&name).copied()
      };
    // if global_id.is_some(){

    //     let cap_size = f.body.captures.iter().map(|c| self.variable(
    //         c.clone(),
    //         false,
    //         0
    //       )).map(|t| t.num_words()).sum::<usize>();
        
    //     for _ in 0..cap_size+1 {
    //         self.static_data_segment.push(Value::Nil.to_word());
    //       }
    // }

    let t = self.function(f.body, 0);

    if let Some(id) = global_id {
      self.function_compiler.func.chunk.define_global(id as u32, line, t);
    } else {
        self.function_compiler.local_count+= t.num_words();
    }
  }

  #[instrument(level = "trace", skip(self))]
  pub fn array_declaration(&mut self, a: ArrayDeclaration, line: usize) {
    let global_id = if self.resolve_local(0, &a.name).0.is_some() {
        None
      } else {
        
      self.gloabls.get(&a.name).copied()
      };
    let _var_type = a.elem_tipe;
    let n = a.elements.len();

    self.define_array(global_id, Some(n as u32), a, line);
  }

  fn define_array(
    &mut self,
    global_id: Option<usize>,
    size: Option<u32>,
    a: ArrayDeclaration,
    _line: usize
  ) {
    let line = 0;

    if global_id.is_none() {
      self.function_compiler.func.chunk.write(Opcode::DefineStackArray as u8, line);

      let elem_type = a.elem_tipe;
      let mut pointee_type = match elem_type.decay(self.custom_structs.clone().into()).as_ref() {
        ValueType::Pointer(t, _) => *t,
        _ => ValueType::Undef.intern(),
      };

      if let Expression::Empty = a.elements[0] {
        for _ in 0..size.unwrap() {
          for _ in 0..pointee_type.num_words() {
            self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
          }
        }
        return;
      }

      for v in &a.elements {
        let pt = self.visit_expression(v.clone(), false);
        if pointee_type == ValueType::Undef.intern() {
          pointee_type = pt;
        }

        if pt != pointee_type && pt != ValueType::Nil.intern() {
          error!(
            self,
            format!("b Element must have type {pointee_type:?}, got {pt:?} instead").as_str()
          );
        }
      }
      self.function_compiler.local_count += size.unwrap() as usize * pointee_type.num_words() + 1;
    } else {
      let global_id = unsafe { global_id.unwrap_unchecked() };
      // global array
      let elem_type = a.elem_tipe;
      let mut pointee_type = match elem_type.decay(self.custom_structs.clone().into()).as_ref() {
        ValueType::Pointer(t, _) => *t,
        _ => ValueType::Undef.intern(),
      };

      if let Expression::Empty = a.elements[0] {
        for _ in 0..size.unwrap() {
          for _ in 0..pointee_type.num_words() {
            self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
          }
        }
      }

    //   self.static_data_segment.extend(Value::Nil.to_words());
      let elems_start = self.static_data_segment.len();
      let words = Value::Pointer(Pointer::Static(elems_start)).to_words();

      self.static_data_segment[
        global_id as usize..(global_id as usize) + words.len()
      ].copy_from_slice(&words);

      for _ in 0..size.unwrap() {
        for _ in 0..pointee_type.num_words() {
          self.static_data_segment.extend(Value::Nil.to_words());
        }
      }

      for v in a.elements.clone() {
        let pt = self.visit_expression(v, false);
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



      self.function_compiler.func.chunk.write_pool_opcode(
        Opcode::DefineGlobalArray,
        elems_start as i32,
        line
      );
      self.function_compiler.func.chunk.write(size.unwrap() as u8, line);
      self.function_compiler.func.chunk.write(pointee_type.num_words() as u8, line);
    }
  }
}
