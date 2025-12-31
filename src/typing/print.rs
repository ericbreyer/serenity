use std::fmt::Write;

use super::{Closure, Debug, Display, ValueType};

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&match self {
            ValueType::Float => "float".to_owned(),
            ValueType::Integer => "int".to_owned(),
            ValueType::Char => "char".to_owned(),
            ValueType::Bool => "bool".to_owned(),
            ValueType::Nil => "nil".to_owned(),
            ValueType::Closure(Closure {
                args,
                upvals,
                ret,
                generics: _,
            }) => {
                let mut s = String::new();
                s.push_str("fn[");
                for (i, v) in upvals.iter().enumerate() {
                    s.push_str(&v.to_string());
                    if i != upvals.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str("](");
                for (i, v) in args.iter().enumerate() {
                    s.push_str(&v.to_string());
                    if i != args.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") -> ");
                s.push_str(&ret.to_string());
                s
            }
            ValueType::ExternalFn(ret, _) => {
                let mut s = String::new();
                s.push_str("fn");
                s.push_str(" -> ");
                s.push_str(&ret.to_string());
                s
            }
            ValueType::Pointer(t, _) => format!("*{t}"),
            ValueType::LValue(t, _) => format!("&{t}"),
            ValueType::Array(t, s) => {
                if let Some(s) = s {
                    format!("[{t}; {s}]")
                } else {
                    format!("[{t}]")
                }
            }
            ValueType::Struct(st) => {
                let mut s = String::new();
                s.push_str("struct ");
                s.push_str(&st.name);
                s.push_str(" { ");
                let bg = st.fields.borrow();
                for (k, v) in bg.iter() {
                    write!(s, "{k}: {}, ", v.value).unwrap();
                }
                s.push('}');
                s
            }
            ValueType::SelfStructRef(s, _) => format!("struct {s}"),
            ValueType::GenericParam(n, c) => {
                if c.is_empty() {
                    format!("<{n}>")
                } else {
                    format!("<{n}: {c:?}>")
                }
            }
            ValueType::Err => "err".to_owned(),
            ValueType::TypeVar(i) => format!("${i}"),
        })
    }
}

impl Debug for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
