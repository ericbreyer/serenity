use std::fmt::Debug;

#[derive(Clone, PartialEq, Copy)]
pub enum Value {
    Integer(i64),
    UInteger(u64),
    Float(f64),
    Char(u8),
    Bool(bool),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{i}"),
            Value::UInteger(i) => write!(f, "{i}u"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Char(c) => write!(f, "{}", *c as char),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_value() {
        let val = Value::Integer(42);
        assert_eq!(val, Value::Integer(42));
    }

    #[test]
    fn test_unsigned_integer_value() {
        let val = Value::UInteger(42);
        assert_eq!(val, Value::UInteger(42));
    }

    #[test]
    fn test_float_value() {
        let val = Value::Float(3.14);
        assert_eq!(val, Value::Float(3.14));
    }

    #[test]
    fn test_char_value() {
        let val = Value::Char(b'A');
        assert_eq!(val, Value::Char(b'A'));
    }

    #[test]
    fn test_bool_true_value() {
        let val = Value::Bool(true);
        assert_eq!(val, Value::Bool(true));
    }

    #[test]
    fn test_bool_false_value() {
        let val = Value::Bool(false);
        assert_eq!(val, Value::Bool(false));
    }

    #[test]
    fn test_integer_negative() {
        let val = Value::Integer(-42);
        assert_eq!(val, Value::Integer(-42));
    }

    #[test]
    fn test_float_negative() {
        let val = Value::Float(-3.14);
        assert_eq!(val, Value::Float(-3.14));
    }

    #[test]
    fn test_integer_zero() {
        let val = Value::Integer(0);
        assert_eq!(val, Value::Integer(0));
    }

    #[test]
    fn test_unsigned_integer_zero() {
        let val = Value::UInteger(0);
        assert_eq!(val, Value::UInteger(0));
    }

    #[test]
    fn test_float_zero() {
        let val = Value::Float(0.0);
        assert_eq!(val, Value::Float(0.0));
    }

    #[test]
    fn test_large_integer() {
        let val = Value::Integer(i64::MAX);
        assert_eq!(val, Value::Integer(i64::MAX));
    }

    #[test]
    fn test_large_unsigned_integer() {
        let val = Value::UInteger(u64::MAX);
        assert_eq!(val, Value::UInteger(u64::MAX));
    }

    #[test]
    fn test_special_chars() {
        let test_chars = vec![
            (b'\n', '\n'),
            (b'\t', '\t'),
            (b'\r', '\r'),
            (b' ', ' '),
            (b'0', '0'),
            (b'z', 'z'),
            (b'Z', 'Z'),
        ];

        for (byte_val, _char) in test_chars {
            let val = Value::Char(byte_val);
            assert_eq!(val, Value::Char(byte_val));
        }
    }

    #[test]
    fn test_value_inequality() {
        let int_val = Value::Integer(5);
        let float_val = Value::Float(5.0);
        assert_ne!(int_val, float_val);
    }

    #[test]
    fn test_value_clone() {
        let original = Value::Integer(42);
        let cloned = original;
        assert_eq!(original, cloned);
    }

    #[test]
    fn test_value_copy() {
        let val1 = Value::Bool(true);
        let val2 = val1;
        assert_eq!(val1, val2);
    }

    #[test]
    fn test_integer_debug_format() {
        let val = Value::Integer(42);
        let debug_str = format!("{:?}", val);
        assert_eq!(debug_str, "42");
    }

    #[test]
    fn test_unsigned_integer_debug_format() {
        let val = Value::UInteger(42);
        let debug_str = format!("{:?}", val);
        assert_eq!(debug_str, "42u");
    }

    #[test]
    fn test_float_debug_format() {
        let val = Value::Float(3.1);
        let debug_str = format!("{:?}", val);
        assert!(debug_str.contains("3.1"));
    }

    #[test]
    fn test_char_debug_format() {
        let val = Value::Char(b'A');
        let debug_str = format!("{:?}", val);
        assert_eq!(debug_str, "A");
    }

    #[test]
    fn test_bool_debug_format() {
        let true_val = Value::Bool(true);
        let false_val = Value::Bool(false);
        assert_eq!(format!("{:?}", true_val), "true");
        assert_eq!(format!("{:?}", false_val), "false");
    }

    #[test]
    fn test_extreme_values() {
        let min_int = Value::Integer(i64::MIN);
        let max_int = Value::Integer(i64::MAX);
        let max_uint = Value::UInteger(u64::MAX);

        assert_eq!(min_int, Value::Integer(i64::MIN));
        assert_eq!(max_int, Value::Integer(i64::MAX));
        assert_eq!(max_uint, Value::UInteger(u64::MAX));
    }

    #[test]
    fn test_float_precision() {
        let val1 = Value::Float(0.1 + 0.2);
        let val2 = Value::Float(0.30000000000000004);
        // Note: floating point comparison might be tricky due to precision
        // This test just ensures values can be created and compared
        assert_eq!(val1, val2);
    }

    #[test]
    fn test_multiple_values_different_types() {
        let int_val = Value::Integer(5);
        let uint_val = Value::UInteger(5);
        let float_val = Value::Float(5.0);
        let bool_val = Value::Bool(true);
        let _char_val = Value::Char(b'5');

        // All are distinct types
        assert_ne!(int_val, uint_val);
        assert_ne!(int_val, float_val);
        assert_ne!(bool_val, Value::Bool(false));
    }
}
