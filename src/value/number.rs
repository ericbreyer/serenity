use std::{ops, fmt::Debug};

#[derive(Clone, Copy)]
pub enum Number {
    Float(f64),
    Integer(i64)
}

impl Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(arg0) => write!(f, "{}f", arg0),
            Self::Integer(arg0) => write!(f, "{}", arg0),
        }
    }
}

impl ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 + n2),
            (Number::Float(n1), Number::Integer(n2)) => Number::Float(n1 + n2 as f64),
            (Number::Integer(n1), Number::Float(n2)) => Number::Float(n1 as f64 + n2),
            (Number::Integer(n1), Number::Integer(n2)) => Number::Integer(n1 + n2),
        }
    }
}

impl ops::Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 - n2),
            (Number::Float(n1), Number::Integer(n2)) => Number::Float(n1 - n2 as f64),
            (Number::Integer(n1), Number::Float(n2)) => Number::Float(n1 as f64 - n2),
            (Number::Integer(n1), Number::Integer(n2)) => Number::Integer(n1 - n2),
        }
    }
}

impl ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 * n2),
            (Number::Float(n1), Number::Integer(n2)) => Number::Float(n1 * n2 as f64),
            (Number::Integer(n1), Number::Float(n2)) => Number::Float(n1 as f64 * n2),
            (Number::Integer(n1), Number::Integer(n2)) => Number::Integer(n1 * n2),
        }
    }
}

impl ops::Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 / n2),
            (Number::Float(n1), Number::Integer(n2)) => Number::Float(n1 / n2 as f64),
            (Number::Integer(n1), Number::Float(n2)) => Number::Float(n1 as f64 / n2),
            (Number::Integer(n1), Number::Integer(n2)) => Number::Integer(n1 / n2),
        }
    }
}

impl ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Float(f) => Number::Float(-f),
            Number::Integer(i) => Number::Integer(-i),
        }    
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Float(n1), Number::Float(n2)) => n1.partial_cmp(n2),
            (Number::Float(n1), Number::Integer(n2)) => n1.partial_cmp(&(*n2 as f64)),
            (Number::Integer(n1), Number::Float(n2)) => (*n1 as f64).partial_cmp(n2),
            (Number::Integer(n1), Number::Integer(n2)) => n1.partial_cmp(n2),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Float(n1), Number::Float(n2)) => n1 == n2,
            (Number::Float(n1), Number::Integer(n2)) => n1 == &(*n2 as f64),
            (Number::Integer(n1), Number::Float(n2)) => (*n1 as f64) == *n2,
            (Number::Integer(n1), Number::Integer(n2)) => n1 == n2,
        }
    }
}