use std::{
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};

#[derive(Clone, Eq, PartialOrd, Ord)]
pub struct SharedString(Rc<str>);

impl From<String> for SharedString {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<&str> for SharedString {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl std::hash::Hash for SharedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.0).hash(state);
    }
}

impl PartialEq for SharedString {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Debug for SharedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("{:?}", self.0))
    }
}

impl Deref for SharedString {
    type Target = Rc<str>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for SharedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("{}", self.0))
    }
}
