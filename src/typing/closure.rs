use super::*;

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Closure {
    pub args: Box<[UValueType]>,
    pub upvals: Box<[UValueType]>,
    pub ret: UValueType,
    pub generics: BTreeMap<SharedString, UValueType>,
}

impl Closure {
    pub fn new(
        args: Box<[UValueType]>,
        upvals: Box<[UValueType]>,
        ret: UValueType,
        generics: impl Into<Option<BTreeMap<SharedString, UValueType>>>,
    ) -> Self {
        Self {
            args,
            upvals,
            ret,
            generics: generics.into().unwrap_or_default(),
        }
    }
}
