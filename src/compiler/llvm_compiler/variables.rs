
use super::*;

pub struct Variables<'ctx> {
    variables: RefCell<VecDeque<HashMap<String, (PointerValue<'ctx>, UValueType)>>>,
}

impl<'ctx> Variables<'ctx> {
    pub fn new() -> Self {
        let v = Variables {
            variables: VecDeque::new().into(),
        };
        v.begin_scope();
        v
    }

    pub fn begin_scope(&self) {
        self.variables.borrow_mut().push_front(HashMap::new());
    }
    pub fn end_scope(&self) {
        self.variables.borrow_mut().pop_front();
    }

    pub fn get_variable(&self, name: &str) -> Result<(PointerValue<'ctx>, UValueType)> {
        for scope in self.variables.borrow().iter() {
            if let Some(v) = scope.get(name) {
                return Ok(*v);
            }
        }
        Err(anyhow::anyhow!("Variable {} not found", name))
    }

    pub fn set_variable(&self, name: &str, value: (PointerValue<'ctx>, UValueType)) {
        self.variables
            .borrow_mut()
            .front_mut()
            .unwrap()
            .insert(name.into(), value);
    }
}
