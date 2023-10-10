use crate::internal::prelude::*;

#[derive(Debug, Default)]
pub struct Scope {
    pub parent: Option<Id>,
    pub declarations: HashMap<Identifier, top_level::DeclarationKind>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Debug)]
pub struct Cache {
    scopes: Vec<Scope>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    // TODO: ?
    pub fn root_scope(&self) -> Id {
        Id(0)
    }

    pub fn create_scope(
        &mut self,
        parent: Id,
    ) -> Id {
        let id = Id(self.scopes.len());
        self.scopes.push(Scope {
            parent: Some(parent),
            declarations: HashMap::new(),
        });
        id
    }

    // TODO: index operator instead?
    pub fn get(&mut self, Id(id): Id) -> &mut Scope {
        &mut self.scopes[id]
    }
}
