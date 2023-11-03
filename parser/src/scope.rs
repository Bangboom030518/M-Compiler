use crate::internal::prelude::*;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Scope {
    pub parent: Option<Id>,
    pub declarations: HashMap<Ident, top_level::DeclarationKind>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Debug)]
pub struct File {
    pub root: Id,
    pub cache: Cache,
}

impl File {
    #[must_use]
    pub fn root(&self) -> &Scope {
        self.cache.get(self.root)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cache {
    scopes: Vec<Scope>,
}

impl Cache {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    #[must_use]
    pub fn lookup(
        &self,
        scope: Id,
        ident: Ident,
    ) -> Option<&top_level::DeclarationKind> {
        let scope = self.get(scope);
        scope.declarations
            .get(&ident)
            .or_else(|| self.lookup(scope.parent?, ident))
    }

    // TODO: ?
    #[must_use]
    pub const fn root_scope(&self) -> Id {
        Id(0)
    }

    pub fn create_scope(&mut self, parent: Id) -> Id {
        let id = Id(self.scopes.len());
        self.scopes.push(Scope {
            parent: Some(parent),
            declarations: HashMap::new(),
        });
        id
    }

    #[must_use]
    pub fn get(&self, Id(id): Id) -> &Scope {
        &self.scopes[id]
    }

    // TODO: index operator instead?
    pub fn get_mut(&mut self, Id(id): Id) -> &mut Scope {
        &mut self.scopes[id]
    }
}
