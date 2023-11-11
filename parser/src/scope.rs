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
        &self.cache[self.root]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cache {
    scopes: Vec<Scope>,
}

impl Cache {
    pub const ROOT_SCOPE: Id = Id(0);

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
        let scope = &self[scope];
        scope.declarations
            .get(&ident)
            .or_else(|| self.lookup(scope.parent?, ident))
    }

    pub fn create_scope(&mut self, parent: Id) -> Id {
        let id = Id(self.scopes.len());
        self.scopes.push(Scope {
            parent: Some(parent),
            declarations: HashMap::new(),
        });
        id
    }
}

impl std::ops::Index<Id> for Cache {
    type Output = Scope;

    fn index(&self, Id(id): Id) -> &Self::Output {
        &self.scopes[id]
    }
}

impl std::ops::IndexMut<Id> for Cache {
    fn index_mut(&mut self, Id(id): Id) -> &mut Self::Output {
        &mut self.scopes[id]
    }
}
