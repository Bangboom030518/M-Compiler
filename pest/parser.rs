// TODO: refactor into seperate modules

use crate::Rule;
use pest::iterators::{Pair as PestPair, Pairs as PestPairs};

type Pairs<'a> = PestPairs<'a, Rule>;
type Pair<'a> = PestPair<'a, Rule>;

#[derive(Debug)]
pub struct Program {
    body: Vec<Statement>,
}

impl<'a> From<Pairs<'a>> for Program {
    fn from(pairs: Pairs<'a>) -> Self {
        let body = pairs
            .filter_map(|pair| match pair.as_rule() {
                Rule::statement => Some(Statement::from(pair)),
                Rule::EOI => None,
                _ => unreachable!(
                    "Expected only statements at top level. Found '{:?}'",
                    pair.as_rule()
                ),
            })
            .collect::<Vec<Statement>>();

        Self { body }
    }
}

#[derive(Debug)]
enum Statement {
    Expression(Expression),
    Declaration(Declaration),
}

impl<'a> From<Pair<'a>> for Statement {
    fn from(pair: Pair<'a>) -> Self {
        let pair = expect_single_child(pair);
        match pair.as_rule() {
            Rule::expression => Statement::Expression(Expression::from(pair)),
            Rule::declaration => Statement::Declaration(Declaration::from(pair)),
            rule => unreachable!(
                "Statement can only be an expression or declaration. Found '{:?}'",
                rule
            ),
        }
    }
}

#[derive(Debug)]
enum Declaration {
    Import(Import),
    Variable(Variable),
}

impl<'a> From<Pair<'a>> for Declaration {
    fn from(pair: Pair<'a>) -> Self {
        let pairs = pair.into_inner();
        unimplemented!("Declaration Parsing")
    }
}

#[derive(Debug)]
struct Import {
    path: String,
    namespaces: Vec<String>,
}

#[derive(Debug)]
struct Variable {
    kind: VariableKind,
    value: Expression,
}

#[derive(Debug)]
enum VariableKind {
    Static,
    Let,
    Const,
    Var,
}

#[derive(Debug)]
enum Literal {
    String(String),
    Integer(isize),
    Char(char),
    Float(f64),
}

#[derive(Debug)]
enum UnaryOperator {
    LogicalNOT,
    Negate,
}

#[derive(Debug)]
enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponent,
    Modulo,
    LogicalOR,
    LogicalAND,
}

#[derive(Debug)]
struct UnaryExpression {
    operator: UnaryOperator,
    operand: Box<Expression>,
}

#[derive(Debug)]
struct BinaryExpression {
    operator: BinaryOperator,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug)]
enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Identifier(String),
}



impl<'a> From<Pair<'a>> for Expression {
    fn from(pair: Pair<'a>) -> Self {
        
        let pair = expect_single_child(pair);
        match pair.as_rule() {
        Rule::binary_expression => unimplemented!(),
        Rule::unary_expression => unimplemented!(),
        Rule::literal => unimplemented!(),
        Rule::group => unimplemented!(),
        Rule::identifier => unimplemented!(),
        // TODO: write function to automate this
        rule => unreachable!("Expression can only be a 'binary_expression', 'unary_expression', 'literal', 'group' or 'identifier'. Found '{:?}'", rule)
    }
    }
}

pub fn parse(pairs: Pairs) -> Program {
    Program::from(pairs)
}

fn expect_single_child(pair: Pair) -> Pair {
    // TODO: is clone the best?
    let mut pairs = pair.clone().into_inner();
    if let Some(pair) = pairs.next() {
        if pairs.next() == None {
            Some(pair)
        } else {
            None
        }
    } else {
        None
    }
    .unwrap_or_else(|| panic!("Rule '{:?}' should have exactly 1 child", pair.as_rule()))
}
