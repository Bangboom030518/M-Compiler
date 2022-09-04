use lexer::State;
use pest::iterators::{Pairs, Pair};
use crate::Rule;

trait Node {}

#[derive(Debug)]
pub struct Program {
    body: Vec<Statement>,
}

impl Node for Program {}

#[derive(Debug)]
enum Statement {
    Expression(Expression),
    Declaration(Declaration)
}

#[derive(Debug)]
enum Declaration {
    Import(Import),
    Variable(Variable)
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
    Float(f64)
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
    right: Box<Expression>
}

#[derive(Debug)]
enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression)
}

impl Expression {
    // fn eval();
}

pub fn parse(pairs: Pairs<Rule>) -> Program {
    Program {
        body: Vec::new()
    }
}

fn parse_body(pairs: Pairs<Rule>) -> Vec<Statement> {
    pairs.map(|pair| {
        match pair.as_rule() {
            Rule::expression => Statement::Expression(parse_expression(pair)),
            Rule::declaration => Statement::Declaration(parse_declaration(pair)),
            _ => unreachable!()
        }
    }).collect::<Vec<Statement>>()
}

fn parse_expression(pair: Pair<Rule>) -> Expression {
    let pairs = pair.into_inner();
    unimplemented!()
}

fn parse_declaration(pair: Pair<Rule>) -> Declaration {
    let pairs = pair.into_inner();
    unimplemented!()
}
