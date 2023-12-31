import std.iterator

interface ToString
    fn to_string = (self Ref<Self>) std.String

interface Token: ToString

type Const = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "const"

type Function = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "fn"

type Type = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "type"

type If = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "if"

type Else = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "else"

type Let = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "let"

type Return = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "return"

type Newline = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "\n"

type Exponent = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "**"

type Union = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "union"

type Struct = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "struct"

type Assignment = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "="

type Plus = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "+"

type Minus = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "-"

type Multiply = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "*"

type Divide = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "/"

type Indent = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "   "

type Dot = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "."

type Remainder = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "%"

type Equal = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "="

type At = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "@"

type NotEqual = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "!="

type GreaterThan = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            ">"

type LessThan = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "<"

type GreaterThanOrEqual = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            ">="

type LessThanOrEqual = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "<="

type Bang = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "!"

type OpenParen = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "("

type CloseParen = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            ")"

type Comma = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            ","

type String = struct
    string std.String

    extend Token

    extend std.ToString
        fn to_string = (self)
            "\"" + self.string + "\""

type Char = struct
    char Char

    extend Token

    extend std.ToString
        fn to_string = (self)
            "'" + self.char + "'"

type Illegal = struct
    extend Token

    extend std.ToString
        fn to_string = (self)
            "<illegal>"


type Token = 
macro_rules! define_token_enums {
    ($($variants:ident),*) => {
        #[derive(Clone, Debug, Default, PartialEq)]
        pub enum Token {
            $($variants),*,
            #[default]
            Illegal,
            String(String),
            Char(char),
            Ident(String),
            Comment(String),
            Integer(u128),
            Float(f64),
        }

        #[derive(Clone, Copy, Debug, PartialEq)]
        pub enum TokenType {
            $($variants),*,
            String,
            Char,
            Ident,
            Comment,
            Integer,
            Float,
            Eoi,
        }
    };
}

define_token_enums!(
    Const,
    Function,
    Type,
    If,
    Else,
    Let,
    Return,
    Newline,
    Exponent,
    Union,
    Struct,
    Assignment,
    Plus,
    Minus,
    Multiply,
    Divide,
    Indent,
    Dot,
    Remainder,
    Equal,
    At,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Bang,
    OpenParen,
    CloseParen,
    Comma
);
