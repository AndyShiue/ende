#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    pub name: String,
    pub arity: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    Literal(i32),
    Var(String),
    Infix(Box<Term>, Operator, Box<Term>),
    Call(FunctionCall, Vec<Term>),
    Scope(Block),
    If(Box<Term>, Box<Term>, Box<Term>),
    While(Box<Term>, Block),
}

#[macro_export]
macro_rules! infix {
    ($lhs: expr, $op: expr, $rhs: expr) => {
        $crate::ast::Term::Infix(Box::new($lhs), $op, Box::new($rhs))
    };
}

// I used to want to provide more useful macros, but I encountered wierd bugs and finally gave up.

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Statement {
    TermSemicolon(Term),
    Let(String, Term),
    LetMut(String, Term),
    Mutate(String, Term),
    Extern(String, u32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub end: Box<Term>,
}
