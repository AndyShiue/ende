#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionCall<'a> {
    pub name: &'a str,
    pub arity: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term<'a> {
    Literal(i32),
    Var(&'a str),
    Infix(Box<Term<'a>>, Operator, Box<Term<'a>>),
    Call(FunctionCall<'a>, &'a [Term<'a>]),
    Scope(Block<'a>),
    If(Box<Term<'a>>, Box<Term<'a>>, Box<Term<'a>>),
    While(Box<Term<'a>>, Block<'a>),
}

#[macro_export]
macro_rules! infix {
    ($lhs: expr, $op: expr, $rhs: expr) => {
        $crate::ast::Term::Infix(Box::new($lhs), $op, Box::new($rhs))
    };
}

// I used to want to provide more useful macros, but I encountered wierd bugs and finally gave up.

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Statement<'a> {
    TermSemicolon(Term<'a>),
    Let(&'a str, Term<'a>),
    LetMut(&'a str, Term<'a>),
    Mutate(&'a str, Term<'a>),
    Extern(&'a str, u32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block<'a> {
    pub stmts: &'a [Statement<'a>],
    pub end: Box<Term<'a>>,
}
