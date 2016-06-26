use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;
use std::collections::HashMap;

use ast::*;
use codegen::Map;

pub trait TypeCheck {
    type Typed;
    fn type_check(&self, env: &mut Map<Type>) -> Result<Self::Typed, Vec<String>>;
}

pub trait Tagged<Tag: Clone> {
    type Untagged;
    fn get_tag(&self) -> Box<Tag>;
    fn untag(&self) -> Self::Untagged;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Forbidden,
    I32Ty,
    Enum(Enumeration),
    FunctionTy(Vec<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::Type::*;
        let ty_name = match *self {
            Forbidden => unreachable!(),
            Enum(ref en) => format!("{}", en),
            I32Ty => format!("I32"),
            FunctionTy(ref args_types, ref ret_type) => {
                let mut string = String::new();
                for arg_ty in args_types {
                    string.push_str(&format!("{}", arg_ty));
                    string.push_str(", ")
                }
                format!("({}) -> {}", string, ret_type)
            }
        };
        write!(f, "{}", ty_name)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Enumeration {
    pub name: String,
    variants: Vec<String>,
}

impl Display for Enumeration {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TaggedFunctionCall<Tag> {
    pub tag: Tag,
    pub name: String,
}

impl TypeCheck for TaggedFunctionCall<Position> {
    type Typed = TaggedFunctionCall<Type>;
    fn type_check(&self, env: &mut Map<Type>) -> Result<Self::Typed, Vec<String>> {
        let untagged = self.untag();
        let ref name = untagged.name;
        let func_ty =
            try!(env.get(name).ok_or(vec![format!("Function {} is undeclared.", name)]));
        match func_ty.clone() {
            ty @ Type::FunctionTy(..) => {
                Ok(
                    TaggedFunctionCall {
                        tag: ty,
                        name: name.clone(),
                    }
                )
            }
            _ => Err(
                vec![format!("{} is called as a function, but it has type {}", name, func_ty)]
            ),
        }
    }
}

impl Tagged<Type> for TaggedFunctionCall<Type> {
    type Untagged = FunctionCall;
    fn get_tag(&self) -> Box<Type> {
        Box::new(self.tag.clone())
    }
    fn untag(&self) -> FunctionCall {
        FunctionCall {
            name: self.clone().name,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TaggedTerm<Tag> {
    Literal(Tag, i32),
    Var(Tag, String),
    Infix(Tag, Box<TaggedTerm<Tag>>, Operator, Box<TaggedTerm<Tag>>),
    Call(Tag, TaggedFunctionCall<Tag>, Vec<TaggedTerm<Tag>>),
    Scope(Tag, TaggedBlock<Tag>),
    If(Tag, Box<TaggedTerm<Tag>>, Box<TaggedTerm<Tag>>, Box<TaggedTerm<Tag>>),
    While(Tag, Box<TaggedTerm<Tag>>, TaggedBlock<Tag>),
    Stmt(Box<TaggedStatement<Tag>>),
}

impl TypeCheck for TaggedTerm<Position> {
    type Typed = TaggedTerm<Type>;
    fn type_check(&self, env: &mut Map<Type>) -> Result<Self::Typed, Vec<String>> {
        use self::TaggedTerm::*;
        use self::Type::*;
        match *self {
            Literal(_, i) => Ok(TaggedTerm::Literal(I32Ty, i)),
            Var(_, ref str) => match env.get(&str.clone()) {
                Some(ty) => Ok(TaggedTerm::Var(ty.clone(), str.clone())),
                None => Err(vec![format!("Undeclared variable {}.", str.clone())]),
            },
            Infix(_, ref left, ref op, ref right) => {
                let tagged_left: TaggedTerm<Type> = try!(left.type_check(&mut env.clone()));
                let tagged_right: TaggedTerm<Type> = try!(right.type_check(env));
                let left_ty = *tagged_left.get_tag();
                let right_ty = *tagged_right.get_tag();
                if left_ty == I32Ty && right_ty == I32Ty {
                    Ok(TaggedTerm::Infix(
                        left_ty, Box::new(tagged_left), op.clone(), Box::new(tagged_right)
                    ))
                } else {
                    return Err(
                        vec![
                            format!("The left-hand-side of {} has type {}, \
                                    but the right-hand-side of it has type {}.",
                                    op, left_ty, right_ty)
                        ]
                    );
                }
            }
            Call(_, ref func, ref args) => {
                let typed_func = try!(func.type_check(&mut env.clone()));
                let (expected_args_types, expected_ret_ty) =
                    if let Type::FunctionTy(args_types, ret_ty) = *typed_func.get_tag() {
                        (args_types, *ret_ty)
                    } else {
                        unreachable!()
                    };
                let expected_arity = expected_args_types.len();
                let actual_arity = args.len();
                if expected_arity == actual_arity {
                    let pairs = expected_args_types.iter().zip(args);
                    let mut has_error = false;
                    let mut tagged_args = Vec::new();
                    let mut errors = Vec::new();
                    for (expected, actual) in pairs {
                        let expected_ty = expected.clone();
                        let tagged_arg: TaggedTerm<Type> =
                            try!(actual.type_check(&mut env.clone()));
                        if !has_error {
                            tagged_args.push(tagged_arg.clone());
                        }
                        let actual_ty = tagged_arg.get_tag();
                        if expected_ty != *actual_ty {
                            has_error = true;
                            errors.push(
                                format!(
                                    "Expect term of type {}, found term of type {}.",
                                    expected_ty, actual_ty
                                )
                            );
                        }
                    }
                    if errors.len() == 0 {
                        Ok(TaggedTerm::Call(
                            expected_ret_ty, try!(func.type_check(env)), tagged_args.clone()
                        ))
                    } else {
                        Err(errors)
                    }
                } else {
                    Err(
                        vec![
                            format!("Function {} expects {} argument(s), but {} are provided.",
                                    func.name, expected_arity, actual_arity)
                        ]
                    )
                }
            }
            Scope(_, ref block) => {
                let tagged_block = try!(block.type_check(env));
                let ty = tagged_block.get_tag();
                Ok(TaggedTerm::Scope(*ty, tagged_block))
            }
            If(_, ref if_clause, ref then_clause, ref else_clause) => {
                let tagged_if = try!(if_clause.type_check(&mut env.clone()));
                let tagged_then = try!(then_clause.type_check(&mut env.clone()));
                let tagged_else = try!(else_clause.type_check(&mut env.clone()));
                let then_ty = *tagged_then.get_tag().clone();
                let else_ty = *tagged_else.get_tag().clone();
                if then_ty == else_ty {
                    Ok(TaggedTerm::If(
                        then_ty, Box::new(tagged_if), Box::new(tagged_then), Box::new(tagged_else)
                    ))
                } else {
                    Err(
                        vec![
                            format!(
                                "The term of the then part has type {}, \
                                 but that of the else part has type {}.",
                                then_ty, else_ty
                            )
                        ]
                    )
                }
            }
            While(_, ref cond, ref block) => {
                let tagged_cond = try!(cond.type_check(&mut env.clone()));
                let cond_ty = *tagged_cond.get_tag();
                if cond_ty != I32Ty {
                    Err(vec!["The condition of a while loop should be of type I32".to_string()])
                } else {
                    let tagged_block: TaggedBlock<Type> = try!(block.type_check(env));
                    Ok(TaggedTerm::While(
                        *tagged_block.get_tag(), Box::new(tagged_cond), tagged_block
                    ))
                }
            }
            Stmt(ref stmt) => {
                Ok(TaggedTerm::Stmt(Box::new(try!(stmt.type_check(env)))))
            }
        }
    }
}

// TODO: write a procedural macro for tagged whatever.
impl Tagged<Type> for TaggedTerm<Type> {
    type Untagged = Term;
    fn get_tag(&self) -> Box<Type> {
        use self::TaggedTerm::*;
        match *self {
            Literal(ref tag, _) => Box::new(tag.clone()),
            Var(ref tag, _) => Box::new(tag.clone()),
            Infix(ref tag, _, _, _) => Box::new(tag.clone()),
            Call(ref tag, _, _) => Box::new(tag.clone()),
            Scope(ref tag, _) => Box::new(tag.clone()),
            If(ref tag, _, _, _) => Box::new(tag.clone()),
            While(ref tag, _, _) => Box::new(tag.clone()),
            Stmt(ref block) => {
                let unit_enum = Enumeration {
                    name: "Unit".to_string(),
                    variants: vec!["unit".to_string()]
                };
                Box::new(Type::Enum(unit_enum))
            }
        }
    }
    fn untag(&self) -> Term {
        match *self {
            TaggedTerm::Literal(_, num) => Term::Literal(num),
            TaggedTerm::Var(_, ref name) => Term::Var(name.clone()),
            TaggedTerm::Infix(_, ref left, op, ref right) =>
                Term::Infix(
                    Box::new(left.untag()), op, Box::new(right.untag())
                ),
            TaggedTerm::Call(_, ref func, ref args) => {
                let args: Vec<Term> = args.iter().map(|arg| arg.untag()).collect();
                Term::Call(func.untag(), args)
            }
            TaggedTerm::Scope(_, ref block) => Term::Scope(block.untag()),
            TaggedTerm::If(_, ref if_clause, ref then_clause, ref else_clause) => {
                Term::If(
                    Box::new(if_clause.untag()),
                    Box::new(then_clause.untag()),
                    Box::new(else_clause.untag())
                )
            }
            TaggedTerm::While(_, ref cond, ref block) => {
                Term::While(Box::new(cond.untag()), block.untag())
            }
            TaggedTerm::Stmt(ref block) => {
                Term::Stmt(Box::new(block.untag()))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TaggedStatement<Tag> {
    TermSemicolon(Tag, TaggedTerm<Tag>),
    Let(Tag, String, TaggedTerm<Tag>),
    LetMut(Tag, String, TaggedTerm<Tag>),
    Mutate(Tag, String, TaggedTerm<Tag>),
    Extern(Tag, String, Type),
}

impl TypeCheck for TaggedStatement<Position> {
    type Typed = TaggedStatement<Type>;
    fn type_check(&self, mut env: &mut Map<Type>) -> Result<Self::Typed, Vec<String>> {
        use self::TaggedStatement::*;
        use self::Type::*;
        match *self {
            TermSemicolon(_, ref term) => {
                let unit_enum = Enumeration {
                    name: "Unit".to_string(),
                    variants: vec!["unit".to_string()]
                };
                let typed_term = try!(term.type_check(&mut env.clone()));
                Ok(TermSemicolon(Enum(unit_enum), typed_term))
            }
            Let(_, ref name, ref term) => {
                let typed_term: TaggedTerm<Type> = try!(term.type_check(&mut env.clone()));
                env.insert(name.clone(), *typed_term.get_tag());
                Ok(Let(Forbidden, name.clone(), typed_term))
            }
            LetMut(_, ref name, ref term) => {
                let typed_term: TaggedTerm<Type> = try!(term.type_check(&mut env.clone()));
                env.insert(name.clone(), *typed_term.get_tag());
                Ok(LetMut(Forbidden, name.clone(), typed_term))
            }
            Mutate(_, ref name, ref term) => {
                let typed_term = try!(term.type_check(&mut env.clone()));
                Ok(Mutate(Forbidden, name.clone(), typed_term))
            }
            Extern(_, ref name, ref ty) => {
                env.insert(name.clone(), ty.clone());
                Ok(Extern(Forbidden, name.clone(), ty.clone()))
            }
        }
    }

}

impl Tagged<Type> for TaggedStatement<Type> {
    type Untagged = Statement;
    fn get_tag(&self) -> Box<Type> {
        use self::TaggedStatement::*;
        match *self {
            TermSemicolon(ref ty, _) => Box::new(ty.clone()),
            Let(ref ty, _, _) => Box::new(ty.clone()),
            LetMut(ref ty, _, _) => Box::new(ty.clone()),
            Mutate(ref ty, _, _) => Box::new(ty.clone()),
            Extern(ref ty, _, _) => Box::new(ty.clone()),
        }
    }
    fn untag(&self) -> Statement {
        match *self {
            TaggedStatement::TermSemicolon(_, ref term) => Statement::TermSemicolon(term.untag()),
            TaggedStatement::Let(_, ref name, ref term) =>
                Statement::Let(name.clone(), term.untag()),
            TaggedStatement::LetMut(_, ref name, ref term) =>
                Statement::LetMut(name.clone(), term.untag()),
            TaggedStatement::Mutate(_, ref name, ref term) =>
                Statement::Mutate(name.clone(), term.untag()),
            TaggedStatement::Extern(_, ref name, ref ty) =>
                Statement::Extern(name.clone(), ty.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TaggedBlock<Tag> {
    pub tag: Tag,
    pub stmts: Vec<TaggedStatement<Tag>>,
    pub end: Box<Option<TaggedTerm<Tag>>>,
}

impl TypeCheck for TaggedBlock<Position> {
    type Typed = TaggedBlock<Type>;
    fn type_check(&self, mut env: &mut Map<Type>) -> Result<Self::Typed, Vec<String>> {
        let mut tagged_stmts = Vec::new();
        for stmt in &self.stmts {
            let tagged_stmt = try!(stmt.type_check(env));
            tagged_stmts.push(tagged_stmt);
        }
        let end = match *self.end {
            Some(ref term) => Some(try!(term.type_check(env))),
            None => None
        };
        let ty = match end.clone() {
            Some(tagged) => tagged.get_tag(),
            None => {
                let unit_enum = Enumeration {
                    name: "Unit".to_string(),
                    variants: vec!["unit".to_string()]
                };
                Box::new(Type::Enum(unit_enum))
            }
        };
        Ok(
            TaggedBlock {
                tag: *ty,
                stmts: tagged_stmts,
                end: Box::new(end),
            }
        )
    }
}

impl Tagged<Type> for TaggedBlock<Type> {
    type Untagged = Block;
    fn get_tag(&self) -> Box<Type> {
        Box::new(self.tag.clone())
    }
    fn untag(&self) -> Block {
        Block {
            stmts: self.stmts.iter().map(TaggedStatement::untag).collect(),
            end: Box::new(self.clone().end.map(|term| term.untag())),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TaggedProgram<Tag> {
    pub tag: Tag,
    pub main: TaggedBlock<Tag>,
}

impl TypeCheck for TaggedProgram<Position> {
    type Typed = TaggedProgram<Type>;
    fn type_check(&self, env: &mut Map<Type>) -> Result<Self::Typed, Vec<String>> {
        Ok(
            TaggedProgram {
                tag: Type::Forbidden,
                main: try!(self.main.type_check(env))
            }
        )
    }
}

impl Tagged<Type> for TaggedProgram<Type> {
    type Untagged = Program;
    fn get_tag(&self) -> Box<Type> {
        Box::new(self.tag.clone())
    }
    fn untag(&self) -> Program {
        Program {
            main: self.main.untag()
        }
    }
}

// All the code below will be removed after compiling actually accept AST with position tags.

impl Tagged<Position> for TaggedFunctionCall<Position> {
    type Untagged = FunctionCall;
    fn get_tag(&self) -> Box<Position> {
        Box::new(self.tag.clone())
    }
    fn untag(&self) -> FunctionCall {
        FunctionCall {
            name: self.clone().name,
        }
    }
}

impl Tagged<Position> for TaggedTerm<Position> {
    type Untagged = Term;
    fn get_tag(&self) -> Box<Position> {
        use self::TaggedTerm::*;
        match *self {
            Literal(ref tag, _) => Box::new(tag.clone()),
            Var(ref tag, _) => Box::new(tag.clone()),
            Infix(ref tag, _, _, _) => Box::new(tag.clone()),
            Call(ref tag, _, _) => Box::new(tag.clone()),
            Scope(ref tag, _) => Box::new(tag.clone()),
            If(ref tag, _, _, _) => Box::new(tag.clone()),
            While(ref tag, _, _) => Box::new(tag.clone()),
            Stmt(ref block) => {
                unreachable!()
            }
        }
    }
    fn untag(&self) -> Term {
        match *self {
            TaggedTerm::Literal(_, num) => Term::Literal(num),
            TaggedTerm::Var(_, ref name) => Term::Var(name.clone()),
            TaggedTerm::Infix(_, ref left, op, ref right) =>
                Term::Infix(
                    Box::new(left.untag()), op, Box::new(right.untag())
                ),
            TaggedTerm::Call(_, ref func, ref args) => {
                let args: Vec<Term> = args.iter().map(|arg| arg.untag()).collect();
                Term::Call(func.untag(), args)
            }
            TaggedTerm::Scope(_, ref block) => Term::Scope(block.untag()),
            TaggedTerm::If(_, ref if_clause, ref then_clause, ref else_clause) => {
                Term::If(
                    Box::new(if_clause.untag()),
                    Box::new(then_clause.untag()),
                    Box::new(else_clause.untag())
                )
            }
            TaggedTerm::While(_, ref cond, ref block) => {
                Term::While(Box::new(cond.untag()), block.untag())
            }
            TaggedTerm::Stmt(ref block) => {
                Term::Stmt(Box::new(block.untag()))
            }
        }
    }
}

impl Tagged<Position> for TaggedStatement<Position> {
    type Untagged = Statement;
    fn get_tag(&self) -> Box<Position> {
        use self::TaggedStatement::*;
        match *self {
            TermSemicolon(ref ty, _) => Box::new(ty.clone()),
            Let(ref ty, _, _) => Box::new(ty.clone()),
            LetMut(ref ty, _, _) => Box::new(ty.clone()),
            Mutate(ref ty, _, _) => Box::new(ty.clone()),
            Extern(ref ty, _, _) => Box::new(ty.clone()),
        }
    }
    fn untag(&self) -> Statement {
        match *self {
            TaggedStatement::TermSemicolon(_, ref term) => Statement::TermSemicolon(term.untag()),
            TaggedStatement::Let(_, ref name, ref term) =>
                Statement::Let(name.clone(), term.untag()),
            TaggedStatement::LetMut(_, ref name, ref term) =>
                Statement::LetMut(name.clone(), term.untag()),
            TaggedStatement::Mutate(_, ref name, ref term) =>
                Statement::Mutate(name.clone(), term.untag()),
            TaggedStatement::Extern(_, ref name, ref ty) =>
                Statement::Extern(name.clone(), ty.clone()),
        }
    }
}

impl Tagged<Position> for TaggedBlock<Position> {
    type Untagged = Block;
    fn get_tag(&self) -> Box<Position> {
        Box::new(self.tag.clone())
    }
    fn untag(&self) -> Block {
        Block {
            stmts: self.stmts.iter().map(TaggedStatement::untag).collect(),
            end: Box::new(self.clone().end.map(|term| term.untag())),
        }
    }
}

impl Tagged<Position> for TaggedProgram<Position> {
    type Untagged = Program;
    fn get_tag(&self) -> Box<Position> {
        Box::new(self.tag.clone())
    }
    fn untag(&self) -> Program {
        Program {
            main: self.main.untag()
        }
    }
}
