use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;

use ast::*;
use codegen::Map;

pub trait WithTag<Tag: Clone> {
    type Tagged: Tagged<Tag>;
    type Env;
    // If tags are types, the meaning of `tag` would be to type check.
    fn tag(&self, env: Self::Env) -> Result<Self::Tagged, Vec<String>>;
}

pub trait Tagged<Tag: Clone>: Sized {
    type Untagged: WithTag<Tag, Tagged=Self>;
    fn get_tag(&self) -> Tag;
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
                    string.push_str(&*format!("{}", arg_ty));
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
    pub name: String,
    pub args_tags: Vec<Tag>,
    pub ret_tag: Tag
}

impl WithTag<Type> for FunctionCall {
    type Tagged = TaggedFunctionCall<Type>;
    type Env = Map<Type>;
    fn tag(&self, env: Self::Env) -> Result<Self::Tagged, Vec<String>> {
        let ref name = self.name;
        let func_ty =
            try!(env.get(name).ok_or(vec![format!("Function {} is undeclared.", name)]));
        match *func_ty {
            Type::FunctionTy(ref args_types, ref ret_ty) => {
                Ok(
                    TaggedFunctionCall {
                        name: name.clone(),
                        args_tags: args_types.clone(),
                        ret_tag: *ret_ty.clone(),
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
    fn get_tag(&self) -> Type {
        Type::FunctionTy(self.clone().args_tags, Box::new(self.clone().ret_tag))
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
}

impl WithTag<Type> for Term {
    type Tagged = TaggedTerm<Type>;
    type Env = Map<Type>;
    fn tag(&self, env: Self::Env) -> Result<Self::Tagged, Vec<String>> {
        use self::Type::*;
        match *self {
            Term::Literal(i) => Ok(TaggedTerm::Literal(I32Ty, i)),
            Term::Var(ref str) => match env.get(&str.clone()) {
                Some(ty) => Ok(TaggedTerm::Var(ty.clone(), str.clone())),
                None => Err(vec![format!("Undeclared variable {}.", str.clone())]),
            },
            Term::Infix(ref left, ref op, ref right) => {
                let cloned_left = left.clone();
                let cloned_right = right.clone();
                let tagged_left = try!(cloned_left.tag(env.clone()));
                let tagged_right = try!(cloned_right.tag(env));
                let left_ty = tagged_left.get_tag();
                let right_ty = tagged_right.get_tag();
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
            Term::Call(ref func, ref args) => {
                let typed_func = try!(func.tag(env.clone()));
                let expected_arity = typed_func.args_tags.len();
                let actual_arity = args.len();
                if expected_arity == actual_arity {
                    let pairs = typed_func.args_tags
                                          .iter()
                                          .zip(args);
                    let mut has_error = false;
                    let mut tagged_args = Vec::new();
                    let mut errors = Vec::new();
                    for (expected, actual) in pairs {
                        let expected_ty = expected.clone();
                        let tagged_arg = try!(actual.tag(env.clone()));
                        if !has_error {
                            tagged_args.push(tagged_arg.clone());
                        }
                        let actual_ty = tagged_arg.get_tag();
                        if expected_ty != actual_ty {
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
                            typed_func.ret_tag, try!(func.tag(env)), tagged_args.clone()
                        ))
                    } else {
                        Err(errors)
                    }
                } else {
                    Err(
                        vec![
                            format!("Function {} expects {} arguments, but only {} are provided.",
                                    func.name, expected_arity, actual_arity)
                        ]
                    )
                }
            }
            Term::Scope(ref block) => {
                let tagged_block = try!(block.tag(env));
                let ty = tagged_block.get_tag();
                Ok(TaggedTerm::Scope(ty, tagged_block))
            }
            _ => unimplemented!()
        }
    }
}

// TODO: write a procedural macro for tagged whatever.
impl Tagged<Type> for TaggedTerm<Type> {
    type Untagged = Term;
    fn get_tag(&self) -> Type {
        use self::TaggedTerm::*;
        match *self {
            Literal(ref tag, _) => tag.clone(),
            Var(ref tag, _) => tag.clone(),
            Infix(ref tag, _, _, _) => tag.clone(),
            Call(ref tag, _, _) => tag.clone(),
            Scope(ref tag, _) => tag.clone(),
            If(ref tag, _, _, _) => tag.clone(),
            While(ref tag, _, _) => tag.clone(),
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
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TaggedStatement<Tag> {
    TermSemicolon(Tag, TaggedTerm<Tag>),
    Let(Tag, String, TaggedTerm<Tag>),
    LetMut(Tag, String, TaggedTerm<Tag>),
    Mutate(Tag, String, TaggedTerm<Tag>),
    Extern(Tag, String, Vec<Type>, Type),
}

impl WithTag<Type> for Statement {
    type Tagged = TaggedStatement<Type>;
    type Env = Map<Type>;
    fn tag(&self, mut env: Self::Env) -> Result<Self::Tagged, Vec<String>> {
        use self::Type::*;
        match *self {
            Statement::TermSemicolon(ref term) => {
                let unit_enum = Enumeration {
                    name: "Unit".to_string(),
                    variants: vec!["unit".to_string()]
                };
                let tagged_term = try!(term.tag(env.clone()));
                Ok(TaggedStatement::TermSemicolon(Enum(unit_enum), tagged_term))
            }
            Statement::Let(ref name, ref term) => {
                let tagged_term = try!(term.tag(env.clone()));
                Ok(TaggedStatement::Let(Forbidden, name.clone(), tagged_term))
            }
            Statement::LetMut(ref name, ref term) => {
                let tagged_term = try!(term.tag(env.clone()));
                Ok(TaggedStatement::LetMut(Forbidden, name.clone(), tagged_term))
            }
            Statement::Mutate(ref name, ref term) => {
                let tagged_term = try!(term.tag(env.clone()));
                Ok(TaggedStatement::Mutate(Forbidden, name.clone(), tagged_term))
            }
            Statement::Extern(ref name, ref args_types, ref ret_type) => {
                env.insert(name.clone(),
                           FunctionTy(args_types.clone(), Box::new(ret_type.clone()))
                          );
                Ok(TaggedStatement::Extern(
                    Forbidden, name.clone(), args_types.clone(), ret_type.clone()
                ))
            }
        }
    }

}

impl Tagged<Type> for TaggedStatement<Type> {
    type Untagged = Statement;
    fn get_tag(&self) -> Type {
        unimplemented!()
    }
    fn untag(&self) -> Statement {
        unimplemented!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TaggedBlock<Tag> {
    pub stmts: Vec<TaggedStatement<Tag>>,
    pub end: Box<TaggedTerm<Tag>>,
}

impl WithTag<Type> for Block {
    type Tagged = TaggedBlock<Type>;
    type Env = Map<Type>;
    fn tag(&self, env: Self::Env) -> Result<Self::Tagged, Vec<String>> {
        let mut tagged_stmts = Vec::new();
        for stmt in &self.stmts {
            let tagged_stmt = try!(stmt.tag(env.clone()));
            tagged_stmts.push(tagged_stmt);
        }
        Ok(
            TaggedBlock {
                stmts: tagged_stmts,
                end: Box::new(try!(self.end.tag(env))),
            }
        )
    }
}

impl Tagged<Type> for TaggedBlock<Type> {
    type Untagged = Block;
    fn get_tag(&self) -> Type {
        self.end.get_tag()
    }
    fn untag(&self) -> Block {
        Block {
            stmts: self.stmts.iter().map(TaggedStatement::untag).collect(),
            end: Box::new(self.end.untag()),
        }
    }
}