use HsClosureFunc::*;

use ast::*;
use type_check::*;
use std::ffi::CStr;
use std::str::from_utf8;

pub trait FromHaskellRepr {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Self;
}

impl FromHaskellRepr for Position {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Position {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);
        use ast::Statement::*;

        match con_name.as_str() {
            "main:Ast.Position" => {
                Position {
                    start_pos : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)),
                    end_pos : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))
                }
            },
            _ => panic!("from_haskell_repr Position: unrecognized constructor name: {}", con_name)
        }
    }
}
impl<T1: FromHaskellRepr, T2: FromHaskellRepr> FromHaskellRepr for (T1, T2) {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> (T1, T2) {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);
        use ast::Statement::*;

        match con_name.as_str() {
            "ghc-prim:GHC.Tuple.(,)" => {
                    (FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)))
            },
            _ => panic!("from_haskell_repr (T1, T2): unrecognized constructor name: {}", con_name)
        }
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for TaggedProgram<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> TaggedProgram<T1> {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        TaggedProgram {
            tag : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)),
            main : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))
        }
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for Box<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Box<T1> {
        Box::new(FromHaskellRepr::from_haskell_repr(i))
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for Option<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Option<T1> {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);

        match con_name.as_str() {
            "base:GHC.Base.Just" => Some(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))),
            "base:GHC.Base.Nothing" => None,
            _ => panic!("from_haskell_repr Option: unrecognized constructor name: {}", con_name)
        }
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for Vec<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Vec<T1> {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        unsafe fn go<T1: FromHaskellRepr>(closure : *mut StgClosure, mut acc : Vec<T1>) -> Vec<T1> {
            let con_name = get_constructor_desc(closure);

            match con_name.as_str() {
                "ghc-prim:GHC.Types.:" => {
                    let t1 = FromHaskellRepr::from_haskell_repr(get_nth_payload(closure, 0));
                    acc.push(t1);
                    go(_UNTAG_CLOSURE(deRefStgInd(get_nth_payload(closure, 1))), acc)
                },
                "ghc-prim:GHC.Types.[]" => {
                    acc
                },
                _ => panic!("to_rust_types: unrecognized constructor name: {}", con_name)
            }
        };
        go(input_ref, Vec::<T1>::new())
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for TaggedBlock<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> TaggedBlock<T1> {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        TaggedBlock {
            tag : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)),
            stmts : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)),
            end : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))
        }
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for TaggedStatement<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> TaggedStatement<T1> {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);
        use type_check::TaggedStatement::*;

        match con_name.as_str() {
            "main:Ast.TermSemicolon" => TermSemicolon(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.Let" => Let(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))),
            "main:Ast.LetMut" => LetMut(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))),
            "main:Ast.Mutate" => Mutate(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))),
            "main:Ast.Extern" => Extern(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))),
            _ => panic!("from_haskell_repr TaggedStatement: unrecognized constructor name: {}", con_name)
        }
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for TaggedTerm<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> TaggedTerm<T1> {
        use type_check::TaggedTerm::*;
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);

        match con_name.as_str() {
            "main:Ast.Literal" => Literal(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.Var" => Var(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.Infix" => Infix(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 3))),
            "main:Ast.Call" => Call(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))),
            "main:Ast.Scope" => Scope(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.If" => If(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 3))),
            "main:Ast.While" => While(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2))),
            _ => panic!("from_haskell_repr Term: unrecognized constructor name: {}", con_name)
        }
    }
}

impl<T1: FromHaskellRepr> FromHaskellRepr for TaggedFunctionCall<T1> {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> TaggedFunctionCall<T1> {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        TaggedFunctionCall {
            tag : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)),
            name : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)),
        }
    }
}

impl FromHaskellRepr for Program {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Program {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        Program {
            main : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))
        }
    }
}

impl FromHaskellRepr for Block {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Block {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        Block {
            stmts : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)),
            end : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))
        }
    }
}
impl FromHaskellRepr for Operator {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Operator {
        let input = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input);
        match name.as_str() {
            "main:Ast.Add" => Operator::Add,
            "main:Ast.Sub" => Operator::Sub,
            "main:Ast.Mul" => Operator::Mul,
            "main:Ast.Div" => Operator::Div,
            _ => panic!("from_haskell_repr Operator: unrecognized constructor name: {}", name)
        }
    }
}
impl FromHaskellRepr for Term {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Term {
        use ast::Term::*;
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);

        match con_name.as_str() {
            "main:Ast.Literal" => Literal(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))),
            "main:Ast.Var" => Var(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))),
            "main:Ast.Infix" => Infix(Box::new(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1)), Box::new(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2)))),
            "main:Ast.Call" => Call(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.Scope" => Scope(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))),
            "main:Ast.If" => If(Box::new(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))), Box::new(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))), Box::new(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 2)))),
            "main:Ast.While" => While(Box::new(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            _ => panic!("from_haskell_repr Term: unrecognized constructor name: {}", con_name)
        }
    }
}

impl FromHaskellRepr for u32 {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> u32 {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input_ref);

        match name.as_str() {
            "ghc-prim:GHC.Types.W#" => get_nth_payload(input_ref, 0) as u32,
            _ => panic!("from_haskell_repr u32: unrecognized constructor name: {}", name)
        }
    }
}

impl FromHaskellRepr for i32 {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> i32 {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input_ref);

        match name.as_str() {
            "ghc-prim:GHC.Types.I#" => get_nth_payload(input_ref, 0) as i32,
            _ => panic!("from_haskell_repr i32: unrecognized constructor name: {}", name)
        }
    }
}

impl FromHaskellRepr for String {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> String {
        let vec_char : Vec<char> = FromHaskellRepr::from_haskell_repr(i);
        vec_char.iter().cloned().collect()
    }
}

impl FromHaskellRepr for char {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> char {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input_ref);

        match name.as_str() {
            "ghc-prim:GHC.Types.C#" => {
                get_nth_payload(input_ref, 0) as u8 as char
            },
            _ => panic!("from_haskell_repr char: unrecognized constructor name: {}", name)
        }
    }
}

impl FromHaskellRepr for Type {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Type {
        let input = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input);

        use type_check::Type::*;
        match name.as_str() {
            "main:Ast.Forbidden" => Forbidden,
            "main:Ast.I32Ty" => I32Ty,
            "main:Ast.FunctionTy" => FunctionTy(FromHaskellRepr::from_haskell_repr(get_nth_payload(input, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input, 1))),
            _ => panic!("from_haskell_repr Type: unrecognized constructor name: {}", name)
        }
    }
}

impl FromHaskellRepr for Statement {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> Statement {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);
        use ast::Statement::*;

        match con_name.as_str() {
            "main:Ast.TermSemicolon" => TermSemicolon(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))),
            "main:Ast.Let" => Let(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.LetMut" => LetMut(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.Mutate" => Mutate(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            "main:Ast.Extern" => Extern(FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0)), FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 1))),
            _ => panic!("from_haskell_repr Statement: unrecognized constructor name: {}", con_name)
        }
    }
}

impl FromHaskellRepr for FunctionCall {
    unsafe fn from_haskell_repr(i : *mut StgClosure) -> FunctionCall {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        FunctionCall {
            name : FromHaskellRepr::from_haskell_repr(get_nth_payload(input_ref, 0))
        }
    }
}

pub unsafe fn to_rust_str(t : *const i8) -> String {
    from_utf8(CStr::from_ptr(t).to_bytes()).unwrap().to_string()
}

pub unsafe fn deRefStgInd(mut input : *mut StgClosure) -> *mut StgClosure {
    while (input as u64) & 7 == 0 {
        input = (*(input as *mut StgInd)).indirectee;
    }
    input
}
pub unsafe fn get_constructor_desc(i : *mut StgClosure) -> String {
    return to_rust_str(_GET_CON_DESC(_get_con_itbl(_UNTAG_CLOSURE(i))));
}
