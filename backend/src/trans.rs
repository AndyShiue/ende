use HsClosureFunc::*;

use Rts;
use ast::*;
use type_check::*;
use std::ffi::CStr;
use std::str::from_utf8;

pub fn to_rust_program(i : *mut StgClosure) -> Program {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        Program {
            main : to_rust_block(get_nth_payload(input_ref, 0))
        }
    }
}
pub fn to_rust_block(i : *mut StgClosure) -> Block {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        Block {
            stmts : to_rust_statements(get_nth_payload(input_ref, 0)),
            end : Box::new(to_rust_maybe_term(get_nth_payload(input_ref, 1)))
        }
    }
}
fn to_rust_maybe_term(i : *mut StgClosure) -> Option<Term> {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;
        let name = get_constructor_desc(input_ref);
        match name.as_str() {
            "base:Data.Maybe.Just" => Some(to_rust_term(get_nth_payload(input_ref, 0))),
            "base:Data.Maybe.Nothing" => None,
            _ => panic!("to_rust_maybe_term: unrecognized constructor name: {}", name)
        }
    }
}
fn to_rust_operator(i : *mut StgClosure) -> Operator {
    unsafe {
        let input = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input);
        match name.as_str() {
            "main:Ast.Add" => Operator::Add,
            "main:Ast.Sub" => Operator::Sub,
            "main:Ast.Mul" => Operator::Mul,
            "main:Ast.Div" => Operator::Div,
            _ => panic!("to_rust_operator: unrecognized constructor name: {}", name)
        }
    }
}
fn to_rust_term(i : *mut StgClosure) -> Term {
    use ast::Term::*;
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;
        let con_name = get_constructor_desc(input_ref);
        match con_name.as_str() {
            "main:Ast.Literal" => Literal(to_rust_i32(get_nth_payload(input_ref, 0))),
            "main:Ast.Var" => Var(to_rust_string(get_nth_payload(input_ref, 0))),
            "main:Ast.Infix" => Infix(Box::new(to_rust_term(get_nth_payload(input_ref, 0))), to_rust_operator(get_nth_payload(input_ref, 1)), Box::new(to_rust_term(get_nth_payload(input_ref, 2)))),
            "main:Ast.Call" => Call(to_rust_function_call(get_nth_payload(input_ref, 0)), to_rust_terms(get_nth_payload(input_ref, 1))),
            "main:Ast.Scope" => Scope(to_rust_block(get_nth_payload(input_ref, 0))),
            "main:Ast.If" => If(Box::new(to_rust_term(get_nth_payload(input_ref, 0))), Box::new(to_rust_term(get_nth_payload(input_ref, 1))), Box::new(to_rust_term(get_nth_payload(input_ref, 2)))),
            "main:Ast.While" => While(Box::new(to_rust_term(get_nth_payload(input_ref, 0))), to_rust_block(get_nth_payload(input_ref, 1))),
            _ => panic!("to_rust_term: unrecognized constructor name: {}", con_name)
        }

    }
}

fn to_rust_u32(i : *mut StgClosure) -> u32 {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input_ref);
        match name.as_str() {
            "ghc-prim:GHC.Types.I#" => get_nth_payload(input_ref, 0) as u32,
            _ => panic!("to_rust_u32: unrecognized constructor name: {}", name)
        }
    }
}

fn to_rust_i32(i : *mut StgClosure) -> i32 {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input_ref);
        match name.as_str() {
            "ghc-prim:GHC.Types.I#" => get_nth_payload(input_ref, 0) as i32,
            _ => panic!("to_rust_i32: unrecognized constructor name: {}", name)
        }
    }
}
fn to_rust_string(i : *mut StgClosure) -> String {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        fn go(closure : *mut StgClosure, mut acc : Vec<char>) -> Vec<char> {
            unsafe {
                let con_name = get_constructor_desc(closure);
                match con_name.as_str() {
                    "ghc-prim:GHC.Types.:" => {
                        let t1 = to_rust_char(get_nth_payload(closure, 0));
                        acc.push(t1);
                        go(_UNTAG_CLOSURE(deRefStgInd(get_nth_payload(closure, 1))), acc)
                    },
                    "ghc-prim:GHC.Types.[]" => {
                        acc
                    },
                    _ => panic!("to_rust_string: unrecognized constructor name: {}", con_name)
                }
            }
        };
        go(input_ref, Vec::<char>::new()).iter().cloned().collect()
    }
}

fn to_rust_char(i : *mut StgClosure) -> char {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input_ref);
        match name.as_str() {
            "ghc-prim:GHC.Types.C#" => {
                get_nth_payload(input_ref, 0) as u8 as char
            },
            _ => panic!("to_rust_char: unrecognized constructor name: {}", name)
        }
    }
}

fn to_rust_type(i : *mut StgClosure) -> Type {
    unsafe {
        let input = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_desc(input);
        use type_check::Type::*;
        match name.as_str() {
            "main:Ast.Forbidden" => Forbidden,
            "main:Ast.I32Ty" => I32Ty,
            "main:Ast.FunctionTy" => FunctionTy(to_rust_types(get_nth_payload(input, 0)), Box::new(to_rust_type(get_nth_payload(input, 1)))),
            _ => panic!("to_rust_type: unrecognized constructor name: {}", name)
        }
    }
}
fn to_rust_types(i : *mut StgClosure) -> Vec<Type> {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        fn go(closure : *mut StgClosure, mut acc : Vec<Type>) -> Vec<Type> {
            unsafe {
                let con_name = get_constructor_desc(closure);
                match con_name.as_str() {
                    "ghc-prim:GHC.Types.:" => {
                        let t1 = to_rust_type(get_nth_payload(closure, 0));
                        acc.push(t1);
                        go(_UNTAG_CLOSURE(deRefStgInd(get_nth_payload(closure, 1))), acc)
                    },
                    "ghc-prim:GHC.Types.[]" => {
                        acc
                    },
                    _ => panic!("to_rust_types: unrecognized constructor name: {}", con_name)
                }
            }
        };
        go(input_ref, Vec::<Type>::new())
    }
}

fn to_rust_terms(i : *mut StgClosure) -> Vec<Term> {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        fn go(closure : *mut StgClosure, mut acc : Vec<Term>) -> Vec<Term> {
            unsafe {
                let con_name = get_constructor_desc(closure);
                match con_name.as_str() {
                    "ghc-prim:GHC.Types.:" => {
                        let t1 = to_rust_term(get_nth_payload(closure, 0));
                        acc.push(t1);
                        go(_UNTAG_CLOSURE(deRefStgInd(get_nth_payload(closure, 1))), acc)
                    },
                    "ghc-prim:GHC.Types.[]" => {
                        acc
                    },
                    _ => panic!("to_rust_terms: unrecognized constructor name: {}", con_name)
                }
            }
        };
        go(input_ref, Vec::<Term>::new())
    }
}
fn to_rust_statements(i : *mut StgClosure) -> Vec<Statement> {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));

        fn go(closure : *mut StgClosure, mut acc : Vec<Statement>) -> Vec<Statement> {
            unsafe {
                let con_name = get_constructor_desc(closure);
                match con_name.as_str() {
                    "ghc-prim:GHC.Types.:" => {
                        let t1 = to_rust_statement(get_nth_payload(closure, 0));
                        acc.push(t1);
                        go(_UNTAG_CLOSURE(deRefStgInd(get_nth_payload(closure, 1))), acc)
                    },
                    "ghc-prim:GHC.Types.[]" => {
                        acc
                    },
                    _ => panic!("to_rust_statements: unrecognized constructor name: {}", con_name)
                }
            }
        };
        go(input_ref, Vec::<Statement>::new())
    }
}
fn to_rust_statement(i : *mut StgClosure) -> Statement {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let con_name = get_constructor_desc(input_ref);
        use ast::Statement::*;
        match con_name.as_str() {
            "main:Ast.TermSemicolon" => TermSemicolon(to_rust_term(get_nth_payload(input_ref, 0))),
            "main:Ast.Let" => Let(to_rust_string(get_nth_payload(input_ref, 0)), to_rust_term(get_nth_payload(input_ref, 1))),
            "main:Ast.LetMut" => LetMut(to_rust_string(get_nth_payload(input_ref, 0)), to_rust_term(get_nth_payload(input_ref, 1))),
            "main:Ast.Mutate" => Mutate(to_rust_string(get_nth_payload(input_ref, 0)), to_rust_term(get_nth_payload(input_ref, 1))),
            "main:Ast.Extern" => Extern(to_rust_string(get_nth_payload(input_ref, 0)), to_rust_type(get_nth_payload(input_ref, 1))),
            _ => panic!("to_rust_statement: unrecognized constructor name: {}", con_name)
        }
    }
}
fn to_rust_function_call(i : *mut StgClosure) -> FunctionCall {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        FunctionCall {
            name : to_rust_string(get_nth_payload(input_ref, 0))
        }
    }
}

fn to_rust_str(t : *const i8) -> String {
    unsafe {
        from_utf8(CStr::from_ptr(t).to_bytes()).unwrap().to_string()
    }
}

pub fn deRefStgInd(mut input : *mut StgClosure) -> *mut StgClosure {
    unsafe {
        while (input as u64) & 7 == 0 {
            input = (*(input as *mut StgInd)).indirectee;
        }
    }
    input
}
pub fn get_constructor_desc(i : *mut StgClosure) -> String {
    unsafe {
        return to_rust_str(_GET_CON_DESC(_get_con_itbl(_UNTAG_CLOSURE(i))));
    }
}
