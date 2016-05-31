use HsClosureFunc::*;

use Rts;
use ast::*;
use std::ffi::CStr;
use std::os::raw::{c_char, c_void};
use std::str::from_utf8;

pub fn to_rust_block(i : *mut StgClosure) -> Block {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;
        println!("{}", get_constructor_desc(input_ref));
        let zeroth = deRefStgInd(get_nth_payload(input_ref, 0));
        let first = deRefStgInd(get_nth_payload(input_ref, 1));
        println!("{}", get_constructor_desc(zeroth));
        println!("{}", get_constructor_desc(first));
        Block {
            stmts : to_rust_statements(get_nth_payload(input_ref, 0)),
            end : Box::new(to_rust_term(get_nth_payload(input_ref, 1)))
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
            "Literal" => Literal(to_rust_i32(*input.payload.offset(1))),
            "Var" => Var(to_rust_str(*input.payload.offset(1))),
            "Infix" => Infix(Box::new(to_rust_term(*input.payload.offset(1))), to_rust_operator(*input.payload.offset(2)), Box::new(to_rust_term(*input.payload.offset(3)))),
            "Call" => Call(to_rust_function_call(*input.payload.offset(1)), to_rust_terms(*input.payload.offset(2))),
            "Scope" => Scope(to_rust_block(*input.payload.offset(1))),
            "If" => If(Box::new(to_rust_term(*input.payload.offset(1))), Box::new(to_rust_term(*input.payload.offset(2))), Box::new(to_rust_term(*input.payload.offset(3)))),
            "While" => While(Box::new(to_rust_term(*input.payload.offset(1))), to_rust_block(*input.payload.offset(2))),
            _ => panic!("to_rust_term: unrecognized constructor name: {}", con_name)
        }
        
    }
}
fn to_rust_i32(i : *mut StgClosure) -> i32 {
    unsafe {
        let input = *_UNTAG_CLOSURE(deRefStgInd(i));
    }
    unimplemented!()
}
fn to_rust_str(i : *mut StgClosure) -> String {
    unimplemented!()
}
fn to_rust_terms(i : *mut StgClosure) -> Vec<Term> {

    unimplemented!()
}
fn to_rust_statements(i : *mut StgClosure) -> [Statement] {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;

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

    unimplemented!()
}
fn to_rust_function_call(i : *mut StgClosure) -> FunctionCall {
    unimplemented!()
}

fn to_rust_string(t : *const i8) -> String {
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
        return to_rust_string(_GET_CON_DESC(_get_con_itbl(_UNTAG_CLOSURE(i))));
    }
}
