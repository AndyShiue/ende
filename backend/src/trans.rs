use HsClosureFunc::*;
use ast::*;
use std::ffi::CStr;
use std::os::raw::{c_char, c_void};
use std::str::from_utf8;

pub fn to_rust_block<'a>(i : *mut StgClosure) -> Block<'a> {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;
        println!("{}", get_constructor_name(input_ref));
        println!("{:?}", _LOOKS_LIKE_CLOSURE_PTR(*input.payload as *const c_void) as u64);
        Block {
            stmts : to_rust_statements(*input.payload),
            end : Box::new(to_rust_term(*input.payload.offset(1)))
        }
    }
}
fn to_rust_operator(i : *mut StgClosure) -> Operator {
    unsafe {
        let input = _UNTAG_CLOSURE(deRefStgInd(i));
        let name = get_constructor_name(input);
        match name.as_str() {
            "Add" => Operator::Add,
            "Sub" => Operator::Sub,
            "Mul" => Operator::Mul,
            "Div" => Operator::Div,
            _ => panic!("to_rust_operator: unrecognized constructor name: {}", name)
        }
    }
}
fn to_rust_term<'a>(i : *mut StgClosure) -> Term<'a> {
    use ast::Term::*;
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;
        let con_name = get_constructor_name(input_ref);
        match con_name.as_str() {
            "Literal" => Literal(to_rust_i32(*input.payload)),
            "Var" => Var(to_rust_str(*input.payload)),
            "Infix" => Infix(Box::new(to_rust_term(*input.payload)), to_rust_operator(*input.payload.offset(1)), Box::new(to_rust_term(*input.payload.offset(2)))),
            "Call" => Call(to_rust_function_call(*input.payload), to_rust_terms(*input.payload.offset(1))),
            "Scope" => Scope(to_rust_block(*input.payload)),
            "If" => If(Box::new(to_rust_term(*input.payload)), Box::new(to_rust_term(*input.payload.offset(1))), Box::new(to_rust_term(*input.payload.offset(2)))),
            "While" => While(Box::new(to_rust_term(*input.payload)), to_rust_block(*input.payload.offset(1))),
            _ => panic!("to_rust_term: unrecognized constructor name: {}", con_name)
        }
        
    }
}
fn to_rust_i32<'a>(i : *mut StgClosure) -> i32 {
    unsafe {
        let input = *_UNTAG_CLOSURE(deRefStgInd(i));
    }
    unimplemented!()
}
fn to_rust_str<'a>(i : *mut StgClosure) -> &'a str {
    unimplemented!()
}
fn to_rust_terms<'a>(i : *mut StgClosure) -> &'a [Term<'a>] {

    unimplemented!()
}
fn to_rust_statements<'a>(i : *mut StgClosure) -> &'a [Statement<'a>] {
    unsafe {
        let input_ref = _UNTAG_CLOSURE(deRefStgInd(i));
        let input = *input_ref;
        let con_name = get_constructor_name(input_ref);
        println!("con_name in statements: {}", con_name);
    }
    unimplemented!()
}
fn to_rust_statement<'a>(i : *mut StgClosure) -> Statement<'a> {

    unimplemented!()
}
fn to_rust_function_call<'a>(i : *mut StgClosure) -> FunctionCall<'a> {
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

pub fn get_constructor_name(i : *mut StgClosure) -> String {
    let input : *mut StgClosure;
    unsafe { input = _UNTAG_CLOSURE(deRefStgInd(i)) }
    let string = get_constructor_desc(i);
    let mut splitted : Vec<&str> = string.split(':').collect();
    splitted.pop().unwrap().to_string()
}
