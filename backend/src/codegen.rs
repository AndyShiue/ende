use std::os::raw::c_char;
use std::collections::{HashSet, HashMap};
use std::process::Command;

use llvm_sys::prelude::*;
use llvm_sys::core::*;

use type_check::*;
use type_check::Type::*;

use inc::*;
trait ToRaw: Into<Vec<u8>> {
    fn to_raw(self) -> Result<*const c_char, Vec<String>>;
}

impl<'a> ToRaw for &'a str {
    fn to_raw(self: &'a str) -> Result<*const c_char, Vec<String>> {
        use std::error::Error;
        use std::ffi::CString;
        CString::new(self).map(|str| str.as_ptr() as *const i8)
                          .map_err(|err| vec![err.description().to_string()])
    }
}

pub type Map<T> = HashMap<String, T>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Direction {
    Indirect,
    Direct,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnvData {
    llvm_value: LLVMValueRef,
    direction: Direction,
    ty: Type,
}

impl TaggedTerm<Type> {
    pub fn rhs_vars(self: &Self) -> HashSet<String> {
        use type_check::TaggedTerm::*;
        match *self {
            Literal(_, _) => HashSet::new(),
            Var(_, ref name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Infix(_, ref left, _, ref right) => left.rhs_vars()
                                                 .union(&right.rhs_vars())
                                                 .cloned()
                                                 .collect(),
            Call(_, _, ref args) =>
                args.iter()
                    .map(|arg| arg.rhs_vars())
                    .fold(HashSet::new(), |l, r| l.union(&r).cloned().collect()),
            Scope(_, ref block) => block.rhs_vars(),
            If(_, ref cond, ref if_true, ref if_false) => {
                let set: HashSet<_> =
                    cond.rhs_vars().union(&if_true.rhs_vars()).cloned().collect();
                set.union(&if_false.rhs_vars()).cloned().collect()
            }
            While(_, ref cond, ref block) =>
                cond.rhs_vars().union(&block.rhs_vars()).cloned().collect(),
            Stmt(ref stmt) => stmt.rhs_vars()
        }
    }
}

impl TaggedStatement<Type> {
    pub fn rhs_vars(self: &Self) -> HashSet<String> {
        use type_check::TaggedStatement::*;
        match *self {
            TermSemicolon(_, ref term) => term.rhs_vars(),
            Let(_, _, ref rhs) => rhs.rhs_vars(),
            LetMut(_, _, ref rhs) => rhs.rhs_vars(),
            Mutate(_, _, ref rhs) => rhs.rhs_vars(),
            Extern(_, _, _) => HashSet::new(),
        }
    }
}

impl TaggedBlock<Type> {
    pub fn rhs_vars(self: &Self) -> HashSet<String> {
        let stmts_rhs_vars = self.stmts
                                 .iter()
                                 .map(|stmt| stmt.rhs_vars())
                                 .fold(HashSet::new(), |l, r| l.union(&r).cloned().collect());
        let end_vars = match *self.end {
            Some(ref term) => term.rhs_vars(),
            None => HashSet::new(),
        };
        stmts_rhs_vars.union(&end_vars).cloned().collect()
    }
}

pub trait Compile {

    type Env;

    fn new_env() -> Self::Env;

    fn build(self: &Self,
                    module: LLVMModuleRef,
                    func: LLVMValueRef,
                    entry: LLVMBasicBlockRef,
                    builder: LLVMBuilderRef,
                    env: Self::Env) -> Result<LLVMValueRef, Vec<String>>;

    fn init_module(self: &Self,
                   module: LLVMModuleRef,
                   func: LLVMValueRef,
                   builder: LLVMBuilderRef) -> Result<(), Vec<String>> {
        unsafe {
            let entry = LLVMAppendBasicBlock(func, "entry\0".as_ptr() as *const i8);
            LLVMPositionBuilderAtEnd(builder, entry);
            match self.build(module, func, entry, builder, <Self as Compile>::new_env()) {
                Ok(val) => {
                    LLVMBuildRet(builder, val);
                    Ok(())
                }
                Err(vec) => Err(vec),
            }
        }
    }

    fn gen_module(self: &Self) -> Result<LLVMModuleRef, Vec<String>> {
        unsafe {
            let name = try!("Main".to_raw());
            let module = LLVMModuleCreateWithName(name);
            let args: &mut [LLVMTypeRef] = &mut [];
            let func_ty = LLVMFunctionType(LLVMInt32Type(), args.as_mut_ptr() , 0, 0);
            let func = LLVMAddFunction(module, try!("main".to_raw()), func_ty);
            let builder = LLVMCreateBuilder();
            try!(self.init_module(module, func, builder));
            Ok(module)
        }
    }

}

impl Compile for TaggedTerm<Type> {

    type Env = Map<EnvData>;

    fn new_env() -> Self::Env { Map::new() }

    fn build(self: &Self,
             module: LLVMModuleRef,
             func: LLVMValueRef,
             entry: LLVMBasicBlockRef,
             builder: LLVMBuilderRef,
             env: Self::Env) -> Result<LLVMValueRef, Vec<String>> {
        use type_check::TaggedTerm::*;
        unsafe {
            // Build the instructions.
            match *self {
                Literal(_, i) => Ok(LLVMConstInt(LLVMIntType(32), i as u64, 0)),
                Var(_, ref str) => {
                    match env.get(str) {
                        Some(data) => {
                            use self::Direction::*;
                            match data.direction {
                                Indirect => Ok(LLVMBuildLoad(
                                    builder, data.llvm_value, try!("load".to_raw())
                                )),
                                Direct => Ok(data.llvm_value),
                            }
                        }
                        None =>
                            Err(vec![format!("Variable {} isn't declared yet.", str)]),
                    }
                }
                Infix(_, ref left, ref op, ref right) => {
                    use ast::Operator::*;
                    let another_env = env.clone();
                    let left = try!(left.build(module, func, entry, builder, env));
                    let right = try!(right.build(module, func, entry, builder, another_env));
                    match *op {
                        Add => Ok(LLVMBuildAdd(
                            builder, left, right, try!("add".to_raw())
                        )),
                        Sub => Ok(LLVMBuildSub(
                            builder, left, right, try!("sub".to_raw())
                        )),
                        Mul => Ok(LLVMBuildMul(
                            builder, left, right, try!("mul".to_raw())
                        )),
                        Div => Ok(LLVMBuildSDiv(
                            builder, left, right, try!("div".to_raw())
                        )),
                    }
                }
                Call(_, ref func_call, ref args) => {

                    let ref name = func_call.name;
                    let llvm_func = if let Some(env_data) = env.get(name) {
                        let expected_arity;
                        if let FunctionTy(ref args_types, _) = env_data.ty {
                            expected_arity = args_types.len();
                        } else {
                            unreachable!();
                        };
                        let actual_arity = (*args).len();
                        if expected_arity != actual_arity {
                            let expected_arity_str = &*expected_arity.to_string();
                            let actual_arity_str = &*expected_arity.to_string();
                            let error_message = format!(
                                "Function {} expect {} parameters, \
                                 but {} parameters are provided.",
                                name, expected_arity_str, actual_arity_str
                            );
                            return Err(vec![error_message]);
                        }
                        env_data.llvm_value
                    } else {
                        return Err(
                            vec![format!("Function {} hasn't been declared yet.", name)]
                        );
                    };

                    let results: Vec<Result<LLVMValueRef, Vec<String>>> =
                        args.iter()
                            .map(|term| term.build(module, func, entry, builder, env.clone()))
                            .collect();

                    // It's really so painful.
                    // Read the types of `results` and `result_args` to know what I'm doing.
                    let result_args: Result<Vec<LLVMValueRef>, Vec<String>> =
                        results.iter()
                               .fold(Ok(Vec::new()),
                                     |left, right| {
                                         match *right {
                                             Ok(right_val) => match left {
                                                 Ok(mut vec) => {
                                                     vec.push(right_val);
                                                     Ok(vec)
                                                 }
                                                 Err(err) => Err(err),
                                             },
                                             Err(ref left_errors) => match left {
                                                 Ok(_) => Err(left_errors.clone()),
                                                 Err(ref right_errors) => {
                                                     let mut new_vec = left_errors.clone();
                                                     new_vec.append(&mut right_errors.clone());
                                                     Err(new_vec)
                                                 }
                                             }
                                         }
                                     });

                    // let mut raw_args = try!(result_args).as_mut_ptr();
                    // The above line makes the program segfault. Wierd.
                    let mut args: Vec<LLVMValueRef> = try!(result_args);
                    let raw_args = args.as_mut_ptr();
                    let name = &*("call".to_string() + &*func_call.name);
                    let value = LLVMBuildCall(builder,
                                              llvm_func,
                                              raw_args,
                                              args.len() as u32,
                                              try!(name.to_raw())
                                             );
                    Ok(value)
                }
                Scope(_, ref block) => {
                    let new_env = env.clone();
                    let block_result =
                        block.build(module, func, entry, builder, Box::new(new_env));
                    let block = try!(block_result);
                    Ok(block)
                }
                If(_, ref cond, ref if_true, ref if_false) => {
                    use self::Direction::*;
                    // Build the condition.
                    let built_cond = try!(cond.build(module, func, entry, builder, env.clone()));
                    // And check if the condition equals to zero.
                    let zero = LLVMConstInt(LLVMIntType(32), 0, 0);
                    use llvm_sys::LLVMIntPredicate::LLVMIntEQ;
                    let is_zero = LLVMBuildICmp(
                        builder, LLVMIntEQ, built_cond, zero, try!("iszero".to_raw())
                    );
                    // Create the basic blocks.
                    let then_branch = LLVMAppendBasicBlock(func, try!("then".to_raw()));
                    let else_branch = LLVMAppendBasicBlock(func, try!("else".to_raw()));
                    let next = LLVMAppendBasicBlock(func, try!("next".to_raw()));
                    LLVMBuildCondBr(builder, is_zero, else_branch, then_branch);
                    // Now go inside the true case.
                    LLVMPositionBuilderAtEnd(builder, then_branch);
                    // Create a new environment.
                    let mut new_env = env.clone();
                    // Build the phi nodes.
                    for (key, env_data) in &env {
                        if cond.rhs_vars().contains(&**key) {
                            match env_data.direction {
                                Indirect => {
                                    let ty = LLVMPointerType(LLVMIntType(32), 0);
                                    let phi = LLVMBuildPhi(builder, ty, key.as_ptr() as *const i8);
                                    LLVMAddIncoming(phi,
                                                    [env_data.llvm_value].as_mut_ptr(),
                                                    [entry].as_mut_ptr(),
                                                    1);
                                    let new_data = EnvData {
                                        llvm_value: phi,
                                        direction: Indirect,
                                        ty: I32Ty,
                                    };
                                    new_env.insert(key.clone(), new_data);
                                }
                                Direct => {
                                    let name = try!((*key).to_raw());
                                    let phi =
                                        LLVMBuildPhi(builder, LLVMIntType(32), name);
                                    let another_env = env.clone();
                                    let old_data = another_env.get(key).unwrap(); // Safe here.
                                    LLVMAddIncoming(phi,
                                                    [old_data.llvm_value].as_mut_ptr(),
                                                    [entry].as_mut_ptr(),
                                                    1);
                                    // Update the enviroment.
                                    let new_data = EnvData {
                                        llvm_value: phi,
                                        direction: old_data.direction,
                                        ty: I32Ty,
                                    };
                                    new_env.insert(key.clone(), new_data);
                                }
                            }
                        }
                    }
                    let then_val =
                        try!(if_true.build(module, func, entry, builder, env.clone()));
                    LLVMBuildBr(builder, next);
                    // Switch to the false case and do everything again.
                    // The code below is copy-pasted for not overengineering.
                    LLVMPositionBuilderAtEnd(builder, else_branch);
                    let mut new_env = env.clone();
                    for (key, env_data) in &env {
                        if cond.rhs_vars().contains(&**key) {
                            match env_data.direction {
                                Indirect => {
                                    let ty = LLVMPointerType(LLVMIntType(32), 0);
                                    let phi = LLVMBuildPhi(builder, ty, key.as_ptr() as *const i8);
                                    LLVMAddIncoming(phi,
                                                    [env_data.llvm_value].as_mut_ptr(),
                                                    [entry].as_mut_ptr(),
                                                    1);
                                    let env_data = EnvData {
                                        llvm_value: phi,
                                        direction: Indirect,
                                        ty: I32Ty,
                                    };
                                    new_env.insert(key.clone(), env_data);
                                }
                                Direct => {
                                    let name = try!((*key).to_raw());
                                    let phi =
                                        LLVMBuildPhi(builder, LLVMIntType(32), name);
                                    let another_env = env.clone();
                                    let old_data = another_env.get(key).unwrap(); // Safe here.
                                    LLVMAddIncoming(phi,
                                                    [old_data.llvm_value].as_mut_ptr(),
                                                    [entry].as_mut_ptr(),
                                                    1);
                                    // Update the enviroment.
                                    let new_data = EnvData {
                                        llvm_value: phi,
                                        direction: old_data.direction,
                                        ty: I32Ty,
                                    };
                                    new_env.insert(key.clone(), new_data);
                                }
                            }
                        }
                    }
                    let else_val =
                        try!(if_false.build(module, func, entry, builder, env.clone()));
                    LLVMBuildBr(builder, next);
                    // Place The builder at the end of the last loop.
                    LLVMPositionBuilderAtEnd(builder, next);
                    // New enviroment, again.
                    let mut new_env = env.clone();
                    // Build the last phi node representing the value of the whole if-then-else
                    // clause.
                    let if_str = "if";
                    let name = try!(if_str.to_raw());
                    let phi = LLVMBuildPhi(builder, LLVMIntType(32), name);
                    LLVMAddIncoming(phi,
                                    [then_val, else_val].as_mut_ptr(),
                                    [then_branch, else_branch].as_mut_ptr(),
                                    2);
                    let env_data = EnvData { llvm_value: phi, direction: Direct, ty: I32Ty };
                    new_env.insert(if_str.to_string(), env_data);
                    Ok(phi)
                }
                While(_, ref cond, ref block) => {
                    // Build the condition.
                    // It has to be done first because it could mutate variables.
                    let built_cond = try!(cond.build(module, func, entry, builder, env.clone()));
                    // And check if the condition equals to zero.
                    let zero = LLVMConstInt(LLVMIntType(32), 0, 0);
                    use llvm_sys::LLVMIntPredicate::LLVMIntEQ;
                    let is_zero = LLVMBuildICmp(
                        builder, LLVMIntEQ, built_cond, zero, try!("iszero".to_raw())
                    );
                    // Create the basic blocks.
                    let loop_block = LLVMAppendBasicBlock(func, try!("loop".to_raw()));
                    let after_loop = LLVMAppendBasicBlock(func, try!("afterloop".to_raw()));
                    LLVMBuildCondBr(builder, is_zero, after_loop, loop_block);
                    // Now go inside the loop.
                    LLVMPositionBuilderAtEnd(builder, loop_block);
                    // Create a new environment.
                    let mut new_env = env.clone();
                    // Build the phi nodes.
                    for (key, pair) in &env {
                        if cond.rhs_vars().contains(key) {
                            use self::Direction::*;
                            match pair.direction {
                                Indirect => {
                                    let ty = LLVMPointerType(LLVMIntType(32), 0);
                                    let phi = LLVMBuildPhi(builder, ty, key.as_ptr() as *const i8);
                                    let old_ptr = (&env.get(key)).unwrap().llvm_value;
                                    LLVMAddIncoming(phi,
                                                    [old_ptr, phi].as_mut_ptr(),
                                                    [entry, loop_block].as_mut_ptr(),
                                                    2);
                                    let env_data = EnvData {
                                        llvm_value: phi,
                                        direction: Indirect,
                                        ty: I32Ty,
                                    };
                                    new_env.insert(key.clone(), env_data);
                                }
                                Direct => {
                                    let name = try!((*key).to_raw());
                                    let phi =
                                        LLVMBuildPhi(builder, LLVMIntType(32), name);
                                    let another_env = env.clone();
                                    let old_data = another_env.get(key).unwrap(); // Safe here.
                                    LLVMAddIncoming(phi,
                                                    [old_data.llvm_value, phi].as_mut_ptr(),
                                                    [entry, loop_block].as_mut_ptr(),
                                                    2);
                                    // Update the enviroment.
                                    let new_data = EnvData {
                                        llvm_value: phi,
                                        direction: old_data.direction,
                                        ty: I32Ty,
                                    };
                                    new_env.insert(key.clone(), new_data);
                                }
                            }
                        }
                    }
                    try!(block.build(module, func, entry, builder, Box::new(new_env.clone())));
                    // Check the condition for next iteration.
                    let built_cond = try!(cond.build(module, func, entry, builder, new_env));
                    let is_zero = LLVMBuildICmp(
                        builder, LLVMIntEQ, built_cond, zero, try!("iszero".to_raw())
                    );
                    LLVMBuildCondBr(builder, is_zero, after_loop, loop_block);
                    // Place The builder at the end of the last loop.
                    LLVMPositionBuilderAtEnd(builder, after_loop);
                    // Done.
                    Ok(zero)
                }
                Stmt(_) => unimplemented!()
            }
        }
    }

}

impl<'a> From<&'a Type> for LLVMTypeRef {
    fn from(ty: &Type) -> LLVMTypeRef {
        unsafe {
            match *ty {
                Forbidden => unreachable!(),
                I32Ty => LLVMInt32Type(),
                Enum(ref en) => if en.name == "Unit" { LLVMVoidType() } else { unreachable!() },
                FunctionTy(ref args_types, ref ret_type) => {
                    let args_llvm_types: Vec<LLVMTypeRef> =
                        args_types.iter().map(|ty| LLVMTypeRef::from(&*ty)).collect();
                    LLVMFunctionType(LLVMTypeRef::from(&**ret_type),
                                     (args_llvm_types.clone()).as_mut_ptr(),
                                     args_types.len() as u32,
                                     0
                                    )
                }
            }
        }
    }
}

impl Compile for TaggedBlock<Type> {

    type Env = Box<Map<EnvData>>;

    fn new_env() ->  Self::Env { Box::new(Map::new()) }

    fn build(self: &Self,
             module: LLVMModuleRef,
             func: LLVMValueRef,
             entry: LLVMBasicBlockRef,
             builder: LLVMBuilderRef,
             mut env: Self::Env) -> Result<LLVMValueRef, Vec<String>> {
        use type_check::TaggedStatement::*;
        use self::Direction::*;
        unsafe {
            for stmt in &self.stmts {
                match *stmt {
                    TermSemicolon(_, ref term) => {
                        try!(term.build(module, func, entry, builder, *env.clone()));
                    }
                    Let(_, ref lhs, ref rhs) => {
                        let value = try!(rhs.build(module, func, entry, builder, *env.clone()));
                        let env_data = EnvData { llvm_value: value, direction: Direct, ty: I32Ty };
                        env.insert(lhs.clone(), env_data);
                    }
                    LetMut(_, ref lhs, ref rhs) => {
                        let alloca =
                            LLVMBuildAlloca(builder, LLVMInt32Type(), lhs.as_ptr() as *const i8);
                        let built_rhs =
                            try!(rhs.build(module, func, entry, builder, *env.clone()));
                        LLVMBuildStore(builder, built_rhs, alloca);
                        let env_data =
                            EnvData { llvm_value: alloca, direction: Indirect, ty: I32Ty };
                        env.insert(lhs.clone(), env_data);
                    }
                    Mutate(_, ref lhs, ref rhs) => {
                        let var_result = match env.get(lhs) {
                            Some(var) => Ok(var.clone()),
                            None => Err(
                                vec![format!("Variable {} isn't declared yet.", lhs)]
                            ),
                        };
                        let built_rhs =
                            try!(rhs.build(module, func, entry, builder, *env.clone()));
                        let env_data = try!(var_result);
                        match env_data.direction {
                            Indirect => {
                                LLVMBuildStore(builder, built_rhs, env_data.llvm_value);
                            }
                            Direct =>
                                return Err(
                                    vec![
                                        format!(
                                            "Variable {} is immutable, so it cannot be mutated.",
                                            lhs
                                        )
                                    ]
                                ),
                        }
                    }
                    Extern(_, ref name, ref ty) => {
                        let func_ty = LLVMTypeRef::from(ty);
                        let func = LLVMAddFunction(
                            module,
                            // Actually unnessasary clone.
                            try!(name.to_raw().map_err(|err: Vec<String>| vec![err[0].clone()])),
                            func_ty
                        );
                        let env_data = EnvData {
                            llvm_value: func,
                            direction: Direct,
                            ty: ty.clone(),
                        };
                        env.insert(name.clone(), env_data);
                    }
                }
            }
            if let Some(ref term) = *self.end {
                term.build(module, func, entry, builder, *env)
            } else {
                use std::ptr::null;
                use llvm_sys::LLVMValue;
                Ok(null::<LLVMValue>() as *mut _)
            }
        }
    }
}

impl Compile for TaggedProgram<Type> {

    type Env = Box<Map<EnvData>>;

    fn new_env() -> Self::Env { Box::new(Map::new()) }

    fn build(self: &Self,
             module: LLVMModuleRef,
             func: LLVMValueRef,
             entry: LLVMBasicBlockRef,
             builder: LLVMBuilderRef,
             env: Self::Env) -> Result<LLVMValueRef, Vec<String>> {
        self.main.build(module, func, entry, builder, env)
    }
}

pub unsafe fn emit_ir(module: LLVMModuleRef, output: String) {
    use llvm_sys::bit_writer::*;
    let mut bc = output.clone();
    bc.push_str(".bc");
    LLVMWriteBitcodeToFile(module, bc.to_raw().unwrap());
}

pub unsafe fn emit_exe(output: String) {
    let mut bc = output.clone();
    bc.push_str(".bc");
    let mut o = output.clone();
    o.push_str(".o");
    let llc_output = Command::new(LLVM_LLC_PATH)
        .arg(bc)
        .arg("--filetype=obj")
        .arg("-o")
        .arg(o.clone())
        .output()
        .unwrap_or_else(|e| { panic!("failed to execute llc: {}", e) });
    println!("{}", String::from_utf8_lossy(&*llc_output.stdout));
    println!("{}", String::from_utf8_lossy(&*llc_output.stderr));
    let gcc_output = Command::new("gcc")
        .arg("-o")
        .arg(output)
        .arg(o)
        .output()
        .unwrap_or_else(|e| { panic!("failed to execute gcc: {}", e) });
    println!("{}", String::from_utf8_lossy(&*gcc_output.stdout));
    println!("{}", String::from_utf8_lossy(&*gcc_output.stderr));
}
