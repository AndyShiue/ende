use std::os::raw::c_char;
use std::collections::{HashSet, HashMap};

use llvm_sys::prelude::*;
use llvm_sys::core::*;

use ast::*;
use type_check::*;
use type_check::Type::*;

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

impl Term {
    pub fn rhs_vars(self: &Term) -> HashSet<String> {
        use ast::Term::*;
        match *self {
            Literal(_) => HashSet::new(),
            Var(ref name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Infix(ref left, _, ref right) => left.rhs_vars()
                                                 .union(&right.rhs_vars())
                                                 .cloned()
                                                 .collect(),
            Call(_, ref args) =>
                args.iter()
                    .map(|arg| arg.rhs_vars())
                    .fold(HashSet::new(), |l, r| l.union(&r).cloned().collect()),
            Scope(ref block) => block.rhs_vars(),
            If(ref cond, ref if_true, ref if_false) => {
                let set: HashSet<_> =
                    cond.rhs_vars().union(&if_true.rhs_vars()).cloned().collect();
                set.union(&if_false.rhs_vars()).cloned().collect()
            }
            While(ref cond, ref block) =>
                cond.rhs_vars().union(&block.rhs_vars()).cloned().collect(),
        }
    }
}

impl Statement {
    pub fn rhs_vars(self: &Statement) -> HashSet<String> {
        use ast::Statement::*;
        match *self {
            TermSemicolon(ref term) => term.rhs_vars(),
            Let(_, ref rhs) => rhs.rhs_vars(),
            LetMut(_, ref rhs) => rhs.rhs_vars(),
            Mutate(_, ref rhs) => rhs.rhs_vars(),
            Extern(_, _, _) => HashSet::new(),
        }
    }
}

impl Block {
    pub fn rhs_vars(self: &Block) -> HashSet<String> {
        let stmts_rhs_vars = self.stmts
                                 .iter()
                                 .map(|stmt| stmt.rhs_vars())
                                 .fold(HashSet::new(), |l, r| l.union(&r).cloned().collect());
        stmts_rhs_vars.union(&self.end.rhs_vars()).cloned().collect()
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
            match self.build(module, func, entry, builder, Self::new_env()) {
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

impl Compile for Term {

    type Env = Map<EnvData>;

    fn new_env() -> Self::Env { Map::new() }

    fn build(self: &Term,
             module: LLVMModuleRef,
             func: LLVMValueRef,
             entry: LLVMBasicBlockRef,
             builder: LLVMBuilderRef,
             env: Self::Env) -> Result<LLVMValueRef, Vec<String>> {
        use ast::Term::*;
        unsafe {
            // Build the instructions.
            match *self {
                Literal(i) => Ok(LLVMConstInt(LLVMIntType(32), i as u64, 0)),
                Var(ref str) => {
                    match env.get(str) {
                        Some(pair) => {
                            use self::Direction::*;
                            match pair.direction {
                                Indirect => Ok(LLVMBuildLoad(
                                    builder, pair.llvm_value, try!("load".to_raw())
                                )),
                                Direct => Ok(pair.llvm_value),
                            }
                        }
                        None =>
                            Err(vec![format!("Variable {} isn't declared yet.", str)]),
                    }
                }
                Infix(ref left, ref op, ref right) => {
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
                Call(ref func_call, ref args) => {

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
                Scope(ref block) => {
                    let new_env = env.clone();
                    let block_result =
                        block.build(module, func, entry, builder, Box::new(new_env));
                    let block = try!(block_result);
                    Ok(block)
                }
                If(ref cond, ref if_true, ref if_false) => {
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
                While(ref cond, ref block) => {
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
                        if cond.rhs_vars().contains(&**key) {
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

impl Compile for Block {

    type Env = Box<Map<EnvData>>;

    fn new_env() ->  Self::Env { Box::new(Map::new()) }

    fn build(self: &Block,
             module: LLVMModuleRef,
             func: LLVMValueRef,
             entry: LLVMBasicBlockRef,
             builder: LLVMBuilderRef,
             mut env: Self::Env) -> Result<LLVMValueRef, Vec<String>> {
        use self::Direction::*;
        use ast::Statement::*;
        unsafe {
            for stmt in &self.stmts {
                match *stmt {
                    TermSemicolon(ref term) => {
                        try!(term.build(module, func, entry, builder, *env.clone()));
                    }
                    Let(ref lhs, ref rhs) => {
                        let value = try!(rhs.build(module, func, entry, builder, *env.clone()));
                        let env_data = EnvData { llvm_value: value, direction: Direct, ty: I32Ty };
                        env.insert(lhs.clone(), env_data);
                    }
                    LetMut(ref lhs, ref rhs) => {
                        let alloca =
                            LLVMBuildAlloca(builder, LLVMInt32Type(), lhs.as_ptr() as *const i8);
                        let built_rhs =
                            try!(rhs.build(module, func, entry, builder, *env.clone()));
                        LLVMBuildStore(builder, built_rhs, alloca);
                        let env_data =
                            EnvData { llvm_value: alloca, direction: Indirect, ty: I32Ty };
                        env.insert(lhs.clone(), env_data);
                    }
                    Mutate(ref lhs, ref rhs) => {
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
                    Extern(ref name, ref args_types, ref ret_type) => {
                        let func_ty =
                            LLVMTypeRef::from(
                                &FunctionTy(args_types.clone(), Box::new(ret_type.clone()))
                            );
                        let func = LLVMAddFunction(
                            module,
                            // Actually unnessasary clone.
                            try!(name.to_raw().map_err(|err| vec![err[0].clone()])),
                            func_ty
                        );
                        let env_data = EnvData {
                            llvm_value: func,
                            direction: Direct,
                            ty: FunctionTy(args_types.clone(), Box::new(ret_type.clone())),
                        };
                        env.insert(name.clone(), env_data);
                    }
                }
            }
            self.end.build(module, func, entry, builder, *env)
        }
    }
}

// Doesn't work right now. Will try to fix.
pub unsafe fn emit_obj(module: LLVMModuleRef) {
    use llvm_sys::target::*;
    use llvm_sys::target_machine::*;
    let triple = LLVMGetDefaultTargetTriple();
    LLVM_InitializeNativeTarget();
    let target = LLVMGetFirstTarget();
    let cpu = "x86-64\0".as_ptr() as *const i8;
    let feature = "\0".as_ptr() as *const i8;
    let opt_level = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
    let reloc_mode = LLVMRelocMode::LLVMRelocDefault;
    let code_model = LLVMCodeModel::LLVMCodeModelDefault;
    let target_machine =
        LLVMCreateTargetMachine(target, triple, cpu, feature, opt_level, reloc_mode, code_model);
    let file_type = LLVMCodeGenFileType::LLVMObjectFile;
    // TODO: error handling here.
    LLVMTargetMachineEmitToFile(target_machine,
                                module,
                                "/Users/andyshiue/Desktop/main.o".to_raw().unwrap() as *mut i8,
                                file_type,
                                ["Cannot init_module file.\0".as_ptr()] // This is wrong.
                                    .as_mut_ptr() as *mut *mut i8);
}


pub unsafe fn emit_ir(module: LLVMModuleRef) {
    use llvm_sys::bit_writer::*;
    LLVMWriteBitcodeToFile(module, "/Users/andyshiue/Desktop/main.bc".to_raw().unwrap());
}
