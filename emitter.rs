// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// LLVM IR emitter

use core::libc::{c_char, c_uint, c_ulonglong};
use core::hashmap::HashMap;

use rustllvm::{ModuleRef, TypeRef, ValueRef, BuilderRef};
use rustllvm::llvm;
use rustllvm;

use util;
use lexer::Position;
use ast::*;

// --== The main part ==--

type ProcMap = HashMap<@str, ValueRef>;
type VarMap = HashMap<@str, ValueRef>;

priv struct EmitterContext {
    module: ModuleRef,
    builder: BuilderRef,

    var_map: VarMap,
    proc_map: ProcMap,

    dbg_empty: ValueRef,
    dbg_integer: ValueRef,
    dbg_boolean: ValueRef,
    dbg_file: ValueRef,

    dbg_vars: ~[ValueRef],
    dbg_subprogs: ~[ValueRef]
}

pub fn emit_program(bc_path: &Path, program: &Program) {
    // TODO: absoule name could be used instead of relative
    let dirname = program.src_path.dirname();
    let filename = program.src_path.filename().get();

    // Prepare debug info empty node for (many) future uses
    let dbg_empty = md_node([const_i32(0)]);

    // Prepare debug info for basic types
    let dbg_integer = mk_basic_type_dbg_node("integer", integer_width, integer_width,
                                             DW_ATE_signed);
    let dbg_boolean = mk_basic_type_dbg_node("boolean", boolean_width, boolean_width,
                                             DW_ATE_boolean);
    
    // Generate debug info for the compiled file.
    let dbg_file = mk_file_dbg_node(dirname, filename);

    let module = create_module(filename);
    let builder = unsafe { llvm::LLVMCreateBuilder() };

    let mut ec = EmitterContext {
        module: module,
        builder: builder,
    
        var_map: HashMap::new(),
        proc_map: HashMap::new(),

        dbg_empty: dbg_empty,
        dbg_integer: dbg_integer,
        dbg_boolean: dbg_boolean,
        dbg_file: dbg_file,
        
        dbg_vars: ~[],
        dbg_subprogs: ~[]
    };

    // Emit code (and generate debug info) for each procedure/variable definition.
    for program.defs.each |def| {
        match *def {
            ProcDef(ref proc) => emit_proc(&mut ec, proc),
            VarDef(ref var) => emit_global_var(&mut ec, var)
        }
    }

    // Emit code (and generate debug info) for the main block of program. The main
    // block is compiled as function: i32 main()
    let main_proc = Proc {
        pos: program.block.pos,
        name: @"main",
        args: ~[],
        ret_ty: Some(IntegerType),
        vars: ~[],
        block: copy program.block // TODO: think of something better
    };
    emit_proc(&mut ec, &main_proc);

    // All the metadata nodes with debug information for variables and procedures
    // are gathered in ec.dbg_vars and ec.dbg_procs lists. We have to wrap these
    // lists in single nodes.
    let dbg_subprog_list = md_node(ec.dbg_subprogs);
    let dbg_var_list =
        if ec.dbg_vars.is_empty() { dbg_empty } else { md_node(ec.dbg_vars) };

    // Generate debug info for this compile unit and add it to the module.
    let dbg_cu = mk_compile_unit_dbg_node(dbg_empty, dbg_file, dbg_subprog_list, dbg_var_list);
    add_named_metadata(module, "llvm.dbg.cu", dbg_cu);

    write_module_bitcode(module, bc_path);
}

fn emit_global_var(ec: &mut EmitterContext, var: &Var) {
    let ty = translate_type(var.ty);
    let lvar = unsafe {
        let lvar = str::as_c_str(var.name, |name| {
            llvm::LLVMAddGlobal(ec.module, ty, name)
        });
        rustllvm::SetLinkage(lvar, rustllvm::InternalLinkage);
        let zero = llvm::LLVMConstNull(ty);
        llvm::LLVMSetInitializer(lvar, zero);
        lvar
    };
    ec.var_map.insert(var.name, lvar);

    let dbg_ty = get_type_dbg_node(ec, var.ty);
    let dbg_var = mk_global_var_dbg_node(ec.dbg_file, var.name, dbg_ty, lvar, var.pos.line);
    ec.dbg_vars.push(dbg_var);
}

fn emit_proc(ec: &mut EmitterContext, proc: &Proc) {
    // Determine the LLVM return type of this procedure and a debug node for it.
    let (ret_ty, dbg_ret_ty) = match proc.ret_ty {
        Some(ty) => (translate_type(ty), get_type_dbg_node(ec, ty)),
        None => (type_void(), md_null())
    };

    // Determine the LLVM argument types of this procedure and corresponding debug nodes.
    let mut arg_tys = ~[];
    let mut dbg_arg_tys = ~[];
    for proc.args.each |arg| {
        arg_tys.push(translate_type(arg.ty));
        dbg_arg_tys.push(get_type_dbg_node(ec, arg.ty));
    }

    let fn_ty = type_fn(ret_ty, arg_tys);

    // Create an LLVM function for this procedure.
    let func = unsafe {
        str::as_c_str(proc.name, |name| {
            llvm::LLVMAddFunction(ec.module, name, fn_ty)
        })
    };

    // Update the name -> LLVM function map.
    ec.proc_map.insert(proc.name, func);

    // Create debug information for this procedure.
    let dbg_subprog_ty = mk_subroutine_type_dbg_node(dbg_ret_ty, dbg_arg_tys);
    let dbg_subprog = mk_subprogram_dbg_node(ec.dbg_empty, ec.dbg_file,proc.name,
                                             dbg_subprog_ty, func, proc.pos.line,
                                             proc.block.pos.line); // TODO: check last arg;
    ec.dbg_subprogs.push(dbg_subprog);

    let mut lvm = HashMap::new(); // Local variable map

    // Create entry block for this procedure.
    let entry = unsafe {
        str::as_c_str("entry", |name| {
            llvm::LLVMAppendBasicBlock(func, name)
        })
    };
    unsafe {
        llvm::LLVMPositionBuilderAtEnd(ec.builder, entry);
    }

    // Optionally, create a zeroed local variable to hold the function result.
    let ret_var = do proc.ret_ty.map |ty| {
        emit_local_var(ec, &mut lvm, proc.name, *ty, const_zero(ret_ty))
    };
        
    // Create local variables for procedure arguments.
    let mut i = 0;
    for proc.args.each |arg| {
        let arg_val = unsafe {
            llvm::LLVMGetParam(func, i as c_uint)
        };
        emit_local_var(ec, &mut lvm, arg.name, arg.ty, arg_val);
        i += 1;
    }

    // Create local variables for (Pascal) local variables.
    for proc.vars.each |var| {
        let lty = translate_type(var.ty);
        emit_local_var(ec, &mut lvm, var.name, var.ty, const_zero(lty));
    }
    
    // Emit LLVM IR for all statements.
    for proc.block.stmts.each |stmt| {
        emit_stmt(ec, &lvm, stmt, dbg_subprog);
    }
    
    unset_debug_loc(ec);

    // Emit code returning from a procedure.
    match ret_var {
        Some(var) => unsafe {
            let ret_val = do noname |name| {
                llvm::LLVMBuildLoad(ec.builder, var, name)
            };
            llvm::LLVMBuildRet(ec.builder, ret_val);
        },
        None => unsafe {
            llvm::LLVMBuildRetVoid(ec.builder);
        }
    }
}

fn emit_local_var(ec: &EmitterContext, lvm: &mut VarMap, name: @str, ty: Type,
                  init_val: ValueRef) -> ValueRef {
    let lty = translate_type(ty);
    let var = unsafe {
        let var = str::as_c_str(name, |name| {
            llvm::LLVMBuildAlloca(ec.builder, lty, name)
        });
        llvm::LLVMBuildStore(ec.builder, init_val, var);
        var
// TODO: debug info
//        md_node([var])
//        do noname |name| {
//            llvm:LLVMBuildCall(ec.builder, ec.declare_fn
    };
    lvm.insert(name, var);
    var
}

fn emit_stmt(ec: &EmitterContext, lvm: &VarMap, stmt: &Stmt, dbg_scope: ValueRef) {
    match *stmt {
        AssignmentStmt(pos, var_name, ref expr) => {
            set_debug_loc(ec, pos, dbg_scope);
            
            let var = get_var_pointer(ec, lvm, var_name);
            let val = emit_expr(ec, lvm, *expr);
            unsafe {
                llvm::LLVMBuildStore(ec.builder, val, var);
            }
        },
        CallStmt(pos, proc_name, ref args) => {
            set_debug_loc(ec, pos, dbg_scope);

            let proc = get_proc_pointer(ec, proc_name);
            
            let largs = do args.map |arg| { emit_expr(ec, lvm, arg) };

            do noname |name| {
                unsafe {
                    llvm::LLVMBuildCall(ec.builder, proc, 
                                        vec::raw::to_ptr(largs),
                                        largs.len() as u32, name)
                }
            };
        },
        IfStmt(pos, ref cond, ref then_stmt, ref opt_else_stmt) => unsafe {
            set_debug_loc(ec, pos, dbg_scope);

            let cond_val = emit_expr(ec, lvm, *cond);
            let cond_val = do noname |name| {
                llvm::LLVMBuildTrunc(ec.builder, cond_val, type_i1(), name)
            };

            let cond_bb = llvm::LLVMGetInsertBlock(ec.builder);
            let func = llvm::LLVMGetBasicBlockParent(cond_bb);

            let then_bb = str::as_c_str("if_then", |name| {
                llvm::LLVMAppendBasicBlock(func, name)
            });
            let end_bb = str::as_c_str("if_end", |name| {
                llvm::LLVMAppendBasicBlock(func, name)
            });
            let else_bb = match *opt_else_stmt {
                Some(*) => str::as_c_str("if_else", |name| {
                    llvm::LLVMAppendBasicBlock(func, name)
                }),
                None => end_bb
            };

            llvm::LLVMBuildCondBr(ec.builder, cond_val, then_bb, else_bb);

            llvm::LLVMPositionBuilderAtEnd(ec.builder, then_bb);
            emit_stmt(ec, lvm, *then_stmt, dbg_scope);
            llvm::LLVMBuildBr(ec.builder, end_bb);

            match *opt_else_stmt {
                Some(ref else_stmt) => {
                    llvm::LLVMPositionBuilderAtEnd(ec.builder, else_bb);
                    emit_stmt(ec, lvm, *else_stmt, dbg_scope);
                    llvm::LLVMBuildBr(ec.builder, end_bb);
                },
                None => {}
            }

            llvm::LLVMPositionBuilderAtEnd(ec.builder, end_bb);
        },
        BlockStmt(_, ref block) => {
            for block.stmts.each |stmt| {
                emit_stmt(ec, lvm, stmt, dbg_scope);
            }
        }
    }
}

fn emit_expr(ec: &EmitterContext, lvm: &VarMap, expr: &Expr) -> ValueRef {
    match *expr {
        IntLiteralExpr(_, num) => const_integer(num),
        BooleanLiteralExpr(_, b) => const_boolean(b),
        VarValExpr(_, var_name) => {
            let var = get_var_pointer(ec, lvm, var_name);
            unsafe {
                do noname |name| { llvm::LLVMBuildLoad(ec.builder, var, name) }
            }
        },
        FunctionExpr(_, func_name, ref args) => {
            let func = get_proc_pointer(ec, func_name);  
            let largs = do args.map |arg| { emit_expr(ec, lvm, arg) };
            do noname |name| {
                unsafe {
                    llvm::LLVMBuildCall(ec.builder, func, 
                                        vec::raw::to_ptr(largs),
                                        args.len() as u32, name)
                }
            }
        },
        BinaryArithExpr(_, op, ref e1, ref e2) => {
            let le1 = emit_expr(ec, lvm, *e1);
            let le2 = emit_expr(ec, lvm, *e2);
            unsafe {
                do noname |name| {
                    match op {
                        AddOp => llvm::LLVMBuildAdd(ec.builder, le1, le2, name),
                        SubOp => llvm::LLVMBuildSub(ec.builder, le1, le2, name),
                        MulOp => llvm::LLVMBuildMul(ec.builder, le1, le2, name),
                        DivOp => llvm::LLVMBuildSDiv(ec.builder, le1, le2, name),
                        ModOp => llvm::LLVMBuildSRem(ec.builder, le1, le2, name)
                    }
                }
            }
        },
        UnaryArithExpr(_, op, ref e) => {
            let le = emit_expr(ec, lvm, *e);
            match op {
                PlusOp => le,
                MinusOp => do noname |name| {
                    unsafe {
                        llvm::LLVMBuildNeg(ec.builder, le, name)
                    }
                }
            }
        },
        BinaryLogicalExpr(_, op, ref e1, ref e2) => {
            let le1 = emit_expr(ec, lvm, *e1);
            let le2 = emit_expr(ec, lvm, *e2);
            unsafe {
                do noname |name| {
                    match op {
                        AndOp => llvm::LLVMBuildAnd(ec.builder, le1, le2, name),
                        OrOp => llvm::LLVMBuildOr(ec.builder, le1, le2, name)
                    }
                }
            }
        },
        NegationExpr(_, ref e) => {
            let le = emit_expr(ec, lvm, *e);
            unsafe {
                do noname |name| { llvm::LLVMBuildNot(ec.builder, le, name) }
            }
        },
        ComparisonExpr(_, op, ref e1, ref e2) => {
            let lop = match op {
                EqOp => rustllvm::IntEQ,
                NeOp => rustllvm::IntNE,
                LtOp => rustllvm::IntSLT,
                GtOp => rustllvm::IntSGT,
                LeOp => rustllvm::IntSLE,
                GeOp => rustllvm::IntSGE,
            } as c_uint;
            let le1 = emit_expr(ec, lvm, *e1);
            let le2 = emit_expr(ec, lvm, *e2);
            unsafe {
                let cmp = do noname |name| {
                    llvm::LLVMBuildICmp(ec.builder, lop, le1, le2, name)
                };
                do noname |name| {
                    llvm::LLVMBuildZExt(ec.builder, cmp, boolean_repr_type(), name)
                }
            }
        }
    }
}

// --== Debug location ==--

fn set_debug_loc(ec: &EmitterContext, pos: Position, dbg_scope: ValueRef) {
    let dbg_loc = md_node([const_i32(pos.line as i32),
                           const_i32(pos.col as i32),
                           dbg_scope,
                           md_null()]);
    unsafe {
        llvm::LLVMSetCurrentDebugLocation(ec.builder, dbg_loc);
    }
}

fn unset_debug_loc(ec: &EmitterContext) {
    unsafe {
        llvm::LLVMSetCurrentDebugLocation(ec.builder, md_null());
    }
}

// --== Hard-coded data layout and target specification for LLVM ==--

static data_layout_str: &'static str = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128";
static target_triple_str: &'static str = "x86_64-unknown-linux-gnu";

// --== Type representation and translation ==--

static integer_width : uint = 32;
static boolean_width : uint = 8;

#[inline(always)]
fn integer_repr_type() -> TypeRef {
    type_i32()
}

#[inline(always)]
fn boolean_repr_type() -> TypeRef {
    type_i8()
}

#[inline(always)]
fn const_integer(val : i32) -> ValueRef {
    const_i32(val)
}

#[inline(always)]
fn const_boolean(b : bool) -> ValueRef {
    const_i8(if b { 1i8 } else { 0i8 })
}

fn translate_type(ty: Type) -> TypeRef {
    match ty {
        IntegerType => integer_repr_type(),
        BooleanType => boolean_repr_type()
    }
}

fn get_type_dbg_node(ec: &EmitterContext, ty: Type) -> ValueRef {
    match ty {
        IntegerType => ec.dbg_integer,
        BooleanType => ec.dbg_boolean
    }
}

// --== ObjectMap helpers ==--

fn get_var_pointer(ec: &EmitterContext, lvm: &VarMap, var_name: @str) -> ValueRef {
    match lvm.find(&var_name) {
        Some(var) => *var,
        None => {
            match ec.var_map.find(&var_name) {
                Some(var) => *var,
                None => fail!(~"No variable pointer found.")
            }
        }
    }
}

fn get_proc_pointer(ec: &EmitterContext, proc_name: @str) -> ValueRef {
    match ec.proc_map.find(&proc_name) {
        Some(llproc) => *llproc,
        None => fail!(~"No procedure pointer found.")
    }
}

// --== DWARF tags ==--

static LLVMDebugVersion: i32 = (12 << 16);

fn vtag(tag: i32) -> ValueRef {
    const_i32(tag | LLVMDebugVersion)
}

static CompileUnitTag: i32 = 17;
static FileTag: i32 = 41;
static BasicTypeTag: i32 = 36;
static GlobalVarTag: i32 = 52;
static SubroutineTypeTag: i32 = 21;
static SubprogramTag: i32 = 46;

static DW_ATE_boolean: i32 = 2;
static DW_ATE_signed: i32 = 5;

// --== Functions to create debug info nodes ==--

fn mk_file_dbg_node(dirname: &str, filename: &str) -> ValueRef {
    md_node([vtag(FileTag),
             md_node([md_str(filename), md_str(dirname)])])
}

fn mk_compile_unit_dbg_node(dbg_empty: ValueRef, dbg_file: ValueRef,
                            dbg_subprog_list: ValueRef, dbg_var_list: ValueRef) -> ValueRef {
    md_node([vtag(CompileUnitTag),
             const_i32(0),          // unusued
             const_i32(9),          // Pascal
             dbg_file,
             md_str("rascal"),      // producer
             const_i1(false),       // optimized
             md_str(""),            // flags
             const_i32(0),          // runtime version
             dbg_empty,             // enum types
             dbg_empty,             // retained types
             dbg_subprog_list,      // subprograms
             dbg_var_list,          // global variables
             md_str("")])           // split debug file name
}

fn mk_basic_type_dbg_node(name: &str, size: uint, align: uint, encoding: i32) -> ValueRef {
    md_node([vtag(BasicTypeTag),
            md_null(),               // context
            md_str(name),          
            md_null(),               // file description
            const_i32(0),            // line number
            const_i64(size as i64),
            const_i64(align as i64),
            const_i64(0),            // offset
            const_i32(0),            // flags
            const_i32(encoding as i32)])
}

fn mk_subroutine_type_dbg_node(dbg_ret_ty: ValueRef, dbg_arg_tys: &[ValueRef]) -> ValueRef {
    let mut dbg_members = ~[dbg_ret_ty];
    dbg_members.push_all(dbg_arg_tys);
    let dbg_member_list = md_node(dbg_members);

    md_node([vtag(SubroutineTypeTag),
             const_i32(0),  // ?
             md_str(""),    // name
             const_i32(0),  // ?
             const_i32(0),  // line number
             const_i64(0),  // size
             const_i64(0),  // align
             const_i64(0),  // offset
             const_i32(0),  // flags
             md_null(),     // derived from
             dbg_member_list,
             const_i32(0),  // ?
             const_i32(0)]) // ?
}

fn mk_global_var_dbg_node(dbg_file: ValueRef, name: &str, dbg_ty: ValueRef,
                          var: ValueRef, line: uint) -> ValueRef {
    md_node([vtag(GlobalVarTag),
             const_i32(0),     // unused
             md_null(),        // context
             md_str(name),
             md_str(name),
             md_str(""),       // linkage name
             dbg_file,
             const_i32(line as i32),
             dbg_ty,
             const_i1(true),   // static
             const_i1(true),   // not extern
             var,
             md_null()])
}

// TODO: scope_line? what value to set?
fn mk_subprogram_dbg_node(dbg_empty: ValueRef, dbg_file: ValueRef, name: &str,
                          dbg_ty: ValueRef, subprog: ValueRef,
                          line: uint, scope_line: uint) -> ValueRef {
    md_node([vtag(SubprogramTag),
             const_i32(0),      // unused
             dbg_file ,         // context
             md_str(name),      // name
             md_str(name),      // display name
             md_str(""),        // link name
             dbg_file,
             const_i32(line as i32),
             dbg_ty,
             const_i1(false),   // static
             const_i1(true),    // not extern
             const_i32(0),      // virtuality
             const_i32(0),      // virtual index
             md_null(),         // type containing ptr to vtable
             const_i32(256),    // flags [TODO: CHECK]
             const_i1(false),   // optimized
             subprog,
             md_null(),         // template parameters
             md_null(),         // ?
             dbg_empty ,        // variables
             const_i32(scope_line as i32)])
}

// --== Various LLVM helpers ==--

fn create_module(name: &str) -> ModuleRef {
    unsafe {
        let ctx = llvm::LLVMGetGlobalContext();
        let module = str::as_c_str(name, |buf| {
            llvm::LLVMModuleCreateWithNameInContext(buf, ctx)
        });   
        str::as_c_str(data_layout_str, |buf| {
            llvm::LLVMSetDataLayout(module, buf);
        });
        str::as_c_str(target_triple_str, |buf| {
            llvm::LLVMSetTarget(module, buf);
        });
        module
    }
}    

fn add_named_metadata(module: ModuleRef, name: &str, md: ValueRef) {
    do str::as_c_str(name) |buf| {
        unsafe {
            llvm::LLVMAddNamedMetadataOperand(module, buf, md);
        }
    }
}

fn write_module_bitcode(module: ModuleRef, path: &Path) {
    unsafe {
        let err = str::as_c_str(path.to_str(), |buf| {
            llvm::LLVMWriteBitcodeToFile(module, buf)
        });
        if err != 0 {
            util::error("I/O error while writing output file.");
        }
    }
}
    
fn noname(f : &fn(*c_char) -> ValueRef) -> ValueRef {
    str::as_c_str("", f)
}

fn type_void() -> TypeRef {
    unsafe { llvm::LLVMVoidType() }
}

fn type_i1() -> TypeRef {
    unsafe { llvm::LLVMInt1Type() }
}

fn type_i8() -> TypeRef {
    unsafe { llvm::LLVMInt8Type() }
}

fn type_i32() -> TypeRef {
    unsafe { llvm::LLVMInt32Type() }
}

fn type_i64() -> TypeRef {
    unsafe { llvm::LLVMInt64Type() }
}

fn type_fn(ret_ty: TypeRef, arg_tys: &[TypeRef]) -> TypeRef {
    unsafe {
        llvm::LLVMFunctionType(ret_ty, vec::raw::to_ptr(arg_tys),
                               arg_tys.len() as c_uint, rustllvm::False)
    }
}

// --== LLVM constants ==--

fn const_zero(ty: TypeRef) -> ValueRef {
    unsafe {
        llvm::LLVMConstNull(ty)
    }
}

fn const_i1(val: bool) -> ValueRef {
    let numval = if val { 1 } else { 0 };
    unsafe {
        llvm::LLVMConstInt(type_i1(), numval as c_ulonglong, rustllvm::False)
    }
}

fn const_i8(val: i8) -> ValueRef {
    unsafe {
        llvm::LLVMConstInt(type_i8(), val as c_ulonglong, rustllvm::False)
    }
}

fn const_i32(val: i32) -> ValueRef {
    unsafe {
        llvm::LLVMConstInt(type_i32(), val as c_ulonglong, rustllvm::False)
    }
}

fn const_i64(val: i64) -> ValueRef {
    unsafe {
        llvm::LLVMConstInt(type_i64(), val as c_ulonglong, rustllvm::False)
    }
}

fn md_null() -> ValueRef {
    unsafe {
        cast::transmute(ptr::null::<ValueRef>())
    }
}

fn md_str(s: &str) -> ValueRef {
    do str::as_c_str(s) |buf| {
        unsafe {
            llvm::LLVMMDString(buf, s.len() as c_uint)
        }
    }
}

fn md_node(elems: &[ValueRef]) -> ValueRef {
    unsafe {
        llvm::LLVMMDNode(vec::raw::to_ptr(elems), elems.len() as c_uint)
    }
}
