// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// LLVM IR emitter

use core::libc::{c_char, c_uint, c_ulonglong};
use core::hashmap::linear::LinearMap;

use rustllvm::{ModuleRef, TypeRef, ValueRef, BuilderRef};
use rustllvm::llvm;
use rustllvm;

use util;
use lexer::Position;
use ast::*;

static data_layout_str: &'static str = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128";

static target_triple_str: &'static str = "x86_64-unknown-linux-gnu";

static LLVMDebugVersion: i32 = (12 << 16);

static CompileUnitTag: i32 = 17;
static FileTag: i32 = 41;
static BasicTypeTag: i32 = 36;
static GlobalVarTag: i32 = 52;
static SubroutineTypeTag: i32 = 21;
static SubprogramTag: i32 = 46;
static LexicalBlockTag: i32 = 11;

static DW_ATE_boolean: int = 2;
static DW_ATE_signed: int = 5;

type ObjectMap = LinearMap<@str, ValueRef>;

priv struct EmitterContext {
    llmod: ModuleRef,
    llbuilder: BuilderRef,

    var_map: ObjectMap,
    proc_map: ObjectMap,

    mdempty: ValueRef,
    mdinteger: ValueRef,
    mdboolean: ValueRef,
    mdfile: ValueRef,

    mdvars: ~[ValueRef],
    mdsubprogs: ~[ValueRef]
}

pub fn emit_program(bc_path: &Path, program: &Program) {
    let dirname = program.src_path.dirname();
    let filename = program.src_path.filename().get();

    let mdempty = mdnode([const_i32(0)]);
    let mdinteger = basic_type_description("integer", 32, 32, DW_ATE_signed);
    let mdboolean = basic_type_description("boolean", 1, 1, DW_ATE_boolean);
    
    let mdfile = mdnode([dbgtag(FileTag),
                         mdstr(filename),
                         mdstr(dirname),
                         mdnull()]);

    let mut ec = unsafe {
        let llctx = llvm::LLVMGetGlobalContext();
    
        let llmod = str::as_c_str(filename, |buf| {
            llvm::LLVMModuleCreateWithNameInContext(buf, llctx)
        });   
        str::as_c_str(data_layout_str, |buf| {
            llvm::LLVMSetDataLayout(llmod, buf);
        });
        str::as_c_str(target_triple_str, |buf| {
            llvm::LLVMSetTarget(llmod, buf);
        });

        let llbuilder = llvm::LLVMCreateBuilder();

        EmitterContext {
            llmod: llmod,
            llbuilder: llbuilder,

            var_map: LinearMap::new(),

            mdempty: mdempty,
            mdinteger: mdinteger,
            mdboolean: mdboolean,
            mdfile: mdfile,

            mdvars: ~[],
            mdsubprogs: ~[]
        }
    };

    for program.defs.each |def| {
        match *def {
            ProcDef(ref proc) => emit_proc(&mut ec, proc),
            VarDef(ref var) => emit_global_var(&mut ec, var)
        }
    }

    main_proc = Proc {
        pos: program.block.pos,
        name: @"main",
        args: ~[],
        ret_ty: Some<IntegerType>,
        vars: ~[],
        block: program.block
    }
    emit_proc(&mut ec, &main_proc);

    let mdsubprogs = mdnode(ec.mdsubprogs);
    let mdvars =
        if ec.mdvars.is_empty() { ec.mdempty } else { mdnode(ec.mdvars) };
    
    let mdcu = mdnode([dbgtag(CompileUnitTag),
                       const_i32(0),          // unusued
                       const_i32(9),          // Pascal
                       mdstr(filename),
                       mdstr(dirname),
                       mdstr("tp"),           // producer
                       const_i1(true),        // main
                       const_i1(false),       // optimized
                       mdstr(""),             // flags
                       const_i32(0),          // runtime version
                       mdnode([mdempty]),     // enum types
                       mdnode([mdempty]),     // retained types
                       mdnode([mdsubprogs]),  // subprograms
                       mdnode([mdvars])]);    // global variables

    emit_named_metadata(&ec, "llvm.dbg.cu", mdcu);

    unsafe {
        let err = str::as_c_str(bc_path.to_str(), |buf| {
            llvm::LLVMWriteBitcodeToFile(ec.llmod, buf)
        });
        if err != 0 {
            util::error("I/O error while writing output file.");
        }
    }
}

// TODO: name
// TODO: uint || int?
fn basic_type_description(name: &str, size: int, align: int, encoding: int) -> ValueRef {
    mdnode([dbgtag(BasicTypeTag),
            mdnull(),         // context
            mdstr(name),          
            mdnull(),         // file description
            const_i32(0),     // line number
            const_i64(size as i64),
            const_i64(align as i64),
            const_i64(0),     // offset
            const_i32(0),     // flags
            const_i32(encoding as i32)])
}

// TODO: name
// TODO: arg name
fn subroutine_type_description(file_node: ValueRef, member_nodes: &[ValueRef]) -> ValueRef {
    mdnode([dbgtag(SubroutineTypeTag),
            mdnull(),      // context
            mdstr(""),     // name
            mdnull(),      // file
            const_i32(0),  // line number
            const_i64(0),  // size
            const_i64(0),  // align
            const_i64(0),  // offset
            const_i32(0),  // flags
            mdnull(),      // derived from
            mdnode(member_nodes),
            const_i32(0)]) // runtime language
}

fn subprogram_description(name: &str, file_node: ValueRef, ty_node: ValueRef,
                          ptr: ValueRef, empty_node: ValueRef,
                          line: uint, scope_line: uint) -> ValueRef {
    mdnode([dbgtag(SubprogramTag),
            const_i32(0),    // unused
            file_node,       // context
            mdstr(name),     // name
            mdstr(name),     // display name
            mdstr(""),       // link name
            file_node,
            const_i32(line as i32),
            ty_node,
            const_i1(false), // static
            const_i1(true),  // not extern
            const_i32(0),    // virtuality
            const_i32(0),    // virtual index
            mdnull(),        // type containing ptr to vtable
            const_i32(256),  // flags [TODO: CHECK]
            const_i1(false), // optimized
            ptr,
            mdnull(),        // template parameters
            mdnull(),        // TODO: declaration? when?
            empty_node,      // variables
            const_i32(scope_line as i32)])
}

// TODO: name
fn variable_description(name: &str, file_node: ValueRef, line: uint, type_node: ValueRef,
                        ptr: ValueRef) -> ValueRef {
    mdnode([dbgtag(GlobalVarTag),
            const_i32(0),    // unused
            mdnull(),        // context
            mdstr(name),
            mdstr(name),
            mdstr(""),       // linkage name
            file_node,
            const_i32(line as i32),
            type_node,
            const_i1(true),  // static
            const_i1(true),  // not extern
            ptr])
}

fn lexical_block(mdcontext: ValueRef, pos: Position,
                 mdfile: ValueRef) -> ValueRef {
    mdnode([dbgtag(LexicalBlockTag),
            mdcontext,
            const_i32(pos.line as i32),
            const_i32(pos.col as i32),
            mdfile])
}

fn emit_global_var(ec: &mut EmitterContext, var: &Var) {
    let llty = translate_type(var.ty);
    let llvar = unsafe {
        let llvar = str::as_c_str(var.name, |name| {
            llvm::LLVMAddGlobal(ec.llmod, llty, name)
        });
        rustllvm::SetLinkage(llvar, rustllvm::InternalLinkage);
        let llzero = llvm::LLVMConstNull(llty);
        llvm::LLVMSetInitializer(llvar, llzero);
        llvar
    };
    ec.var_map.insert(var.name, llvar);

    let mdvar = variable_description(var.name, ec.mdfile, var.pos.line,
                                     ec.mdinteger, llvar);
    ec.mdvars.push(mdvar);
}

fn emit_proc(ec: &mut EmitterContext, proc: &Proc) {
    let (llretty, mdretty) = match proc.ret_ty {
        Some(ty) => (translate_type(ty), translate_mdtype(ec, ty)),
        None => (type_void(), ec.mdnull)
    }
    let llargtys = proc.args.map |arg| { translate_type(arg.ty) }
    let llty = type_fn(llret_ty, llarg_tys);

    unsafe {
        let llmain = str::as_c_str(proc.name, |name| {
            llvm::LLVMAddFunction(ec.llmod, name, llty)
        });

        let mdtype = subroutine_type_description(ec.mdfile, [ec.mdinteger]);
        let mdfunc = subprogram_description("main", ec.mdfile, mdtype,
                                            llmain, mdnode([ec.mdempty]), pos.line,
                                            pos.line);
        ec.mdsubprogs.push(mdfunc);

        let mdblock = lexical_block(mdfunc, pos, ec.mdfile);

        let llentry = str::as_c_str("entry", |name| {
            llvm::LLVMAppendBasicBlock(llmain, name)
        });
        llvm::LLVMPositionBuilderAtEnd(ec.llbuilder, llentry);

        for stmts.each |stmt| {
            emit_stmt(ec, stmt, mdblock);
        }
    
        unset_debug_loc(ec);

        llvm::LLVMBuildRet(ec.llbuilder, llvm::LLVMConstNull(type_i32()));
    }
}

fn emit_main_func(ec: &mut EmitterContext, stmts: &[Stmt], pos: Position) {
    let llty = type_fn(type_i32(), []);
    unsafe {
        let llmain = str::as_c_str("main", |name| {
            llvm::LLVMAddFunction(ec.llmod, name, llty)
        });

        let mdtype = subroutine_type_description(ec.mdfile, [ec.mdinteger]);
        let mdfunc = subprogram_description("main", ec.mdfile, mdtype,
                                            llmain, mdnode([ec.mdempty]), pos.line,
                                            pos.line);
        ec.mdsubprogs.push(mdfunc);

        let mdblock = lexical_block(mdfunc, pos, ec.mdfile);

        let llentry = str::as_c_str("entry", |name| {
            llvm::LLVMAppendBasicBlock(llmain, name)
        });
        llvm::LLVMPositionBuilderAtEnd(ec.llbuilder, llentry);

        for stmts.each |stmt| {
            emit_stmt(ec, stmt, mdblock);
        }
    
        unset_debug_loc(ec);

        llvm::LLVMBuildRet(ec.llbuilder, llvm::LLVMConstNull(type_i32()));
    }
}

fn emit_stmt(ec: &EmitterContext, stmt: &Stmt, scope_node: ValueRef) {
    match *stmt {
        AssignmentStmt(pos, var_name, ref expr) => {
            set_debug_loc(ec, pos, scope_node);

            let llvar = get_var_pointer(ec, var_name);
            let llval = emit_expr(ec, *expr);
            unsafe {
                llvm::LLVMBuildStore(ec.llbuilder, llval, llvar)
            };
        },
        IfStmt(pos, ref cond, ref then_stmt, ref opt_else_stmt) => unsafe {
            set_debug_loc(ec, pos, scope_node);

            let llcond = emit_expr(ec, *cond);

            let llcondbb = llvm::LLVMGetInsertBlock(ec.llbuilder);
            let llfunc = llvm::LLVMGetBasicBlockParent(llcondbb);

            let llthenbb = str::as_c_str("if_then", |name| {
                llvm::LLVMAppendBasicBlock(llfunc, name)
            });
            let llendbb = str::as_c_str("if_end", |name| {
                llvm::LLVMAppendBasicBlock(llfunc, name)
            });
            let llelsebb = match *opt_else_stmt {
                Some(*) => str::as_c_str("if_else", |name| {
                    llvm::LLVMAppendBasicBlock(llfunc, name)
                }),
                None => llendbb
            };

            llvm::LLVMBuildCondBr(ec.llbuilder, llcond, llthenbb, llelsebb);

            llvm::LLVMPositionBuilderAtEnd(ec.llbuilder, llthenbb);
            emit_stmt(ec, *then_stmt, scope_node);
            llvm::LLVMBuildBr(ec.llbuilder, llendbb);

            match *opt_else_stmt {
                Some(ref else_stmt) => {
                    llvm::LLVMPositionBuilderAtEnd(ec.llbuilder, llelsebb);
                    emit_stmt(ec, *else_stmt, scope_node);
                    llvm::LLVMBuildBr(ec.llbuilder, llendbb);
                },
                None => {}
            }

            llvm::LLVMPositionBuilderAtEnd(ec.llbuilder, llendbb);
        },
        BlockStmt(_, ref stmts) => {
            for stmts.each |stmt| {
                emit_stmt(ec, stmt, scope_node);
            }
        }
    }
}

fn set_debug_loc(ec: &EmitterContext, pos: Position, scope_node: ValueRef) {
    let loc_node = mdnode([const_i32(pos.line as i32),
                           const_i32(pos.col as i32),
                           scope_node,
                           mdnull()]);
    unsafe {
        llvm::LLVMSetCurrentDebugLocation(ec.llbuilder, loc_node);
    }
}

fn unset_debug_loc(ec: &EmitterContext) {
    unsafe {
        llvm::LLVMSetCurrentDebugLocation(ec.llbuilder, mdnull());
    }
}

fn emit_expr(ec: &EmitterContext, expr: &Expr) -> ValueRef {
    match *expr {
        IntLiteralExpr(_, num) => const_i64(num),
        VarValExpr(_, var_name) => unsafe {
            let llvar = get_var_pointer(ec, var_name);
            do noname |name| { llvm::LLVMBuildLoad(ec.llbuilder, llvar, name) }
        },
        BinaryArithExpr(_, op, ref e1, ref e2) => unsafe {
            let lle1 = emit_expr(ec, *e1);
            let lle2 = emit_expr(ec, *e2);
            do noname |name| {
                match op {
                    AddOp => llvm::LLVMBuildAdd(ec.llbuilder, lle1, lle2, name),
                    SubOp => llvm::LLVMBuildSub(ec.llbuilder, lle1, lle2, name),
                    MulOp => llvm::LLVMBuildMul(ec.llbuilder, lle1, lle2, name),
                    DivOp => llvm::LLVMBuildSDiv(ec.llbuilder, lle1, lle2, name),
                    ModOp => llvm::LLVMBuildSRem(ec.llbuilder, lle1, lle2, name)
                }
            }
        },
        UnaryArithExpr(_, op, ref e) => unsafe {
            let lle = emit_expr(ec, *e);
            match op {
                PlusOp => lle,
                MinusOp => do noname |name| {
                    llvm::LLVMBuildNeg(ec.llbuilder, lle, name)
                }
            }
        },
        BinaryLogicalExpr(_, op, ref e1, ref e2) => unsafe {
            let lle1 = emit_expr(ec, *e1);
            let lle2 = emit_expr(ec, *e2);
            do noname |name| {
                match op {
                    AndOp => llvm::LLVMBuildAnd(ec.llbuilder, lle1, lle2, name),
                    OrOp => llvm::LLVMBuildOr(ec.llbuilder, lle1, lle2, name)
                }
            }
        },
        NegationExpr(_, ref e) => unsafe {
            let lle = emit_expr(ec, *e);
            do noname |name| { llvm::LLVMBuildNot(ec.llbuilder, lle, name) }
        },
        ComparisonExpr(_, op, ref e1, ref e2) => {
            let llop = match op {
                EqOp => rustllvm::IntEQ,
                NeOp => rustllvm::IntNE,
                LtOp => rustllvm::IntSLT,
                GtOp => rustllvm::IntSGT,
                LeOp => rustllvm::IntSLE,
                GeOp => rustllvm::IntSGE,
            } as c_uint;
            unsafe {
                let lle1 = emit_expr(ec, *e1);
                let lle2 = emit_expr(ec, *e2);
                do noname |name| {
                    llvm::LLVMBuildICmp(ec.llbuilder, llop, lle1, lle2, name)
                }
            }
        }
    }
}

fn emit_named_metadata(ec: &EmitterContext, name: &str, md: ValueRef) {
    do str::as_c_str(name) |buf| {
        unsafe {
            llvm::LLVMAddNamedMetadataOperand(ec.llmod, buf, md);
        }
    }
}

fn translate_type(ty: Type) -> TypeRef {
    match ty {
        IntegerType => type_i32(),
        BooleanType => type_i1()
    }
}

fn translate_mdtype(ty: Type) -> TypeRef {
    match ty {
        IntegerType => type_i32(),
        BooleanType => type_i1()
    }
}

fn get_var_pointer(ec: &EmitterContext, var_name: @str) -> ValueRef {
    match ec.var_map.find(&var_name) {
        Some(llvar) => *llvar,
        None => fail!(~"No variable pointer found.")
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

fn type_i32() -> TypeRef {
    unsafe { llvm::LLVMInt32Type() }
}

fn type_fn(ret_ty: TypeRef, arg_tys: &[TypeRef]) -> TypeRef {
    unsafe {
        llvm::LLVMFunctionType(ret_ty, vec::raw::to_ptr(arg_tys),
                               arg_tys.len() as c_uint, rustllvm::False)
    }
}

fn const_bool(val: bool) -> ValueRef {
    let numval = if val { 1 } else { 0 };
    unsafe {
        llvm::LLVMConstInt(type_i1(), numval as c_ulonglong, rustllvm::False)
    }
}

fn const_i32(val: i32) -> ValueRef {
    unsafe {
        llvm::LLVMConstInt(type_i32(), val as c_ulonglong, rustllvm::False)
    }
}

fn mdstr(s: &str) -> ValueRef {
    do str::as_c_str(s) |buf| {
        unsafe {
            llvm::LLVMMDString(buf, s.len() as c_uint)
        }
    }
}

fn mdnull() -> ValueRef {
    unsafe {
        cast::reinterpret_cast(&ptr::null::<ValueRef>())
    }
}

fn mdnode(elems: &[ValueRef]) -> ValueRef {
    unsafe {
        llvm::LLVMMDNode(vec::raw::to_ptr(elems), elems.len() as c_uint)
    }
}

fn dbgtag(tag: i32) -> ValueRef {
    const_i32(LLVMDebugVersion | tag)
}