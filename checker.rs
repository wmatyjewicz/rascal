// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// Semantic checker

use core::hashmap::linear::LinearMap;

use util;
use lexer::Position;
use ast::*;

enum DefType {
    VarDefType(Type),
    ProcDefType(~[Type], Option<Type>)
}

struct CheckerContext {
    src_path: @Path,
    def_map: LinearMap<@str, DefType>
}

type LocalVarMap = LinearMap<@str, Type>;

pub fn check_program(program: &Program) {
    let mut cc = CheckerContext {
        src_path: program.src_path,
        def_map: LinearMap::new()
    };

    for program.defs.each |def| {
        match *def {
            VarDef(ref var) => {
                check_global_ident(&cc, var.pos, var.name);
                cc.def_map.insert(var.name, VarDefType(var.ty));
            },
            ProcDef(ref proc) => {
                check_global_ident(&cc, proc.pos, proc.name);

                let mut arg_tys = ~[];
                let mut lvm : LocalVarMap = LinearMap::new();
                match proc.ret_ty {
                    Some(ty) => { lvm.insert(proc.name, ty); },
                    None => {}
                };
                for proc.args.each |arg| {
                    arg_tys.push(arg.ty);
                    if (lvm.contains_key(&arg.name)) {
                        error_at_pos(&cc, arg.pos, "Duplicate local identifier.");
                    }
                    lvm.insert(arg.name, arg.ty);
                }
                for proc.vars.each |var| {
                    if (lvm.contains_key(&var.name)) {
                        error_at_pos(&cc, var.pos, "Duplicate local identifier.");
                    }
                    lvm.insert(var.name, var.ty);
                }

                cc.def_map.insert(proc.name, ProcDefType(arg_tys, proc.ret_ty));

                for proc.block.stmts.each |stmt| {
                    check_stmt(&cc, &lvm, stmt);
                }
            }
        }
    }
        
    let empty_lvm = LinearMap::new();
    for program.block.stmts.each |stmt| {
        check_stmt(&cc, &empty_lvm, stmt);
    }
}

fn check_stmt(cc: &CheckerContext, lvm: &LocalVarMap, stmt: &Stmt) {
    match *stmt {
        AssignmentStmt(pos, var_name, ref expr) => {
            let var_ty = match lvm.find(&var_name) {
                Some(ty) => *ty,
                None => find_global_var_type(cc, pos, var_name)
            };
            let expr_ty = check_expr(cc, lvm, *expr);
            if var_ty != expr_ty {
                error_at_pos(cc, pos, "Type mismatch.");
            }
        },
        CallStmt(pos, proc_name, ref args) => {
            let arg_formal_tys = find_proc_arg_types(cc, pos, proc_name);
            check_args(cc, lvm, pos, arg_formal_tys, *args);
        },
        IfStmt(pos, ref cond, ref then_stmt, ref opt_else_stmt) => {
            let cond_ty = check_expr(cc, lvm, *cond);
            if cond_ty != BooleanType {
                error_at_pos(cc, pos, "Condition type mismatch.");
            }
            check_stmt(cc, lvm, *then_stmt);
            match *opt_else_stmt {
                Some(ref else_stmt) => check_stmt(cc, lvm, *else_stmt),
                None => {}
            }
        },
        BlockStmt(_, ref block) => {
            for block.stmts.each |stmt| {
                check_stmt(cc, lvm, stmt);
            }
        }
    }
}

fn check_expr(cc: &CheckerContext, lvm: &LocalVarMap, expr: &Expr) -> Type {
    match *expr {
        IntLiteralExpr(*) => IntegerType,
        BooleanLiteralExpr(*) => BooleanType,
        VarValExpr(pos, var_name) => {
            match lvm.find(&var_name) {
                Some(ty) => *ty,
                None => find_global_var_type(cc, pos, var_name)
            }
        },
        FunctionExpr(pos, func_name, ref args) => {
            let (arg_formal_tys, ret_ty) = find_func_types(cc, pos, func_name);
            check_args(cc, lvm, pos, arg_formal_tys, *args);
            ret_ty
        },
        BinaryArithExpr(pos, _, ref e1, ref e2) => {
            check_operand(cc, lvm, pos, IntegerType, *e1);
            check_operand(cc, lvm, pos, IntegerType, *e2);
            IntegerType
        },
        UnaryArithExpr(pos, _, ref e) => {
            check_operand(cc, lvm, pos, IntegerType, *e);
            IntegerType
        },
        BinaryLogicalExpr(pos, _, ref e1, ref e2) => {
            check_operand(cc, lvm, pos, BooleanType, *e1);
            check_operand(cc, lvm, pos, BooleanType, *e2);
            BooleanType
        },
        NegationExpr(pos, ref e) => {
            check_operand(cc, lvm, pos, BooleanType, *e);
            BooleanType
        },
        ComparisonExpr(pos, _, ref e1, ref e2) => {
            check_operand(cc, lvm, pos, IntegerType, *e1);
            check_operand(cc, lvm, pos, IntegerType, *e2);
            BooleanType
        }
    }
}

fn check_args(cc: &CheckerContext, lvm: &LocalVarMap, pos: Position,
              arg_formal_tys: &[Type], args: &[Expr]) {
    if args.len() != arg_formal_tys.len() {
        error_at_pos(cc, pos, "Invalid number of arguments.");
    }
    let mut i = 0;
    for args.each |arg| {
        let arg_actual_ty = check_expr(cc, lvm, arg);
        if arg_actual_ty != arg_formal_tys[i] {
            error_at_pos(cc, pos, "Argument type mismatch.");
        }
        i += 1;
    }
}

fn check_operand(cc: &CheckerContext, lvm: &LocalVarMap, pos: Position,
                 expected_ty: Type, expr: &Expr) {
    let ty = check_expr(cc, lvm, expr);
    if ty != expected_ty {
        error_at_pos(cc, pos, "Invalid operand type.");
    }
}

fn find_global_var_type(cc: &CheckerContext, pos: Position, var_name: @str) -> Type {
    match cc.def_map.find(&var_name) {
        Some(&VarDefType(ty)) => ty,
        _ => error_at_pos(cc, pos, "Unknown variable identifier.")
    }
}

fn find_proc_arg_types(cc: &CheckerContext, pos: Position, proc_name: @str) -> ~[Type] {
    match cc.def_map.find(&proc_name) {
        // TODO: can we skip vector copying?
        Some(&ProcDefType(ref arg_tys, _)) => copy *arg_tys,
        _ => error_at_pos(cc, pos, "Unknown procedure/function identifier.")
    }
}

fn find_func_types(cc: &CheckerContext, pos: Position, func_name: @str) -> (~[Type], Type) {
    match cc.def_map.find(&func_name) {
        // TODO: can we skip vector copying?
        Some(&ProcDefType(ref arg_tys, Some(ret_ty))) => (copy *arg_tys, ret_ty),
        Some(&ProcDefType(_, None)) => error_at_pos(cc, pos, "Procedure does not return a value."),
        _ => error_at_pos(cc, pos, "Unknown function identifier.")
    }
}

fn check_global_ident(cc: &CheckerContext, pos: Position, name: @str) {
    if "main" == name {
        error_at_pos(cc, pos, "\"main\" is a reserved global identifier.");
    }
    if (cc.def_map.contains_key(&name)) {
        error_at_pos(cc, pos, "Duplicate global identifier.");
    }
}

fn error_at_pos(cc: &CheckerContext, pos: Position, msg: &str) -> ! {
    util::error(fmt!("%s:%u:%u: %s", cc.src_path.to_str(), pos.line, pos.col, msg));
}
