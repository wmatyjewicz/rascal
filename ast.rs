// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// AST

use lexer::Position;

pub struct Program {
    src_path: @Path,
    defs: ~[Def],
    block: Block
}

pub enum Def {
    ProcDef(Proc),
    VarDef(Var)
}

pub struct Var {
    pos: Position,
    name: @str,
    ty: Type
}

pub struct Proc {
    pos: Position,
    name: @str,
    args: ~[Var],
    ret_ty: Option<Type>,
    vars: ~[Var],
    block: Block
}

pub struct Block {
    pos: Position,
    stmts: ~[Stmt]
}

pub enum Stmt {
    AssignmentStmt(Position, @str, ~Expr),
    CallStmt(Position, @str, ~[Expr]),
    IfStmt(Position, ~Expr, ~Stmt, Option<~Stmt>),
    BlockStmt(Position, Block)
}

pub enum Expr {
    IntLiteralExpr(Position, i32),
    BooleanLiteralExpr(Position, bool),
    VarValExpr(Position, @str),
    FunctionExpr(Position, @str, ~[Expr]),
    BinaryArithExpr(Position, BinaryArithOp, ~Expr, ~Expr),
    UnaryArithExpr(Position, UnaryArithOp, ~Expr),
    BinaryLogicalExpr(Position, BinaryLogicalOp, ~Expr, ~Expr),
    NegationExpr(Position, ~Expr),
    ComparisonExpr(Position, ComparisonOp, ~Expr, ~Expr)
}

pub enum BinaryArithOp {
    AddOp,
    SubOp,
    MulOp,
    DivOp,
    ModOp
}

pub enum UnaryArithOp {
    PlusOp,
    MinusOp
}

pub enum BinaryLogicalOp {
    AndOp,
    OrOp,
}

pub enum ComparisonOp {
    EqOp,
    NeOp,
    LtOp,
    GtOp,
    LeOp,
    GeOp
}

#[deriving(Eq)]
pub enum Type {
    IntegerType,
    BooleanType,
}
