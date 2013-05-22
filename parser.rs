// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// Parser

use util;
use token::*;
use lexer::Lexer;
use ast::*;

pub fn parse_program(lexer: &mut Lexer) -> Program {
    let mut defs = ~[];
    loop {
        match lexer.token {
            KW_PROCEDURE | KW_FUNCTION => {
                let is_func = lexer.token == KW_FUNCTION;
                lexer.consume();
                let pos = lexer.token_pos;
                let name = parse_ident(lexer);
                let args = parse_arg_def_list(lexer);
                let ret_ty = if is_func {
                    parse_token(lexer, COLON);
                    Some(parse_type(lexer))
                } else {
                    None
                };
                let vars = parse_opt_vars_def(lexer);
                let block = parse_block(lexer);
                let proc = Proc {
                    pos : pos,
                    name : name,
                    args : args,
                    ret_ty : ret_ty,
                    vars : vars,
                    block : block
                };
                defs.push(ProcDef(proc));
            },
            KW_VAR => {
                let vars = parse_opt_vars_def(lexer);
                for vars.each |var| {
                    defs.push(VarDef(*var));
                }
            }
            _ => break
        }
    }

    let block = parse_block(lexer);
    parse_token(lexer, DOT);

    Program { src_path: lexer.src_path, defs: defs, block: block }
}

fn parse_arg_def_list(lexer: &mut Lexer) -> ~[Var] {
    let mut vars = ~[];
    parse_token(lexer, LPAREN);
    match lexer.token {
        IDENT(_) => {
            vars.push(parse_var_def(lexer));
            while lexer.token == COMMA {
                lexer.consume();
                vars.push(parse_var_def(lexer));
            }
        },
        _ => {}
    }
    parse_token(lexer, RPAREN);
    vars
}

fn parse_opt_vars_def(lexer: &mut Lexer) -> ~[Var] {
    if lexer.token != KW_VAR {
        return ~[];
    }
    lexer.consume();
    let mut vars = ~[];
    loop {
        match lexer.token {
            IDENT(_) => {
                vars.push(parse_var_def(lexer));
                parse_token(lexer, SEMICOLON);
            },
            _ => break
        }
    }
    vars
}

fn parse_var_def(lexer: &mut Lexer) -> Var {
    let pos = lexer.token_pos;
    let name = parse_ident(lexer);
    parse_token(lexer, COLON);
    let ty = parse_type(lexer);
    Var { pos : pos, name : name, ty : ty }
}

fn parse_block(lexer: &mut Lexer) -> Block {
    let mut stmts = ~[];
    let pos = lexer.token_pos;
    parse_token(lexer, KW_BEGIN);
    if lexer.token != KW_END {
        stmts.push(parse_stmt(lexer));
        loop {
            match lexer.token {
                KW_END => break,
                SEMICOLON => {
                    lexer.consume();
                    match lexer.token {
                        KW_END => break,
                        _ => stmts.push(parse_stmt(lexer))
                    }
                },
                _ => break
            }
        }
    }
    parse_token(lexer, KW_END);
    Block { pos : pos, stmts: stmts }
}

// TODO: Change to -> ~Stmt?
fn parse_stmt(lexer: &mut Lexer) -> Stmt {
    let pos = lexer.token_pos;
    match lexer.token {
        KW_READLN => {
            lexer.consume();
            parse_token(lexer, LPAREN);
            let s = parse_ident(lexer);
            parse_token(lexer, RPAREN);
            ReadLnStmt(pos, s)
        },
        KW_WRITELN => {
            lexer.consume();
            parse_token(lexer, LPAREN);
            let val = parse_expr(lexer);
            parse_token(lexer, RPAREN);
            WriteLnStmt(pos, ~val)
        },
        IDENT(s) => {
            lexer.consume();
            if lexer.token == LPAREN {
                CallStmt(pos, s, parse_arg_val_list(lexer))
            } else {
                parse_token(lexer, ASSIGN);
                AssignmentStmt(pos, s, ~parse_expr(lexer))
            }
        },
        KW_IF => {
            lexer.consume();
            let cond = parse_expr(lexer);
            parse_token(lexer, KW_THEN);
            let then_stmt = parse_stmt(lexer);
            let opt_else_stmt = if (lexer.token == KW_ELSE) {
                lexer.consume();
                Some(~parse_stmt(lexer))
            } else {
                None
            };
            IfStmt(pos, ~cond, ~then_stmt, opt_else_stmt)
        },
        KW_WHILE => {
            lexer.consume();
            let cond = parse_expr(lexer);
            parse_token(lexer, KW_DO);
            let do_stmt = parse_stmt(lexer);
            WhileStmt(pos, ~cond, ~do_stmt)
        },
        KW_BEGIN => BlockStmt(pos, parse_block(lexer)),
        _ => error_at_token(lexer, "Statement expected.")
    }
}

// TODO: Change to -> ~Expr?
fn parse_expr(lexer: &mut Lexer) -> Expr {
    let e1 = parse_simple_expr(lexer);
    let op = match lexer.token {
        EQ => EqOp,
        NE => NeOp,
        LT => LtOp,
        GT => GtOp,
        LE => LeOp,
        GE => GeOp,
        _ => return e1
    };
    let pos = lexer.token_pos;
    lexer.consume();
    let e2 = parse_simple_expr(lexer);
    ComparisonExpr(pos, op, ~e1, ~e2)
}

fn parse_simple_expr(lexer: &mut Lexer) -> Expr {
    let mut e = parse_term(lexer);
    while is_additive(lexer.token) {
        let pos = lexer.token_pos;
        let op = lexer.token;
        lexer.consume();
        let e2 = parse_term(lexer);

        e = match op {
            PLUS => BinaryArithExpr(pos, AddOp, ~e, ~e2),
            MINUS => BinaryArithExpr(pos, SubOp, ~e, ~e2),
            KW_OR => BinaryLogicalExpr(pos, OrOp, ~e, ~e2),
            _ => fail!(~"Unknown additive operator token")
        }
    }
    e
}

fn is_additive(token: Token) -> bool {
    match token {
        PLUS | MINUS | KW_OR => true,
        _ => false
    }
}

fn parse_term(lexer: &mut Lexer) -> Expr {
    let mut e = parse_signed_factor(lexer);
    while is_multiplicative(lexer.token) {
        let pos = lexer.token_pos;
        let op = lexer.token;
        lexer.consume();
        let e2 = parse_signed_factor(lexer);

        e = match op {
            TIMES => BinaryArithExpr(pos, MulOp, ~e, ~e2),
            KW_DIV => BinaryArithExpr(pos, DivOp, ~e, ~e2),
            KW_MOD => BinaryArithExpr(pos, ModOp, ~e, ~e2),
            KW_AND => BinaryLogicalExpr(pos, AndOp, ~e, ~e2),
            _ => fail!(~"Unknown multiplicative operator token")
        }
    }
    e
}

fn is_multiplicative(token: Token) -> bool {
    match token {
        TIMES | KW_DIV | KW_MOD | KW_AND => true,
        _ => false
    }
}

fn parse_signed_factor(lexer: &mut Lexer) -> Expr {
    let pos = lexer.token_pos;
    match lexer.token {
        PLUS => {
            lexer.consume();
            UnaryArithExpr(pos, PlusOp, ~parse_factor(lexer))
        },
        MINUS => {
            lexer.consume();
            UnaryArithExpr(pos, MinusOp, ~parse_factor(lexer))
        },
        KW_NOT => {
            lexer.consume();
            NegationExpr(pos, ~parse_factor(lexer))
        },
        _ => parse_factor(lexer)
    }
}

fn parse_factor(lexer: &mut Lexer) -> Expr {
    let pos = lexer.token_pos;
    match lexer.token {
        INT_LITERAL(num) => {
            lexer.consume();
            IntLiteralExpr(pos, num)
        },
        KW_TRUE => {
            lexer.consume();
            BooleanLiteralExpr(pos, true)
        },
        KW_FALSE => {
            lexer.consume();
            BooleanLiteralExpr(pos, false)
        },
        IDENT(s) => {
            lexer.consume();
            if lexer.token == LPAREN {
                FunctionExpr(pos, s, parse_arg_val_list(lexer))
            } else {
                VarValExpr(pos, s)
            }
        },
        LPAREN => {
            lexer.consume();
            let e = parse_expr(lexer);
            parse_token(lexer, RPAREN);
            e
        }
        _ => error_at_token(lexer, "Factor expected.")
    }
}

fn parse_arg_val_list(lexer: &mut Lexer) -> ~[Expr] {
    let mut args = ~[];
    parse_token(lexer, LPAREN);
    if lexer.token != RPAREN {
        args.push(parse_expr(lexer));
        while lexer.token == COMMA {
            lexer.consume();
            args.push(parse_expr(lexer));
        }
    }
    parse_token(lexer, RPAREN);
    args
}

fn parse_type(lexer: &mut Lexer) -> Type {
    let ty = match lexer.token {
        KW_INTEGER => IntegerType,
        KW_BOOLEAN => BooleanType,
        _ => error_at_token(lexer, "Type expected.")
    };
    lexer.consume();
    ty
}

fn parse_ident(lexer: &mut Lexer) -> @str {
    match lexer.token {
        IDENT(s) => {
            lexer.consume();
            s
        },
        _ => error_at_token(lexer, "Identifier expected.")
    }
}

fn parse_token(lexer: &mut Lexer, token: Token) {
    if lexer.token != token {
        if lexer.token == EOF {
            error_at_token(lexer, "Unexpected end of file.");
        } else {
            error_at_token(lexer, "Unexpected token.");
        }
    }
    lexer.consume();
}

fn error_at_token(lexer: &Lexer, msg: &str) -> ! {
    util::error(fmt!("%s:%u:%u: %s", lexer.src_path.to_str(), lexer.token_pos.line,
                     lexer.token_pos.col, msg));
}
