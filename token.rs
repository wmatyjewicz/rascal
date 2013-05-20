// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// Tokens

use core::hashmap::HashMap;

#[deriving(Eq)]
pub enum Token {
    EOF,

    // Arithmetic operators
    PLUS,
    MINUS,
    TIMES,

    // Comparison operators
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,

    // Delimiters etc.
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    ASSIGN,
    DOT,
    COMMA,
    COLON,
    SEMICOLON,
    TO,

    // Keywords
    KW_AND,
    KW_BEGIN,
    KW_BOOLEAN,
    KW_DIV,
    KW_ELSE,
    KW_END,
    KW_FALSE,
    KW_FUNCTION,
    KW_IF,
    KW_INTEGER,
    KW_MOD,
    KW_NOT,
    KW_OR,
    KW_PROCEDURE,
    KW_READLN,
    KW_THEN,
    KW_TRUE,
    KW_VAR,
    KW_WRITELN,
    
    // Identifier
    IDENT(@str),

    // Literals
    INT_LITERAL(i32)
}

pub type KeywordMap = HashMap<~str, Token>;

pub fn get_keyword_map() -> KeywordMap {
    let mut kw_map = HashMap::new();
    kw_map.insert(~"and", KW_AND);
    kw_map.insert(~"begin", KW_BEGIN);
    kw_map.insert(~"boolean", KW_BOOLEAN);
    kw_map.insert(~"div", KW_DIV);
    kw_map.insert(~"else", KW_ELSE);
    kw_map.insert(~"end", KW_END);
    kw_map.insert(~"false", KW_FALSE);
    kw_map.insert(~"function", KW_FUNCTION);
    kw_map.insert(~"if", KW_IF);
    kw_map.insert(~"integer", KW_INTEGER);
    kw_map.insert(~"mod", KW_MOD);
    kw_map.insert(~"not", KW_NOT);
    kw_map.insert(~"or", KW_OR);
    kw_map.insert(~"procedure", KW_PROCEDURE);
    kw_map.insert(~"readln", KW_READLN);
    kw_map.insert(~"then", KW_THEN);
    kw_map.insert(~"true", KW_TRUE);
    kw_map.insert(~"var", KW_VAR);
    kw_map.insert(~"writeln", KW_WRITELN);
    kw_map
}
