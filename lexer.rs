// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// Lexer
// Treats input as ASCII text.

use util;
use token;
use token::Token;

pub struct Position {
    line: uint,
    col: uint
}

pub struct Lexer {
    src_path: @Path,
    priv src: ~str,
    priv offset: uint,
    priv pos: Position,
    priv token_offset: uint,
    token_pos: Position,
    token: Token,
    priv kw_map: token::KeywordMap
}

pub fn from_string(src: ~str) -> Lexer {
    new_lexer(@Path("<string>"), src)
}

pub fn from_file(src_path: @Path) -> Lexer {
    let src = match io::read_whole_file_str(src_path) {
        Ok(s) => s,
        Err(msg) => util::error(~"I/O error while reading source file: " + msg)
    };
    new_lexer(src_path, src)
}

fn new_lexer(src_path: @Path, src: ~str) -> Lexer {
    assert!(src.is_ascii());

    let mut lexer = Lexer {
        src_path: src_path,
        src: src,
        offset: 0u,
        pos: Position { line: 1u, col: 0u },
        token_offset: 0u,
        token_pos: Position { line: 1u, col: 0u },
        token: token::EOF,
        kw_map: token::get_keyword_map()
    };
    lexer.consume(); // Initialize lexer.
    lexer
}

pub impl Lexer {
    fn consume(&mut self) {
        self.skip_whitespace_and_comments();
        self.token_offset = self.offset;
        self.token_pos = self.pos;
        if (self.is_end()) {
            self.token = token::EOF;
        } else {
            self.token = self.scan_token();
        }
    }
}

impl Lexer {
    fn skip_whitespace_and_comments(&mut self) {
        while !self.is_end() {
            let c = self.peek_char();
            if (is_whitespace(c)) {
                self.consume_char();
            } else if (c == '{') {
                self.consume_char();
                while (self.guarded_peek_char() != '}') {
                    self.consume_char();
                }
                self.consume_char();
            } else {
                break;
            }
        }
    }

    fn scan_token(&mut self) -> Token {
        match self.scan_ident_or_keyword() {
            Some(tok) => return tok,
            _ => {}
        }
        match self.scan_int_literal() {
            Some(tok) => return tok,
            _ => {}
        }
        match self.scan_two_char_token() {
            Some(tok) => return tok,
            _ => {}
        }
        match self.scan_one_char_token() {
            Some(tok) => return tok,
            _ => {}
        }
        self.error_at_peek(~"Unexpected character: " + str::from_char(self.peek_char()));
    }

    fn scan_ident_or_keyword(&mut self) -> Option<Token> {
        let c = self.peek_char();
        if is_ident_start(c) {
            self.consume_char();
            while !self.is_end() && is_ident_cont(self.peek_char()) {
                self.consume_char();
            }
            let ident = self.src.slice(self.token_offset, self.offset)
                .to_ascii().to_lower().to_str_ascii();
            Some(self.token_from_ident_str(ident))
        }
        else {
            None
        }
    }

    fn scan_int_literal(&mut self) -> Option<Token> {
        let c = self.peek_char();
        if is_digit(c) {
            self.consume_char();
            while !self.is_end() && is_digit(self.peek_char()) {
                self.consume_char();
            }
            // FIXME: Why is str::to_owned necessary to make borrowck quiet?
            let neg = ~"-" + str::to_owned(self.src.slice(self.token_offset, self.offset));
            let num = match i32::from_str(neg) {
                Some(i) => -i,
                None => self.error_at_peek("Integer literal out of bounds")
            };
            Some(token::INT_LITERAL(num))
        } else {
            None
        }
    }

    fn scan_two_char_token(&mut self) -> Option<Token> {
        match self.peek_char() {
            '<' => {
                self.consume_char();
                if self.is_end() {
                    return Some(token::LT);
                }
                match self.peek_char() {
                    '=' => {
                        self.consume_char();
                        Some(token::LE)
                    },
                    '>' => {
                        self.consume_char();
                        Some(token::NE)
                    },
                    _ => Some(token::LT)
                }
            },
            '>' => {
                self.consume_char();
                if self.is_end() || (self.peek_char() != '=') {
                    Some(token::GT)
                } else {
                    self.consume_char();
                    Some(token::GE)
                }
            },
            ':' => {
                self.consume_char();
                if self.is_end() || (self.peek_char() != '=') {
                    Some(token::COLON)
                } else {
                    self.consume_char();
                    Some(token::ASSIGN)
                }
            },
            '.' => {
                self.consume_char();
                if self.is_end() || (self.peek_char() != '.') {
                    Some(token::DOT)
                } else {
                    self.consume_char();
                    Some(token::TO)
                }
            }
            _ => None
        }
    }

    fn scan_one_char_token(&mut self) -> Option<Token> {
        let tok = match self.peek_char() {
            // Arithmetic operators
            '+' => token::PLUS,
            '-' => token::MINUS,
            '*' => token::TIMES,
            // The only comparison operator not handled by scan_two_char_token
            '=' => token::EQ,
            // Delimiters etc.
            '(' => token::LPAREN,
            ')' => token::RPAREN,
            '[' => token::LBRACKET,
            ']' => token::RBRACKET,
            '.' => token::DOT,
            ',' => token::COMMA,
            ':' => token::COLON,
            ';' => token::SEMICOLON,            
            _ => return None
        };
        self.consume_char();
        Some(tok)
    }
    
    fn token_from_ident_str(&self, s: ~str) -> Token {
        match self.kw_map.find(&s) {
            Some(tok) => copy *tok,
            None => token::IDENT(s.to_managed())
        }
    }

    fn consume_char(&mut self) {
        if self.peek_char() == '\n' {
            self.pos.line += 1;
            self.pos.col = 0;
        } else {
            self.pos.col += 1
        }
        self.offset += 1;
    }

    fn guarded_peek_char(&self) -> char {
        if self.is_end() {
            util::error("Unexpected end of file");
        }
        self.peek_char()
    }
        
    #[inline(always)]
    fn peek_char(&self) -> char {
        self.src[self.offset] as char
    }

    #[inline(always)]
    fn is_end(&self) -> bool {
        self.offset == self.src.len()
    }

    fn error_at_peek(&self, msg: &str) -> ! {
        util::error(fmt!("%s:%u:%u: %s", self.src_path.to_str(), self.token_pos.line,
                         self.token_pos.col, msg));
    }
}
    
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r' || c == '\n'
}

fn is_ident_start(c: char) -> bool {
    is_letter(c) || c == '_'
}

fn is_ident_cont(c: char) -> bool {
    is_ident_start(c) || is_digit(c)
}

#[inline(always)]
fn is_letter(c: char) -> bool {
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

#[inline(always)]
fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

#[cfg(test)]
mod test {
    use super::*;
    use token;

    #[test]
    fn whitespace_and_comments_consumption() {
        let lexer = from_string(~"\n\r\t {\n\r\t }\n\r\t {\n\r\t }begin");
        assert_eq!(lexer.token, token::KW_BEGIN);
        assert_eq!(lexer.token_pos.line, 5);
        assert_eq!(lexer.token_pos.col, 4);
    }

    #[test]
    fn ident() {
        let lexer = from_string(~"_aB1");
        assert_eq!(lexer.token, token::IDENT(@"_ab1"));
        assert_eq!(lexer.token_pos.line, 1);
        assert_eq!(lexer.token_pos.col, 0);
    }

    #[test]
    fn keywords() {
        let mut lexer = from_string(~"begin end");
        assert_eq!(lexer.token, token::KW_BEGIN);
        assert_eq!(lexer.token_pos.line, 1);
        assert_eq!(lexer.token_pos.col, 0);
        lexer.consume();
        assert_eq!(lexer.token, token::KW_END);
        assert_eq!(lexer.token_pos.line, 1);
        assert_eq!(lexer.token_pos.col, 6);
    }

    #[test]
    fn int_literal() {
        let lexer = from_string(~"123");
        assert_eq!(lexer.token, token::INT_LITERAL(123));
    }
}
