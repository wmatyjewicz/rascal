// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

use lexer;
use parser;
use checker;
use emitter;

fn compile(src_path: @Path, bc_path: &Path) {
    let mut lexer = lexer::from_file(src_path);
    let program = parser::parse_program(&mut lexer);
    checker::check_program(&program);
    emitter::emit_program(bc_path, &program);
}

#[main]
fn main() {
    let args = os::args();
    if args.len() != 2 {
        fail!(~"Source file not specified.");
    }
    let src_path = @Path(args[1]);
    let bc_path = src_path.with_filetype("bc");

    compile(src_path, &bc_path);
}
