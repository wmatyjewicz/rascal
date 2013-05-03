// Copyright (C) 2013 Wojciech Matyjewicz
//
// This file is distributed under the terms of the MIT License.
// See LICENSE file for details.

// Various utility functions

pub fn error(msg: &str) -> ! {
    io::println(msg);
    fail!();
}
