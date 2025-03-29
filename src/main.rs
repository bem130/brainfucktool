/*
    Brainfuck Interpreter in Rust
        Ported to Rust by Bem130 (C to Rust), 2025

    This is a Rust port of an existing C-based interpreter.

    The original source code was obtained from:
    https://launchpad.net/ubuntu/+source/bf/20041219ubuntu6

    Below is the text from the original source:

    bf.c
    ******************************************************************************

    Yet another Brainfuck interpreter

    Author: Stephan Beyer

    Copyright (C) GPL, 2003, 2004 Stephan Beyer - s-beyer@gmx.net

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
    THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
    IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    ******************************************************************************
*/


use clap::Parser;
use std::fs;
use std::io::{self, Read, Write};

/// Brainfuck Interpreter in Rust
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Opt {
    /// Specify number of cells (default 30000)
    #[arg(short = 'c', default_value = "30000")]
    cells: usize,

    /// Show used code input (prints to stderr)
    #[arg(short = 'i', action)]
    showinput: bool,

    /// Translate newline (ASCII 10) to null (0)
    #[arg(short = 'n', action)]
    null: bool,

    /// Disallow wraparound (error on decrementing 0 or incrementing 255)
    #[arg(short = 'w', action)]
    nowrap: bool,

    /// Set input mode (0-4); only mode 0 is implemented in this version
    #[arg(short = ',', default_value = "0")]
    inputmode: u8,

    /// Input file containing Brainfuck source code
    filename: String,
}

/// Structure representing a single aggregated Brainfuck command.
#[derive(Debug)]
struct Progr {
    // For commands that are not aggregated (like [ ] , .), op holds the character.
    op: Option<char>,
    // Aggregated count for '+' or '-' commands.
    plus: i32,
    // Aggregated count for '>' or '<' commands.
    step: i32,
    // The index of the matching bracket for loops.
    matching: Option<usize>,
}

impl Progr {
    fn new() -> Self {
        Progr {
            op: None,
            plus: 0,
            step: 0,
            matching: None,
        }
    }
}

/// Reads the Brainfuck program from a string and aggregates consecutive commands.
fn read_program(contents: &str) -> Vec<Progr> {
    // Explicitly annotate the type for the vector.
    let mut program: Vec<Progr> = Vec::new();
    let valid_chars = "+-<>.,[]";
    let mut last_char: Option<char> = None;

    // Iterate over each character from the source file.
    for c in contents.chars() {
        if valid_chars.contains(c) {
            // Determine whether to aggregate with the previous command.
            let mut new_cmd = false;
            if let Some(last) = last_char {
                if (last == '+' || last == '-') && (c == '+' || c == '-') {
                    if let Some(last_cmd) = program.last_mut() {
                        if last_cmd.op.is_none() {
                            last_cmd.plus += if c == '+' { 1 } else { -1 };
                            last_char = Some(c);
                            continue;
                        }
                    }
                } else if (last == '>' || last == '<') && (c == '>' || c == '<') {
                    if let Some(last_cmd) = program.last_mut() {
                        if last_cmd.op.is_none() {
                            last_cmd.step += if c == '>' { 1 } else { -1 };
                            last_char = Some(c);
                            continue;
                        }
                    }
                } else {
                    new_cmd = true;
                }
            } else {
                new_cmd = true;
            }

            if new_cmd || program.is_empty() {
                let mut cmd = Progr::new();
                match c {
                    '+' => cmd.plus = 1,
                    '-' => cmd.plus = -1,
                    '>' => cmd.step = 1,
                    '<' => cmd.step = -1,
                    _ => cmd.op = Some(c),
                }
                program.push(cmd);
            }
            last_char = Some(c);
        }
    }
    program
}

/// Finds matching brackets for loops using index-based iteration.
fn find_matching_brackets(program: &mut Vec<Progr>) -> Result<(), String> {
    let mut stack: Vec<usize> = Vec::new();
    for i in 0..program.len() {
        if let Some(op) = program[i].op {
            if op == '[' {
                stack.push(i);
            } else if op == ']' {
                if let Some(j) = stack.pop() {
                    program[j].matching = Some(i);
                    program[i].matching = Some(j);
                } else {
                    return Err("Unbalanced brackets: extra ']' found".to_string());
                }
            }
        }
    }
    if !stack.is_empty() {
        return Err("Unbalanced brackets: missing ']'".to_string());
    }
    Ok(())
}

/// Reads a single byte of input. For simplicity, only input mode 0 is implemented.
fn get_input(opt: &Opt) -> io::Result<u8> {
    let mut buffer = [0; 1];
    io::stdin().read_exact(&mut buffer)?;
    let mut byte = buffer[0];
    // If the -n option is enabled, translate newline to null.
    if opt.null && byte == b'\n' {
        byte = 0;
    }
    Ok(byte)
}

/// Interprets the Brainfuck program.
fn interprete(program: &Vec<Progr>, opt: &Opt) -> Result<(), String> {
    // Create the Brainfuck tape with the specified number of cells.
    let mut tape = vec![0u8; opt.cells];
    let mut ptr: usize = 0;
    let mut i = 0;
    while i < program.len() {
        let cmd = &program[i];
        // Optionally show the command if showinput is enabled.
        if opt.showinput {
            if let Some(ch) = cmd.op {
                eprint!("{}", ch);
            } else if cmd.plus != 0 {
                eprint!("{}", if cmd.plus > 0 { '+' } else { '-' });
            } else if cmd.step != 0 {
                eprint!("{}", if cmd.step > 0 { '>' } else { '<' });
            }
        }
        if let Some(op) = cmd.op {
            match op {
                '[' => {
                    // If current cell is zero, jump to the matching ']' command.
                    if tape[ptr] == 0 {
                        if let Some(m) = cmd.matching {
                            i = m;
                        } else {
                            return Err("No matching bracket for '['".to_string());
                        }
                    }
                }
                ']' => {
                    // If current cell is non-zero, jump back to the matching '[' command.
                    if tape[ptr] != 0 {
                        if let Some(m) = cmd.matching {
                            i = m;
                        } else {
                            return Err("No matching bracket for ']'".to_string());
                        }
                    }
                }
                '.' => {
                    // Output the character corresponding to the current cell's value.
                    print!("{}", tape[ptr] as char);
                    io::stdout().flush().unwrap();
                }
                ',' => {
                    // Read a single byte of input.
                    match get_input(opt) {
                        Ok(val) => tape[ptr] = val,
                        Err(e) => return Err(e.to_string()),
                    }
                }
                _ => {}
            }
        }
        // Perform the aggregated '+' or '-' operation.
        if cmd.plus != 0 {
            if opt.nowrap {
                // Check for overflow/underflow if wraparound is disallowed.
                let new_val = tape[ptr] as i32 + cmd.plus;
                if new_val > 255 {
                    return Err("Out of range! Incrementing 0xFF byte is disallowed (-w option).".to_string());
                } else if new_val < 0 {
                    return Err("Out of range! Decrementing 0x00 byte is disallowed (-w option).".to_string());
                }
                tape[ptr] = new_val as u8;
            } else {
                // Wrapping arithmetic modulo 256 is the default behavior.
                tape[ptr] = tape[ptr].wrapping_add(cmd.plus as u8);
            }
        }
        // Perform the aggregated '>' or '<' operation.
        if cmd.step != 0 {
            let new_ptr = ptr as isize + cmd.step as isize;
            if new_ptr < 0 || (new_ptr as usize) >= opt.cells {
                return Err("Pointer out of range! Check the number of cells (-c option).".to_string());
            }
            ptr = new_ptr as usize;
        }
        i += 1;
    }
    Ok(())
}

fn main() {
    // Parse command-line arguments.
    let opt = Opt::parse();

    // Read the Brainfuck source file.
    let content = fs::read_to_string(&opt.filename).unwrap_or_else(|e| {
        eprintln!("Error reading file: {}", e);
        std::process::exit(1);
    });

    // Parse and aggregate the program commands.
    let mut program = read_program(&content);

    // Find matching brackets for loop constructs.
    if let Err(e) = find_matching_brackets(&mut program) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }

    // Interpret (execute) the Brainfuck program.
    if let Err(e) = interprete(&program, &opt) {
        eprintln!("Error during interpretation: {}", e);
        std::process::exit(1);
    }
}
