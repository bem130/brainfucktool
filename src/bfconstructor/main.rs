use std::collections::HashMap;

// Replicates a string `s` for `n` times.
fn replicate(n: usize, s: &str) -> String {
    s.repeat(n)
}

// Encodes a string into a vector of Unicode code points.
fn text_encoder(s: &str) -> Vec<u32> {
    s.chars().map(|c| c as u32).collect()
}

// Returns a string consisting of `x` plus signs.
fn number(x: usize) -> String {
    replicate(x, "+")
}

// Intercalates encoded numbers with ".>" and appends a final ".>".
fn encode_string(s: &str) -> String {
    let encoded: Vec<String> = text_encoder(s)
        .iter()
        .map(|&n| number(n as usize))
        .collect();
    format!("{}{}", encoded.join(".>"), ".>")
}

// Generates a Brainfuck snippet to move a value `n` cells to the left.
fn move_left(n: usize) -> String {
    format!(
        "{}[-]{}[{}+{}-]",
        replicate(n, "<"),
        replicate(n, ">"),
        replicate(n, "<"),
        replicate(n, ">")
    )
}

// Generates a Brainfuck snippet to move a value `n` cells from the left.
fn move_right(n: usize) -> String {
    format!(
        "[-]{}[{}+{}-]{}",
        replicate(n, "<"),
        replicate(n, ">"),
        replicate(n, "<"),
        replicate(n, ">")
    )
}

// Generates a Brainfuck snippet to copy a value `n` cells to the right.
fn copy_right(n: usize) -> String {
    format!(
        "[-]{}[{}+>+<{}-]{}[{}+{}-]<",
        replicate(n, "<"),
        replicate(n, ">"),
        replicate(n, "<"),
        replicate(n + 1, ">"),
        replicate(n + 1, "<"),
        replicate(n + 1, ">")
    ) + "<"
}

// Creates a string with indent*indentsize spaces.
fn make_indent(indent: usize, indentsize: usize) -> String {
    " ".repeat(indent * indentsize)
}

// Structure representing the compiler state.
#[derive(Debug, Clone)]
struct CompilerState {
    env: HashMap<String, usize>,
    next_cell: usize,
    code: String,
}

impl Default for CompilerState {
    fn default() -> Self {
        CompilerState {
            env: HashMap::new(),
            next_cell: 0,
            code: String::new(),
        }
    }
}

// Calculates the relative address of a variable.
fn adr_local(state: &CompilerState, var: &str) -> usize {
    if let Some(&val) = state.env.get(var) {
        state.next_cell - val
    } else {
        panic!("Error: Undefined Variable Name");
    }
}

const COMMENT_WIDTH: usize = 2;

// Updated append_code function with indentation.
fn append_code(
    mut state: CompilerState,
    cmd: &str,
    s: &str,
    delta: isize,
    indent: usize,
    indentsize: usize,
) -> CompilerState {
    let new_next = state.next_cell as isize + delta;
    if new_next < 0 {
        panic!("Error");
    }
    let indent_str = make_indent(indent, indentsize);
    let tail = make_indent(COMMENT_WIDTH-indent, indentsize);
    let code = format!("/* {}{: <12}{} */ {} #{}\n", indent_str, cmd,tail,s,&new_next.to_string());
    state.code.push_str(&code);
    state.next_cell = new_next as usize;
    state
}

// Enum representing various Brainfuck commands.
#[derive(Debug, Clone)]
enum Cmd {
    Clear,
    Copy,
    Get(String),
    Set(String),
    Read,
    Write,
    Push(usize),
    Inc,
    Dec,
    Add,
    Sub,
    Mul,
    Addc(usize),
    Subc(usize),
    Bool,
    Stat(Vec<Cmd>), // A block that guarantees the stack remains unchanged.
    IfThen { cond: Vec<Cmd>, then_block: Vec<Cmd> }, // if [condition] [block]
}

// Processes a list of commands, updating the compiler state with indentation.
fn process_cmd_list(
    mut state: CompilerState,
    cmds: &[Cmd],
    indent: usize,
    indentsize: usize,
) -> CompilerState {
    for cmd in cmds {
        state = process_cmd(state, cmd, indent, indentsize);
    }
    state
}

// Processes a single command and updates the compiler state accordingly with indentation.
fn process_cmd(
    state: CompilerState,
    cmd: &Cmd,
    indent: usize,
    indentsize: usize,
) -> CompilerState {
    match cmd {
        Cmd::Clear => append_code(state, "clear", "[-]", 0, indent, indentsize),
        Cmd::Copy => append_code(state, "copy", "[>+>+<<-]>>[<<+>>-]<", 1, indent, indentsize),
        Cmd::Get(var) => {
            let adr = adr_local(&state, var);
            let code_str = format!(">{}", copy_right(1 + adr));
            append_code(state, &format!("get {}", var), &code_str, 1, indent, indentsize)
        }
        Cmd::Set(var) => {
            let adr = adr_local(&state, var);
            let code_str = format!("{}<", move_left(adr));
            append_code(state, &format!("set {}", var), &code_str, -1, indent, indentsize)
        }
        Cmd::Read => append_code(state, "read", ">,", 1, indent, indentsize),
        Cmd::Write => append_code(state, "write", ".[-]<", -1, indent, indentsize),
        Cmd::Push(n) => {
            let code_str = format!(">{}", number(*n));
            append_code(state, &format!("push {}", n), &code_str, 1, indent, indentsize)
        }
        Cmd::Inc => append_code(state, "inc", "+", 0, indent, indentsize),
        Cmd::Dec => append_code(state, "dec", "-", 0, indent, indentsize),
        Cmd::Add => append_code(state, "add", "[<+>-]<", -1, indent, indentsize),
        Cmd::Sub => append_code(state, "sub", "[<->-]<", -1, indent, indentsize),
        Cmd::Mul => append_code(state, "mul", "<[>>+<<-]>[>[<<+>>>+<-]>[<+>-]<<-]>[-]<<", -1, indent, indentsize),
        Cmd::Addc(n) => {
            let code_str = replicate(*n, "+");
            append_code(state, &format!("addc {}", n), &code_str, 0, indent, indentsize)
        }
        Cmd::Subc(n) => {
            let code_str = replicate(*n, "-");
            append_code(state, &format!("subc {}", n), &code_str, 0, indent, indentsize)
        }
        Cmd::Bool => append_code(state, "bool", "[[-]>+<]>[<+>-]<", 0, indent, indentsize),
        Cmd::Stat(cmds_inner) => {
            // Create a temporary state with the same next_cell and env, but empty code.
            let temp_state = CompilerState {
                env: state.env.clone(),
                next_cell: state.next_cell,
                code: String::new(),
            };
            // Increase indent for inner stat block.
            let inner_state = process_cmd_list(temp_state, cmds_inner, indent + 1, indentsize);
            if inner_state.next_cell != state.next_cell {
                panic!("Error: Stack pointer changed in stat block");
            }
            let inner_indent = make_indent(indent, indentsize);
            let code_str = format!(
                "\n{}{}",
                inner_state.code,
                format!("/* {}{: <12}{} */",
                    make_indent(indent, indentsize),
                    "end stat",
                    make_indent(COMMENT_WIDTH-indent, indentsize),
                ),
            );
            append_code(state, "stat", &code_str, 0, indent, indentsize)
        }
        Cmd::IfThen { cond, then_block } => {
            // Process condition block in a temporary state with increased indent.
            let temp_state = CompilerState {
                env: state.env.clone(),
                next_cell: state.next_cell,
                code: String::new(),
            };
            let cond_state = process_cmd_list(temp_state, cond, indent + 1, indentsize);
            if cond_state.next_cell != state.next_cell + 1 {
                panic!("Error: Condition block must increase stack pointer by 1");
            }
            // Process then block with increased indent.
            let then_state = process_cmd_list(
                CompilerState {
                    env: cond_state.env.clone(),
                    next_cell: cond_state.next_cell,
                    code: String::new(),
                },
                then_block,
                indent + 1,
                indentsize,
            );
            if then_state.next_cell != cond_state.next_cell {
                panic!("Error: Then block must not change stack pointer");
            }
            let indent_str = make_indent(indent, indentsize);
            let code_str = format!(
                "\n{}{}{} [\n{}{}{}   [-]]<",
                cond_state.code,
                format!("/* {}{: <12}{} */",
                    make_indent(indent, indentsize),
                    "then",
                    make_indent(COMMENT_WIDTH-indent, indentsize),
                ),
                indent_str,
                then_state.code,
                format!("/* {}{: <12}{} */",
                    make_indent(indent, indentsize),
                    "end if",
                    make_indent(COMMENT_WIDTH-indent, indentsize),
                ),
                indent_str
            );
            append_code(state, "if", &code_str, 0, indent, indentsize)
        }
    }
}

// Establishes a scope by setting up let variables and processing commands.
fn scope(letvars: &[&str], cmds: &[Cmd], indent: usize, indentsize: usize) -> CompilerState {
    let mut state = CompilerState::default();
    for var in letvars {
        let idx = state.next_cell;
        state.env.insert(var.to_string(), idx);
        let code_str = format!(">");
        state = append_code(state, &format!("let {}", var), &code_str, 1, indent, indentsize);
    }
    state.code += "\n";
    process_cmd_list(state, cmds, indent, indentsize)
}

// Example program that uses the defined commands.
fn example_program() -> String {
    scope(
        &["a", "b"],
        &[
            Cmd::IfThen {
                cond: vec![Cmd::Push(1)],
                then_block: vec![Cmd::Stat(vec![
                    Cmd::Push(5),
                    Cmd::Set("a".to_string()),
                ])],
            },
            Cmd::IfThen {
                cond: vec![Cmd::Push(0)],
                then_block: vec![Cmd::Stat(vec![
                    Cmd::Push(4),
                    Cmd::Set("b".to_string()),
                ])],
            },
            Cmd::Stat(vec![
                Cmd::Push(5),
                Cmd::Push(2),
                Cmd::Add,
                Cmd::Push(3),
                Cmd::Sub,
                Cmd::Push(10),
                Cmd::Mul,
                Cmd::Write,
            ]),
        ],
        0,
        4,
    ).code
}

fn main() {
    // Testing utility functions.
    println!("text_encoder(\"ABCDE\") -> {:?}", text_encoder("ABCDE"));
    println!("text_encoder(\"abcde\") -> {:?}", text_encoder("abcde"));
    println!("move_right(1) -> {}", move_right(1));

    // Print the generated Brainfuck code from the example program.
    println!("```bf");
    println!("{}", example_program());
    println!("```");
}
