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

// Appends generated code to the compiler state and updates the stack pointer.
fn append_code(mut state: CompilerState, s: &str, delta: isize) -> CompilerState {
    let new_next = state.next_cell as isize + delta;
    if new_next < 0 {
        panic!("Error");
    }
    state.code.push_str(s);
    state.code.push_str(" #");
    state.code.push_str(&new_next.to_string());
    state.code.push('\n');
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

// Processes a list of commands, updating the compiler state.
fn process_cmd_list(mut state: CompilerState, cmds: &[Cmd]) -> CompilerState {
    for cmd in cmds {
        state = process_cmd(state, cmd);
    }
    state
}

// Processes a single command and updates the compiler state accordingly.
fn process_cmd(state: CompilerState, cmd: &Cmd) -> CompilerState {
    match cmd {
        Cmd::Clear => append_code(state, "/* clear */   [-]", 0),
        Cmd::Copy => append_code(state, "/* copy */   [>+>+<<-]>>[<<+>>-]<", 1),
        Cmd::Get(var) => {
            let adr = adr_local(&state, var);
            let code_str = format!("/* get {} */   >{}", var, copy_right(1 + adr));
            append_code(state, &code_str, 1)
        }
        Cmd::Set(var) => {
            let adr = adr_local(&state, var);
            let code_str = format!("/* set {} */   {}<", var, move_left(adr));
            append_code(state, &code_str, -1)
        }
        Cmd::Read => append_code(state, "/* read */   >,", 1),
        Cmd::Write => append_code(state, "/* write */   .[-]<", -1),
        Cmd::Push(n) => {
            let code_str = format!("/* push {} */   >{}", n, number(*n));
            append_code(state, &code_str, 1)
        }
        Cmd::Inc => append_code(state, "/* inc */   +", 0),
        Cmd::Dec => append_code(state, "/* dec */   -", 0),
        Cmd::Add => append_code(state, "/* add */   [<+>-]<", -1),
        Cmd::Sub => append_code(state, "/* sub */   [<->-]<", -1),
        Cmd::Mul => append_code(
            state,
            "/* mul */   <[>>+<<-]>[>[<<+>>>+<-]>[<+>-]<<-]>[-]<<",
            -1,
        ),
        Cmd::Addc(n) => {
            let code_str = format!("/* addc {} */   {}", n, replicate(*n, "+"));
            append_code(state, &code_str, 0)
        }
        Cmd::Subc(n) => {
            let code_str = format!("/* subc {} */   {}", n, replicate(*n, "-"));
            append_code(state, &code_str, 0)
        }
        Cmd::Bool => append_code(state, "/* bool */   [[-]>+<]>[<+>-]<", 0),
        Cmd::Stat(cmds_inner) => {
            // Create a temporary state with the same next_cell and env, but empty code.
            let temp_state = CompilerState {
                env: state.env.clone(),
                next_cell: state.next_cell,
                code: String::new(),
            };
            let inner_state = process_cmd_list(temp_state, cmds_inner);
            if inner_state.next_cell != state.next_cell {
                panic!("Error: Stack pointer changed in stat block");
            }
            let code_str = format!("/* stat */\n{}/* end stat */", inner_state.code);
            append_code(state, &code_str, 0)
        }
        Cmd::IfThen { cond, then_block } => {
            // Process condition block in a temporary state.
            let temp_state = CompilerState {
                env: state.env.clone(),
                next_cell: state.next_cell,
                code: String::new(),
            };
            let cond_state = process_cmd_list(temp_state, cond);
            if cond_state.next_cell != state.next_cell + 1 {
                panic!("Error: Condition block must increase stack pointer by 1");
            }
            let then_state = process_cmd_list(
                CompilerState {
                    env: cond_state.env.clone(),
                    next_cell: cond_state.next_cell,
                    code: String::new(),
                },
                then_block,
            );
            if then_state.next_cell != cond_state.next_cell {
                panic!("Error: Then block must not change stack pointer");
            }
            let code_str = format!(
                "/* if */\n{}/* then */   [\n{}/* end if */   [-]]<",
                cond_state.code, then_state.code
            );
            append_code(state, &code_str, 0)
        }
    }
}

// Establishes a scope by setting up let variables and processing commands.
fn scope(letvars: &[&str], cmds: &[Cmd]) -> CompilerState {
    let mut state = CompilerState::default();
    for var in letvars {
        let idx = state.next_cell;
        state.env.insert(var.to_string(), idx);
        let code_str = format!("/* let {} */   >", var);
        state = append_code(state, &code_str, 1);
    }
    process_cmd_list(state, cmds)
}

// Example program that uses the defined commands.
fn example_program() -> String {
    let state = scope(
        &["a", "b"],
        &[
            Cmd::IfThen {
                cond: vec![Cmd::Push(1)], // condition block: e.g., push true
                then_block: vec![Cmd::Stat(vec![
                    Cmd::Push(5),
                    Cmd::Set("a".to_string()),
                ])],
            },
            Cmd::IfThen {
                cond: vec![Cmd::Push(0)], // condition block: e.g., push false
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
    );
    state.code
}

fn main() {
    // Testing utility functions
    println!("text_encoder(\"ABCDE\") -> {:?}", text_encoder("ABCDE"));
    println!("text_encoder(\"abcde\") -> {:?}", text_encoder("abcde"));
    println!("move_right(1) -> {}", move_right(1));

    // Print the generated Brainfuck code from the example program.
    println!("```bf");
    println!("{}", example_program());
    println!("```");
}
