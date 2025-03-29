import Std.Data.HashMap

-- sをn個に複製する
def replicate (n: Nat) (s: String): String :=
  match n with
  | 0     => ""
  | n + 1 => s ++ replicate n s

def textEncoder (s: String): List Nat :=
  s.toList.map Char.toNat

def number (x: Nat): String :=
  "" ++ replicate x "+"

def string (s: String): String :=
  String.intercalate ".>" ((textEncoder s).map number) ++ ".>"

-- 値をn個手前のセルに移動
def moveLeft (n: Nat): String :=
    replicate n "<" ++
  "[-]" ++
    replicate n ">" ++
  "[" ++
    replicate n "<" ++
    "+" ++
    replicate n ">" ++
    "-" ++
  "]"

-- 値をn個手前のセルから移動
def moveRight (n: Nat): String :=
  "[-]" ++
  replicate n "<" ++
  "[" ++
    replicate n ">" ++
    "+" ++
    replicate n "<" ++
    "-" ++
  "]" ++
  replicate n ">"

-- 値をn個手前のセルに複製
def copyRight (n: Nat): String :=
    "[-]"
        ++ replicate n "<"
        ++ "["
        ++ replicate n ">"
        ++ "+>+<"
        ++ replicate n "<"
        ++ "-"
        ++ "]"
        ++ replicate (n + 1) ">"
        ++ "["
        ++ replicate (n + 1) "<"
        ++ "+"
        ++ replicate (n + 1) ">"
        ++ "-"
        ++ "]"
        ++ "<"

/-
  CompilerState:
    - env: mapping from variable names to cell indices (the "scope")
    - nextCell: next available cell index
    - code: accumulated Brainfuck code

  コンパイラ状態は、変数名とセル番号のマッピング、次に使えるセル番号、生成された Brainfuck コードを保持します。
-/
structure CompilerState where
  env : Std.HashMap String Nat
  nextCell : Nat
  code : String
deriving Repr

-- Initial state
def initState : CompilerState :=
  { env := Std.HashMap.empty, nextCell := 0, code := "" }

instance : Inhabited CompilerState :=
  { default := initState }

-- 変数の相対アドレスを計算する
def adrLocal (var: String) (st: CompilerState): Nat :=
  match st.env.get? var with
  | some val => st.nextCell - val
  | none => panic! "Error: Undefined Variable Name"

-- Append generated code and update stack pointer
def appendCode (s: String) (n: Int) (st: CompilerState): CompilerState :=
  let nc := st.nextCell + n
  { st with
    code := st.code ++ s ++ " #" ++ (toString nc) ++ "\n",
    nextCell := if nc >= 0 then Int.toNat nc else panic! "Error" }

inductive Cmd where
  | clear   : Cmd
  | copy    : Cmd
  | get     : String → Cmd
  | set     : String → Cmd
  | read    : Cmd
  | write   : Cmd
  | push    : Nat → Cmd
  | inc     : Cmd
  | dec     : Cmd
  | add     : Cmd
  | sub     : Cmd
  | mul     : Cmd
  | addc    : Nat → Cmd
  | subc    : Nat → Cmd
  | bool    : Cmd
  | stat    : List Cmd → Cmd   -- スタックが変わらないことを保証するブロック
  | ifThen     : List Cmd → List Cmd → Cmd  -- 新しいif: if [cond] [block]
deriving Repr

-- 相互再帰定義: processCmdList と processCmd
mutual
  def processCmdList (st : CompilerState) (cmds : List Cmd) : CompilerState :=
    match cmds with
    | []      => st
    | c :: cs => processCmdList (processCmd st c) cs

  def processCmd (st: CompilerState): Cmd → CompilerState
    | Cmd.clear =>
        appendCode ("/* clear */   " ++ "[-]") 0 st
    | Cmd.copy =>
        appendCode ("/* copy */   " ++ "[>+>+<<-]>>[<<+>>-]<") 1 st
    | Cmd.get var =>
        appendCode ("/* get " ++ var ++ " */   " ++ ">" ++ (copyRight (1 + (adrLocal var st)))) 1 st
    | Cmd.set var =>
        appendCode ("/* set " ++ var ++ " */   " ++ (moveLeft (adrLocal var st)) ++ "<") (-1) st
    | Cmd.read =>
        appendCode ("/* read */   " ++ ">,") 1 st
    | Cmd.write =>
        appendCode ("/* write */   " ++ ".[-]<") (-1) st
    | Cmd.push n =>
        appendCode ("/* push " ++ (toString n) ++ " */   " ++ (">" ++ number n)) 1 st
    | Cmd.inc =>
        appendCode ("/* inc */   " ++ "+") 0 st
    | Cmd.dec =>
        appendCode ("/* dec */   " ++ "-") 0 st
    | Cmd.add =>
        appendCode ("/* add */   " ++ "[<+>-]<") (-1) st
    | Cmd.sub =>
        appendCode ("/* sub */   " ++ "[<->-]<") (-1) st
    | Cmd.mul =>
        appendCode ("/* mul */   " ++ "<[>>+<<-]>[>[<<+>>>+<-]>[<+>-]<<-]>[-]<<") (-1) st
    | Cmd.addc n =>
        appendCode ("/* addc " ++ (toString n) ++ " */   " ++ (replicate n "+")) 0 st
    | Cmd.subc n =>
        appendCode ("/* subc " ++ (toString n) ++ " */   " ++ (replicate n "-")) 0 st
    | Cmd.bool =>
        appendCode ("/* bool */   " ++ "[[-]>+<]>[<+>-]<") 0 st
    | Cmd.stat cmds =>
        let innerState := processCmdList { st with code := "" } cmds;
        if innerState.nextCell ≠ st.nextCell then
          panic! "Error: Stack pointer changed in stat block"
        else
          appendCode ("/* stat */\n" ++ innerState.code ++ "/* end stat */") 0 st
    | Cmd.ifThen cond block =>
        let condState := processCmdList { st with code := "" } cond;
        if condState.nextCell ≠ st.nextCell + 1 then
          panic! "Error: Condition block must increase stack pointer by 1"
        else
          let thenState := processCmdList { condState with code := "" } block;
          if thenState.nextCell ≠ condState.nextCell then
            panic! "Error: Then block must not change stack pointer"
          else
            appendCode ("/* if */\n"
                        ++ condState.code
                        ++ "/* then */   [\n"
                        ++ thenState.code
                        ++ "/* end if */   [-]]<") 0 st
end

def scope (letvars: List String) (cmds: List Cmd): CompilerState :=
  let State := letvars.foldl (fun s var =>
        let idx := s.nextCell;
        let newEnv := s.env.insert var idx;
        let s' := appendCode ("/* let " ++ var ++ " */   " ++ ">") 1 s;
        { s' with env := newEnv }
      ) initState;
  cmds.foldl processCmd State


def exampleProgram: CompilerState :=
  scope ["a","b"] [
    Cmd.ifThen
      [
          -- Condition block: must increase stack pointer by 1.
          Cmd.push 1  -- for example, push true
      ]
      [
        -- Then block: must leave stack pointer unchanged.
        Cmd.stat [
              Cmd.push 5,
              Cmd.set "a"
        ],
      ],
    Cmd.ifThen
      [
          Cmd.push 0  -- for example, push false
      ]
      [
        Cmd.stat [
              Cmd.push 4,
              Cmd.set "b"
        ],
      ],
    Cmd.stat [
      Cmd.push 5,
      Cmd.push 2,
      Cmd.add,
      Cmd.push 3,
      Cmd.sub,
      Cmd.push 10,
      Cmd.mul,
      Cmd.write
    ]
  ]

-- Testing utility functions
#eval textEncoder "ABCDE"
#eval textEncoder "abcde"
#eval moveRight 1

-- Print the generated Brainfuck code
#eval IO.println ("```bf\n" ++ exampleProgram.code ++ "```")
