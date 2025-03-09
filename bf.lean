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
  "]"

inductive Cmd where
  | get     : String → Cmd
  | set     : String → Cmd
  | pushNum : Nat → Cmd
  | pushStr : String → Cmd
  | add     : Cmd
  | sub     : Cmd
deriving Repr

/-
  Define a compiler state structure that holds:
    - env: mapping from variable names to cell indices (the "scope")
    - nextCell: next available cell index
    - code: accumulated Brainfuck code
  変数名からセル番号へのマッピング（スコープ）、次に利用可能なセル番号、生成された Brainfuck コードを保持するコンパイラ状態を定義します。
-/
structure CompilerState where
  env : Std.HashMap String Nat
  nextCell : Nat
  code : String
deriving Repr

-- 変数がある相対アドレスを計算する
def adrLocal (var: String) (st: CompilerState): Nat :=
  match st.env.get? var with
  | some val => st.nextCell - val
  | none => panic! "Error: Undefined Variable Name"

-- Initial state with an empty environment, starting cell index 0, and no generated code.
def initState : CompilerState :=
  { env := Std.HashMap.empty, nextCell := 0, code := "" }

-- Helper function to append generated code.
def appendCode (s: String) (st: CompilerState): CompilerState :=
  { st with code := st.code ++ s ++ "\n" }

def bfSet (n: Nat) : String :=
  "<" ++ moveLeft n

def bfGet (n: Nat) : String :=
  moveRight n

def bfPushNum (n: Nat): String :=
  number n ++ ">"

def bfPushStr (s: String): String :=
  string s ++ ">"

def bfAdd : String :=
  "/* add top two stack values */"

def bfSub : String :=
  "/* subtract top two stack values */"

/-
  Process a single command, updating the compiler state.
  各コマンドを処理し、コンパイラ状態を更新
-/
def processCmd (st: CompilerState): Cmd → CompilerState
  | Cmd.get var =>
      appendCode (bfGet (adrLocal var st)) st
  | Cmd.set var =>
      appendCode (bfSet (adrLocal var st)) st
  | Cmd.pushNum n =>
      appendCode (bfPushNum n) st
  | Cmd.pushStr s =>
      appendCode (bfPushStr s) st
  | Cmd.add =>
      appendCode bfAdd st
  | Cmd.sub =>
      appendCode bfSub st


def scope (letvars: List String) (cmds: List Cmd): CompilerState :=
  let State := letvars.foldl (fun s var =>
        let idx := s.nextCell
        let newEnv := s.env.insert var idx
        let s' := appendCode (">") s
        { s' with env := newEnv, nextCell := s'.nextCell + 1 }
      ) initState
  cmds.foldl processCmd State


def exampleProgram: CompilerState :=
  scope ["a","b"] [
    Cmd.pushNum 1,
    Cmd.set "b",
    Cmd.pushNum 2,
    Cmd.set "a",
    Cmd.pushNum 3,
    Cmd.get "b",
  ]

-- #eval exampleProgram
#eval IO.println exampleProgram.code
