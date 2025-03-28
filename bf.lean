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
  { env := Std.HashMap.empty, nextCell := 0, code := "<" }

-- Helper function to append generated code.
def appendCode (s: String) (n: Int) (st: CompilerState): CompilerState :=
  let nc := st.nextCell+n
  { st with
    code := st.code ++ s ++ "\n",
    nextCell := if nc>0 then
                  Int.toNat nc
                else panic! "Error"
  }


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
  | addc    : Nat → Cmd
  | subc    : Nat → Cmd
  | bool    : Cmd
deriving Repr

def processCmd (st: CompilerState): Cmd → CompilerState
  | Cmd.clear =>
      appendCode "[-]" 0 st
  | Cmd.copy =>
      appendCode "[>+>+<<-]>>[<<+>>-]<" 1 st
  | Cmd.get var =>
      appendCode (moveRight (adrLocal var st)) 1 st
  | Cmd.set var =>
      appendCode (moveLeft (adrLocal var st)) (-1) st
  | Cmd.read =>
      appendCode ">," 1 st
  | Cmd.write =>
      appendCode ".[-]<" (-1) st
  | Cmd.push n =>
      appendCode (">" ++ number n) 1 st
  | Cmd.inc =>
      appendCode "+" 0 st
  | Cmd.dec =>
      appendCode "-" 0 st
  | Cmd.add =>
      appendCode "[<+>-]<" (-1) st
  | Cmd.sub =>
      appendCode "[<->-]<" (-1) st
  | Cmd.addc n =>
      appendCode (replicate n "+") 0 st
  | Cmd.subc n =>
      appendCode (replicate n "-") 0 st
  | Cmd.bool =>
      appendCode "[[-]>+<]>[<+>-]<" 0 st


def scope (letvars: List String) (cmds: List Cmd): CompilerState :=
  let State := letvars.foldl (fun s var =>
        let idx := s.nextCell
        let newEnv := s.env.insert var idx
        let s' := appendCode (">") 1 s
        { s' with env := newEnv }
      ) initState
  cmds.foldl processCmd State


def exampleProgram: CompilerState :=
  scope ["a","b"] [
    -- Cmd.push 55,
    -- Cmd.write,
    -- Cmd.push 10,
    -- Cmd.push 10,
    -- Cmd.add,
    -- Cmd.write,
    Cmd.push 1,
    Cmd.push 2,
    Cmd.copy,
  ]

-- 文字コード確認
#eval textEncoder "ABCDE"
#eval textEncoder "abcde"

#eval moveLeft 1

-- #eval exampleProgram
#eval IO.println ( "```bf\n" ++ exampleProgram.code ++ "```")
