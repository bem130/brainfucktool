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

-- Initial state with an empty environment, starting cell index 0, and no generated code.
def initState : CompilerState :=
  { env := Std.HashMap.empty, nextCell := 0, code := "" }

instance : Inhabited CompilerState :=
  { default := initState }


-- 変数がある相対アドレスを計算する
def adrLocal (var: String) (st: CompilerState): Nat :=
  match st.env.get? var with
  | some val => st.nextCell - val
  | none => panic! "Error: Undefined Variable Name"

-- Helper function to append generated code.
def appendCode (s: String) (n: Int) (st: CompilerState): CompilerState :=
  let nc := st.nextCell+n
  { st with
    code := st.code ++ s ++ " #"++(toString nc)++"\n",
    nextCell := if nc>=0 then
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
  | mul     : Cmd
  | addc    : Nat → Cmd
  | subc    : Nat → Cmd
  | bool    : Cmd
  | ifThen  : List Cmd → Cmd
  | stat    : List Cmd → Cmd
deriving Repr

-- 相互再帰定義：processCmdList と processCmd を mutual ブロックで定義
mutual
  def processCmdList (st : CompilerState) (cmds : List Cmd) : CompilerState :=
    match cmds with
    | []      => st
    | c :: cs => processCmdList (processCmd st c) cs

def processCmd (st: CompilerState): Cmd → CompilerState
  | Cmd.clear =>
      appendCode ("/* clear */   "++"[-]") 0 st
  | Cmd.copy =>
      appendCode ("/* copy */   "++"[>+>+<<-]>>[<<+>>-]<") 1 st
  | Cmd.get var =>
      appendCode ("/* get "++var++" */   "++">"++(copyRight (1+(adrLocal var st)))) 1 st
  | Cmd.set var =>
      appendCode ("/* set "++var++" */   "++(moveLeft (adrLocal var st))++"<") (-1) st
  | Cmd.read =>
      appendCode ("/* read */   "++">,") 1 st
  | Cmd.write =>
      appendCode ("/* write */   "++".[-]<") (-1) st
  | Cmd.push n =>
      appendCode ("/* push "++(toString n)++" */   "++(">" ++ number n)) 1 st
  | Cmd.inc =>
      appendCode ("/* inc */   "++"+") 0 st
  | Cmd.dec =>
      appendCode ("/* dec */   "++"-") 0 st
  | Cmd.add =>
      appendCode ("/* add */   "++"[<+>-]<") (-1) st
  | Cmd.sub =>
      appendCode ("/* sub */   "++"[<->-]<") (-1) st
  | Cmd.mul =>
      appendCode ("/* mul */   "++"<[>>+<<-]>[>[<<+>>>+<-]>[<+>-]<<-]>[-]<<") (-1) st
  | Cmd.addc n =>
      appendCode ("/* addc "++(toString n)++" */   "++(replicate n "+")) 0 st
  | Cmd.subc n =>
      appendCode ("/* subc "++(toString n)++" */   "++(replicate n "-")) 0 st
  | Cmd.bool =>
      appendCode ("/* bool */   "++"[[-]>+<]>[<+>-]<") 0 st
  | Cmd.ifThen cmds =>
      -- 内側ブロックを空のコードバッファで開始する
      let innerState := processCmdList { st with code := "" } cmds
      -- 内側ブロックのコードをif文として外側に追加し、nextCellを更新する
      appendCode ("/* if then */   " ++ "[\n" ++ innerState.code ++ "/* end if */   " ++ "[-]]<") 0 { st with nextCell := innerState.nextCell }
  | Cmd.stat cmds =>
      -- 現在のスタックポインタを保存
      let savedCell := st.nextCell
      -- 内部ブロックを空のコードバッファで処理
      let innerState := processCmdList { st with code := "" } cmds
      -- スタックポインタが変わっていないかチェック
      if innerState.nextCell ≠ savedCell then
          panic! "Error: Stack pointer changed in stat block"
      else
          appendCode ("/* stat */\n" ++ innerState.code ++ "/* end stat */") 0 st
end

def scope (letvars: List String) (cmds: List Cmd): CompilerState :=
  let State := letvars.foldl (fun s var =>
        let idx := s.nextCell
        let newEnv := s.env.insert var idx
        let s' := appendCode ("/* let "++var++" */   "++">") 1 s
        { s' with env := newEnv }
      ) initState
  cmds.foldl processCmd State


def exampleProgram: CompilerState :=
  scope ["a"] [
    Cmd.stat [
      Cmd.push 5,
      Cmd.push 2,
      Cmd.add,
      Cmd.push 3,
      Cmd.sub,
      Cmd.push 10,
      Cmd.mul,
      Cmd.write,
    ],
    Cmd.push 1, -- true
    Cmd.ifThen [ -- if文
      Cmd.push 5,
      Cmd.push 50,
      Cmd.write,
      Cmd.set "a",
    ],
  ]

-- 文字コード確認
#eval textEncoder "ABCDE"
#eval textEncoder "abcde"

#eval moveRight 1

-- #eval exampleProgram
#eval IO.println ( "```bf\n" ++ exampleProgram.code ++ "```")
