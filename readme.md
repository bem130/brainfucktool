# BrainFuck

LEAN4を使ってBrainFuckのプログラムを書く試み

## コマンド (powershell)

### bfファイル作成
```powershell
(lean bf.lean | Out-String) -replace '(?s).*```bf\r\n(.*?)\r\n```.*', '$1' > out.bf
```
### 実行
```powershell
cargo run --bin bfir -- out.bf
```

### 結合
```powershell
(lean bf.lean | Out-String) -replace '(?s).*```bf\r\n(.*?)\r\n```.*', '$1' > out.bf; cargo run --bin bfir -- out.bf
```