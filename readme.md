# BrainFuck

LEAN4を使ってBrainFuckのプログラムを書く試み

## コマンド (powershell)

### bfファイル作成
```powershell
(lean bf.lean | Out-String) -replace '(?s).*```bf\r\n(.*?)\r\n```.*', '$1' > out.bf
```
### 実行
#### 文字列で確認
```powershell
wsl bf out.bf
```
#### 数字も確認
```powershell
(wsl bf out.bf | Out-String).TrimEnd("`r", "`n").ToCharArray() | ForEach-Object { "$([int]$_) $_" }$_" }
```

### 結合
#### 文字列で確認
```powershell
(lean bf.lean | Out-String) -replace '(?s).*```bf\r\n(.*?)\r\n```.*', '$1' > out.bf; wsl bf out.bf
```
#### 数字も確認
```powershell
(lean bf.lean | Out-String) -replace '(?s).*```bf\r\n(.*?)\r\n```.*', '$1' > out.bf; (wsl bf out.bf | Out-String).TrimEnd("`r", "`n").ToCharArray() | ForEach-Object { "$([int]$_) $_" }
```