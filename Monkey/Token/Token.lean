import Lean.Data.HashMap

/-- Token の種類 -/
inductive TokenType where
  /-- 受け入れ不能エラー -/
  | ILLEGAL
  /-- ファイル終端 -/
  | EOF
  /-- 識別子 -/
  | IDENT
  /-- 整数 -/
  | INT
  /-- 代入記号 "=" -/
  | ASSIGN
  /-- 足し算記号 + -/
  | PLUS
  /-- コンマ , -/
  | COMMA
  /-- セミコロン ; -/
  | SEMICOLON
  /-- 開き括弧 ( -/
  | LPAREN
  /-- 閉じ括弧 ) -/
  | RPAREN
  /-- 開き波括弧 -/
  | LBRACE
  /-- 閉じ波括弧 -/
  | RBRACE
  /-- 無名関数 fn -/
  | FUNCTION
  /-- LET キーワード -/
  | LET
  /-- 引き算記号 "-" -/
  | MINUS
  /-- ビックリマーク ! -/
  | BANG
  /-- アスタリスク * -/
  | ASTERISK
  /-- スラッシュ "/" -/
  | SLASH
  /-- 小なり "<" -/
  | LT
  /-- 大なり ">" -/
  | GT
  /-- true : Bool -/
  | TRUE
  /-- false : Bool -/
  | FALSE
  /-- IF キーワード -/
  | IF
  /-- ELSE キーワード -/
  | ELSE
  /-- RETURN キーワード -/
  | RETURN
deriving Repr, DecidableEq

/-- TokenType を文字列に変換する -/
def TokenType.toString (t : TokenType) : String :=
  match t with
  | .ILLEGAL => "ILLEGAL"
  | .EOF => "EOF"
  | .IDENT => "IDENT"
  | .INT => "INT"
  | .ASSIGN => "="
  | .PLUS => "+"
  | .COMMA => ","
  | .SEMICOLON => ";"
  | .LPAREN => "("
  | .RPAREN => ")"
  | .LBRACE => "{"
  | .RBRACE => "}"
  | .FUNCTION => "FUNCTION"
  | .LET => "LET"
  | .MINUS => "-"
  | .BANG => "!"
  | .ASTERISK => "*"
  | .SLASH => "/"
  | .LT => "<"
  | .GT => ">"
  | .TRUE => "TRUE"
  | .FALSE => "FALSE"
  | .IF => "IF"
  | .ELSE => "ELSE"
  | .RETURN => "RETURN"

instance : ToString TokenType where
  toString := TokenType.toString

/-- トークン

本にある Token の定義では structure になっていたので、
literal が必要ないのときでも literal の情報があって冗長だった。
それを修正した定義にしている。 -/
inductive Token where
  /-- 識別子 -/
  | IDENT (name : String)
  /-- 数値リテラル -/
  | INT (value : String)
  /-- 受け入れ不能エラー -/
  | ILLEGAL
  /-- ファイル終端 -/
  | EOF
  /-- 代入記号 "=" -/
  | ASSIGN
  /-- 足し算記号 + -/
  | PLUS
  /-- コンマ , -/
  | COMMA
  /-- セミコロン ; -/
  | SEMICOLON
  /-- 開き括弧 ( -/
  | LPAREN
  /-- 閉じ括弧 ) -/
  | RPAREN
  /-- 開き波括弧 -/
  | LBRACE
  /-- 閉じ波括弧 -/
  | RBRACE
  /-- 無名関数 fn -/
  | FUNCTION
  /-- LET キーワード -/
  | LET
  /-- 引き算記号 "-" -/
  | MINUS
  /-- ビックリマーク ! -/
  | BANG
  /-- アスタリスク * -/
  | ASTERISK
  /-- スラッシュ "/" -/
  | SLASH
  /-- 小なり "<" -/
  | LT
  /-- 大なり ">" -/
  | GT
  /-- true : Bool -/
  | TRUE
  /-- false : Bool -/
  | FALSE
  /-- IF キーワード -/
  | IF
  /-- ELSE キーワード -/
  | ELSE
  /-- RETURN キーワード -/
  | RETURN
deriving Repr, BEq, DecidableEq

open TokenType Lean

/-- 言語のキーワード -/
def keywords : HashMap String TokenType :=
  let list : List (String × TokenType) := [
    ("fn", FUNCTION),
    ("let", LET),
    ("true", TRUE),
    ("false", FALSE),
    ("if", IF),
    ("else", ELSE),
    ("return", RETURN),
  ]
  HashMap.ofList list

/-- ユーザ定義の識別子なのか、言語のキーワードなのか分類する -/
def LookupIdent (ident : String) : TokenType :=
  match keywords.find? ident with
  | some tok => tok
  | none => IDENT
