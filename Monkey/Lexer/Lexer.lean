import Monkey.Token.Token

structure Lexer where
  input : String
  position : Nat := 0
  readPosition : Nat := 0
  ch : Char := '\x00'
deriving Repr

-- 空文字を表す Char
#guard ⟨0, by decide⟩ = '\x00'

-- アルファベットかどうか判定する
#check Char.isAlpha

/-- 識別子として許可できるような文字列か？
アルファベットであるか、あるいはアンダースコア -/
def Char.isLetter (ch : Char) : Bool := ch.isAlpha || ch == '_'

namespace Lexer

/-- Lexer を１文字読み進める -/
def readChar : StateM Lexer Unit := do
  let mut l ← get
  let l' := if l.readPosition ≥ l.input.length
    then { l with ch := '\x00' }
    else { l with ch := l.input.get ⟨l.readPosition⟩}
  set { l' with position := l.readPosition, readPosition := l.readPosition + 1 }

/-- デフォルト値を持たせたコンストラクタの変種 -/
def mkD (input : String) (position readPosition : Nat := 0)
    (ch : Char := '\x00') : Lexer :=
  { input := input, position := position, readPosition := readPosition, ch := ch }

def new (input : String) : Lexer :=
  StateT.run readChar (Lexer.mkD input) |> Id.run |>.snd

#check StateT.run readChar

/-- Lexer を更新しつつ、letter ではない文字列が出てくるまで読み進める -/
def readIdentifier : StateM Lexer String := do
  let mut l ← get
  let position := l.position
  while l.ch.isLetter do
    readChar
    l ← get
  return l.input
    |>.take l.position
    |>.drop position

open TokenType

/-- Lexer を更新しながら、次のトークンを読む -/
def nextToken : StateM Lexer Token := do
  let mut l ← get
  let mut tok := match l.ch with
    | '=' => Token.mk ASSIGN (String.singleton l.ch)
    | '+' => Token.mk PLUS (String.singleton l.ch)
    | '(' => Token.mk LPAREN (String.singleton l.ch)
    | ')' => Token.mk RPAREN (String.singleton l.ch)
    | '{' => Token.mk LBRACE (String.singleton l.ch)
    | '}' => Token.mk RBRACE (String.singleton l.ch)
    | ',' => Token.mk COMMA (String.singleton l.ch)
    | ';' => Token.mk SEMICOLON (String.singleton l.ch)
    | '\x00' => Token.mk EOF ""
    | _ => Token.mk ILLEGAL (String.singleton l.ch)
  if l.ch.isLetter then
    let literal ← readIdentifier
    tok := Token.mk (if literal == "let" then LET else IDENT) literal
  readChar
  return tok

end Lexer
