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

namespace Lexer

def readChar (l : Lexer) : Lexer :=
  let l' := if l.readPosition ≥ l.input.length
    then { l with ch := '\x00' }
    else { l with ch := l.input.get ⟨l.readPosition⟩}
  { l' with position := l.readPosition, readPosition := l.readPosition + 1 }

def new (input : String) (position readPosition : Nat := 0) (ch : Char := '\x00') : Lexer := Id.run do
  let mut l := { input := input, position := position, readPosition := readPosition, ch := ch }
  l := l.readChar
  return l

open TokenType

def nextToken : StateM Lexer Token := do
  let mut l ← get
  let tok := match l.ch with
    | '=' => Token.mk ASSIGN (String.singleton l.ch)
    | '+' => Token.mk PLUS (String.singleton l.ch)
    | '(' => Token.mk LPAREN (String.singleton l.ch)
    | ')' => Token.mk RPAREN (String.singleton l.ch)
    | '{' => Token.mk LBRACE (String.singleton l.ch)
    | '}' => Token.mk RBRACE (String.singleton l.ch)
    | ',' => Token.mk COMMA (String.singleton l.ch)
    | ';' => Token.mk SEMICOLON (String.singleton l.ch)
    | '\x00' => Token.mk EOF ""
    | _ =>
      if l.ch.isAlpha then
        sorry
      else
        Token.mk ILLEGAL (String.singleton l.ch)
  set l.readChar
  return tok

end Lexer
