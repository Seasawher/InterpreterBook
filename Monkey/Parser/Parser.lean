import Monkey.Ast.Ast
import Monkey.Lexer.Lexer

/-- 構文解析器 -/
structure Parser where
  /-- レキサー -/
  l : Lexer

  /-- 現在のトークン -/
  curToken : Token

  /-- 次のトークン -/
  peekToken : Token

/-- curToken と peekToken を次に進める -/
def Parser.nextToken : StateM Parser Unit := do
  let p ← get
  let ⟨newToken, newLexer⟩ := p.l.nextToken
  let newParser := Parser.mk newLexer p.peekToken newToken
  set newParser

/-- 新しくパーサを作る -/
def Parser.new (l : Lexer) : Parser :=
  -- なんで型が通るのかよくわからない…
  -- Id モナドは無言で取り出せる
  let (curToken, l') := l.nextToken
  let (peekToken, l'') := l'.nextToken
  { l := l'', curToken, peekToken }

/-- プログラムをパースする -/
def Parser.parseProgram : Program :=
  List.nil
