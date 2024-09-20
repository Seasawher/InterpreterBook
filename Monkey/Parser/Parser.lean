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
def Parser.nextToken : StateM Parser PUnit := do
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

/-- p の curToken が指定されたトークンと種類が一致するか -/
def Parser.curTokenIs (p : Parser) (t : Token) : Bool :=
  Token.sameType p.curToken t

/-- p の peekToken が指定されたトークンと種類が一致するか -/
def Parser.peekTokenIs (p : Parser) (t : Token) : Bool :=
  Token.sameType p.peekToken t

/-- p の peekToken が指定されたトークンと種類が一致するか判定。一致した場合は次に進める -/
def Parser.expectPeek (t : Token) : StateM Parser Bool := do
  let p ← get
  if ! p.peekTokenIs t then
    return false

  _ ← p.nextToken
  return true

/-- let 文をパースする -/
def Parser.parseLetStatement : StateM Parser (Option Statement) := do
  let p ← get
  -- if ! (← p.expectPeek (Token.IDENT "")) then
  --   return none
  sorry

/-- 一文をパースする -/
def Parser.parseStatement (p : Parser) : Option Statement :=
  -- match h : p.curToken with
  -- | .LET => p.parseLetStatement h
  -- | _ => none
  sorry

/-- プログラムをパースする -/
def Parser.parseProgram (p : Parser) : Option Program := do
  let mut program : Program := []
  let mut curParser := p

  while curParser.curToken != Token.EOF do
    let stmt ← curParser.parseStatement
    program := stmt :: program
    curParser := Parser.nextToken curParser |>.snd

  program := program.reverse
  return program
