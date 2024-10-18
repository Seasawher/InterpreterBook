import Monkey.Ast.Ast
import Monkey.Lexer.Lexer
import Lean

/-- 構文解析器 -/
structure Parser where
  /-- レキサー -/
  l : Lexer

  /-- 現在のトークン -/
  curToken : Token

  /-- 次のトークン -/
  peekToken : Token

  /-- 構文解析エラー -/
  errors : List String

/-- Parser を文字列に変換する -/
def Parser.toString (p : Parser) : String :=
  s!"⟨curToken={p.curToken}, peekToken={p.peekToken}⟩ : Parser"

instance : ToString Parser where
  toString := Parser.toString

/-- curToken と peekToken を次に進める -/
def Parser.nextToken : StateM Parser PUnit := do
  let p ← get
  let ⟨newToken, newLexer⟩ := p.l.nextToken
  let newParser : Parser := {
    l := newLexer,
    curToken := p.peekToken,
    peekToken := newToken,
    errors := p.errors
  }
  set newParser

/-- 新しくパーサを作る -/
def Parser.new (l : Lexer) : Parser :=
  -- Id モナドは無言で取り出せる
  let (curToken, l') := l.nextToken
  let (peekToken, l'') := l'.nextToken
  { l := l'', curToken, peekToken, errors := []}

/-- p の curToken が指定されたトークンと種類が一致するか -/
def Parser.curTokenIs (p : Parser) (t : Token) : Bool :=
  Token.sameType p.curToken t

/-- p の peekToken が指定されたトークンと種類が一致するか -/
def Parser.peekTokenIs (p : Parser) (t : Token) : Bool :=
  Token.sameType p.peekToken t

/-- `expectPeek` の過程でエラーが起きたときのために、
エラーメッセージを蓄積する処理 -/
def Parser.peekError (expectedToken : String) : StateM Parser Unit := do
  let p ← get
  set { p with errors := p.errors ++ [s!"expected next token to be {expectedToken}, got {p.peekToken} instead"] }

open Lean Parser Term in
/-- p の peekToken が指定されたトークン `pat` と種類が一致するか判定。一致した場合は次に進める -/
macro "expectPeek " pat:term rest:doSeqItem* : doElem => do
  let pat' : Lean.StrLit := pat.raw.getSubstring?.get! |> toString |> Lean.Syntax.mkStrLit
  `(doElem| do
    let $pat := (← get).peekToken
      | Parser.peekError $pat'
        return none
    nextToken
    $rest*
  )

/-- let 文をパースする -/
def Parser.parseLetStatement : StateM Parser (Option Statement) := do
  expectPeek .IDENT name
  expectPeek .ASSIGN

  while ! (← get).curTokenIs (Token.SEMICOLON) do
    nextToken

  return Statement.letStmt name Expression.notImplemented

/-- 一文をパースする -/
def Parser.parseStatement : StateM Parser (Option Statement) := do
  match (← get).curToken with
  | .LET => parseLetStatement
  | _ => return none

/-- プログラムをパースする -/
def Parser.parseProgram : StateM Parser (Option Program) := do
  let mut program : Program := []

  while (← get).curToken != Token.EOF do
    let some stmt ← parseStatement | return none
    program := stmt :: program
    nextToken

  program := program.reverse
  return program

#eval
  let input := "
    let x = 5;
    let y = 10;
    let foobar = 838383;"

  let l := Lexer.new input
  let p := Parser.new l

  ToString.toString (p.parseProgram |>.run |>.fst)
