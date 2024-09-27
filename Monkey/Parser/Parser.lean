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

/-- Parser を文字列に変換する -/
def Parser.toString (p : Parser) : String :=
  s!"Parser(curToken={p.curToken}, peekToken={p.peekToken})"

instance : ToString Parser where
  toString := Parser.toString

/-- curToken と peekToken を次に進める -/
def Parser.nextToken : StateM Parser PUnit := do
  let p ← get
  let ⟨newToken, newLexer⟩ := p.l.nextToken
  let newParser := Parser.mk newLexer p.peekToken newToken
  set newParser

/-- 新しくパーサを作る -/
def Parser.new (l : Lexer) : Parser :=
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
  let p ← (get : StateM Parser Parser)
  if ! p.peekTokenIs t then
    return false

  nextToken
  return true

/-- let 文をパースする -/
def Parser.parseLetStatement : StateM Parser (Option Statement) := do
  -- TODO: なぜここは StateM モナドの中なのに、
  -- `←` で `Bool × Parser` が取り出されてしまうのか？
  -- `← get` だと `Parser` が取り出されるのに...
  -- let detect ← p.expectPeek (Token.IDENT "")
  -- Answer: `p.expectPeek` のように、`p` を状態として渡すのがよくない
  if ! (← expectPeek (Token.IDENT "")) then
    return none

  -- これは IDENT になっているはずなので name を取り出す
  let .IDENT name := (← get).curToken
    | panic! "curToken is not IDENT"

  if ! (← expectPeek (Token.ASSIGN)) then
    return none

  while ! (← get).curTokenIs (Token.SEMICOLON) do
    nextToken

  let ident := Token.IDENT name
  return Statement.letStmt (token := ident) name (Expression.identifier ident name)

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

  p.parseProgram
