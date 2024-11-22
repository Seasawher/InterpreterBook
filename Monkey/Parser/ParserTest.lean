import Monkey.Lexer.Lexer
import Monkey.Parser.Parser

/-- 一般的な `let` 文に対する parser のテスト -/
def testLetStatement (stmt : Statement) (expectedId : String) : IO Bool := do
  let .letStmt actualId _val := stmt
    | throw <| .userError s!"not expected statement. got={stmt}, expected=Stmt.letStmt"

  -- 期待される識別子と実際の識別子が一致するか
  if actualId != expectedId then
    IO.eprintln s!"not expected identifier. got={actualId} expected={expectedId}"
    return false

  return true

/-- Parser にエラーが一つでもあれば全部出力する -/
def checkParserErrors (p : Parser) : IO Unit := do
  if p.errors.isEmpty then
    return

  for err in p.errors do
    IO.eprintln err

  throw <| .userError "parser has errors"

/-- 具体的な `let` 文に対する parser のテスト -/
def testLetStatements : IO Unit := do
  let input := "
    let x = 5;
    let y = 10;
    let foobar = 838383;"

  let l := Lexer.new input
  let p := Parser.new l

  let ⟨result, parser⟩ := p.parseProgram
  checkParserErrors parser

  let some program := result | IO.eprintln s!"ParseProgram returned none"
  IO.println s!"given program={program}"

  -- 入力の文はちょうど３つのはず
  if program.length != 3 then
    throw <| .userError s!"program.Statements does not contain 3 statements. got={program.length}"

  -- 期待される識別子
  let expectedId := ["x", "y", "foobar"]
  for (id, stmt) in List.zip expectedId program do
    if ! (← testLetStatement stmt id) then
      throw <| .userError s!"testLetStatement failed"

  IO.println "ok!"

#eval testLetStatements


/-- 具体的な return 文に対するテスト -/
def testReturnStatements : IO Unit := do
  let input := "
    return 5;
    return 10;
    return 993322;"

  let l := Lexer.new input
  let p := Parser.new l

  let ⟨result, parser⟩ := p.parseProgram
  checkParserErrors parser

  let some program := result | IO.eprintln s!"ParseProgram returned none"
  IO.println s!"given program={program}"

  -- 入力の文はちょうど３つのはず
  if program.length != 3 then
    throw <| .userError s!"program.Statements does not contain 3 statements. got={program.length}"

  -- return 文だけで構成されていることを確かめる
  for stmt in program do
    let .returnStmt _val := stmt
      | throw <| .userError s!"not expected statement. got={stmt}, expected=Stmt.returnStmt"

#eval testReturnStatements


/-- 一般的な識別子に対するテスト -/
def testIdentifierExpression : IO Unit := do
  let input := "foobar;"

  let l := Lexer.new input
  let p := Parser.new l
  let ⟨result, parser⟩ := p.parseProgram
  checkParserErrors parser

  let some program := result
    | IO.eprintln s!"ParseProgram returned none"

  if program.length != 1 then
    throw <| .userError s!"program length is not equal to 1. got={program.length}"

  let [Statement.exprStmt stmt] := program
    | throw <| .userError s!"Statement.exprStmt is expected. got={program}"

  let Expression.identifier id := stmt
    | throw <| .userError s!"Expression.identifier is expected. got={stmt}"

  if id != "foobar" then
    throw <| .userError s!"ident is expected to be 'foobar'. got={id}"

#eval testIdentifierExpression
