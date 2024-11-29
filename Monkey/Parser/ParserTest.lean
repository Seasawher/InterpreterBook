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

  throw <| .userError (String.intercalate "\n" p.errors)

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

/-- 整数リテラルに対するテスト -/
def testIntegerLiteralExpression : IO Unit := do
  let input := "5;"

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

  let Expression.integerLiteral value := stmt
    | throw <| .userError s!"Expression.integerLiteral is expected. got={stmt}"

  if value != 5 then
    throw <| .userError s!"value is expected to be 5. got={value}"

#eval testIntegerLiteralExpression

private structure PrefixTestCase where
  input : String
  operator : Token
  integer : Int

/-- 前置演算子に対するテスト -/
def testParsingPrefixExpressions : IO Unit := do
  let prefixTests : Array PrefixTestCase := #[
    { input := "!5;", operator := .BANG, integer := 5 },
    { input := "-15;", operator := .MINUS, integer := 15 }
  ]

  for testCase in prefixTests do
    let l := Lexer.new testCase.input
    let p := Parser.new l
    let ⟨result, parser⟩ := p.parseProgram
    checkParserErrors parser

    let some program := result
      | IO.eprintln s!"ParseProgram returned none"

    if program.length != 1 then
      throw <| .userError s!"program.Statements does not contain 1 statement. got={program.length}"

    let [Statement.exprStmt stmt] := program
      | throw <| .userError s!"Statement.exprStmt is expected. got={program}"

    let Expression.prefix operator right := stmt
      | throw <| .userError s!"Expression.prefix is expected. got={stmt}"

    if operator != testCase.operator then
      throw <| .userError s!"operator is expected to be {testCase.operator}. got={operator}"

    let Expression.integerLiteral value := right
      | throw <| .userError s!"Expression.integerLiteral is expected. got={right}"

    if value != testCase.integer then
      throw <| .userError s!"value is expected to be {testCase.integer}. got={value}"

-- #eval testParsingPrefixExpressions
