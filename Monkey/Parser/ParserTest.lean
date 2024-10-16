import Monkey.Lexer.Lexer
import Monkey.Parser.Parser

/-- 一般的な `let` 文に対する parser のテスト -/
def testLetStatement (stmt : Statement) (expectedId : String) : IO Bool := do
  -- なぜか failed to infer binder type エラーになる
  -- let Statement.letStmt token name val := stmt

  -- Statement に let 以外のものを増やすと将来的に動かなくなる
  let .letStmt actualId _val := stmt
    | throw <| .userError s!"not expected statement. got={stmt}, expected=Stmt.letStmt"

  -- -- LET が来ないとエラー
  -- if token != Token.LET then
  --   IO.eprintln s!"not LET. got={token}"
  --   return false

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

/- ## TODO:
入力として以下のように複数のエラーが起こる文を与えたとき、
```
  let input := "
    let x 5;
    let = 10;
    let foobar = 838383;"
```
エラーメッセージがなぜ一度しか表示されないのか？-/
#eval testLetStatements
