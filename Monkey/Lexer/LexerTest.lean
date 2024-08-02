import Monkey.Lexer.Lexer
import Monkey.Token.Token

open TokenType Lexer

/-- NestToken 関数をテストする -/
def testNextToken (input : String) (expected : Array (TokenType × String)) : IO Unit := do
  let mut l : Lexer := Lexer.new input
  for tt in expected do
    let ⟨tok, l'⟩ := l.nextToken |>.run
    l := l'
    if tok.type ≠ tt.fst then
      throw <| .userError s!"tests failed: - tokentype wrong at \"{tt.snd}\". expected={tt.fst}, got={tok.type}"
    if tok.literal ≠ tt.snd then
      throw <| .userError s!"tests failed: - literal wrong. expected={tt.snd}, got={tok.literal}"

  IO.println s!"ok!"

#eval testNextToken
  (input := "=+(){},;")
  (expected := #[
    (ASSIGN, "="),
    (PLUS, "+"),
    (LPAREN, "("),
    (RPAREN, ")"),
    (LBRACE, "{"),
    (RBRACE, "}"),
    (COMMA, ","),
    (SEMICOLON, ";"),
    (EOF, "")
  ])

#eval testNextToken
  (input := "let five = 5;
  let ten = 10;
  let add = fn(x, y) {
  x + y;
  };
  let result = add(five, ten);
  ")
  (expected := #[
    (LET, "let"),
    (IDENT, "five"),
    (ASSIGN, "="),
    (INT, "5"),
    (SEMICOLON, ";"),
    (LET, "let"),
    (IDENT, "ten"),
    (ASSIGN, "="),
    (INT, "10"),
    (SEMICOLON, ";"),
    (LET, "let"),
    (IDENT, "add"),
    (ASSIGN, "="),
    (FUNCTION, "fn"),
    (LPAREN, "("),
    (IDENT, "x"),
    (COMMA, ","),
    (IDENT, "y"),
    (RPAREN, ")"),
    (LBRACE, "{"),
    (IDENT, "x"),
    (PLUS, "+"),
    (IDENT, "y"),
    (SEMICOLON, ";"),
    (RBRACE, "}"),
    (SEMICOLON, ";"),
    (LET, "let"),
    (IDENT, "result"),
    (ASSIGN, "="),
    (IDENT, "add"),
    (LPAREN, "("),
    (IDENT, "five"),
    (COMMA, ","),
    (IDENT, "ten"),
    (RPAREN, ")"),
    (SEMICOLON, ";"),
    (EOF, "")
  ])

#eval testNextToken
  (input := "!-/*5;
  5 < 10 > 5;")
  (expected := #[
    (BANG, "!"),
    (MINUS, "-"),
    (SLASH, "/"),
    (ASTERISK, "*"),
    (INT, "5"),
    (SEMICOLON, ";"),
    (INT, "5"),
    (LT, "<"),
    (INT, "10"),
    (GT, ">"),
    (INT, "5")
  ])
