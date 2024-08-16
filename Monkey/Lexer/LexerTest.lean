import Monkey.Lexer.Lexer
import Monkey.Token.Token

open Token Lexer

/-- NestToken 関数をテストする -/
def testNextToken (input : String) (expected : Array Token) : IO Unit := do
  let mut l : Lexer := Lexer.new input
  for tt in expected do
  for expTok in expected do
    let ⟨tok, l'⟩ := l.nextToken |>.run
    l := l'
    if tok ≠ expTok then
      throw <| .userError s!"tests failed: - token wrong at \"{expTok}\". expected={expTok}, got={tok}"

  IO.println s!"ok!"

#eval testNextToken
  (input := "=+(){},;")
  (expected := #[
    ASSIGN,
    PLUS,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMICOLON,
    EOF
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
    LET,
    IDENT "five",
    ASSIGN,
    INT 5,
    SEMICOLON,
    LET,
    IDENT "ten",
    ASSIGN,
    INT 10,
    SEMICOLON,
    LET,
    IDENT "add",
    ASSIGN,
    FUNCTION,
    LPAREN,
    IDENT "x",
    COMMA,
    IDENT "y",
    RPAREN,
    LBRACE,
    IDENT "x",
    PLUS,
    IDENT "y",
    SEMICOLON,
    RBRACE,
    SEMICOLON,
    LET,
    IDENT "result",
    ASSIGN,
    IDENT "add",
    LPAREN,
    IDENT "five",
    COMMA,
    IDENT "ten",
    RPAREN,
    SEMICOLON,
    EOF
  ])

#eval testNextToken
  (input := "!-/*5;
  5 < 10 > 5;")
  (expected := #[
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    INT 5,
    SEMICOLON,
    INT 5,
    LT,
    INT 10,
    GT,
    INT 5,
    SEMICOLON,
    EOF
  ])

#eval testNextToken
  (input := "if (5 < 10) {
  return true;
  } else {
  return false;
  }")
  (expected := #[
    IF,
    LPAREN,
    INT 5,
    LT,
    INT 10,
    RPAREN,
    LBRACE,
    RETURN,
    TRUE,
    SEMICOLON,
    RBRACE,
    ELSE,
    LBRACE,
    RETURN,
    FALSE,
    SEMICOLON,
    RBRACE,
    EOF
  ])
