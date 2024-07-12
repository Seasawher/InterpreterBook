import Monkey.Lexer.Lexer
import Monkey.Token.Token

def Lexer.nextToken (l : Lexer) : Token := sorry

open TokenType

def testNextToken : IO Unit := do
  let input := "=+(){},;"
  let tests : Array (TokenType × String) := #[
    (ASSIGN, "="),
    (PLUS, "+"),
    (LPAREN, "("),
    (RPAREN, ")"),
    (LBRACE, "{"),
    (RBRACE, "}"),
    (COMMA, ","),
    (SEMICOLON, ";"),
    (EOF, "")
  ]
  let l : Lexer := Lexer.new input
  for tt in tests do
    let tok := l.nextToken
    if tok.type ≠ tt.fst then
      throw <| .userError s!"tests failed: - tokentype wrong. expected={tt.fst}, got={tok.type}"
    if tok.literal ≠ tt.snd then
      throw <| .userError s!"tests failed: - literal wrong. expected={tt.snd}, got={tok.literal}"

  IO.println s!"ok!"
