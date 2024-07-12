/-
// lexer/lexer_test.go

package lexer
import (
 "testing"
 "monkey/token"
)
func TestNextToken(t *testing.T) {
  input := `=+(){},;`
  tests := []struct {
    expectedType token.TokenType
    expectedLiteral string
  }{
    {token.ASSIGN, "="},
    {token.PLUS, "+"},
    {token.LPAREN, "("},
    {token.RPAREN, ")"},

    {token.LBRACE, "{"},
    {token.RBRACE, "}"},
    {token.COMMA, ","},
    {token.SEMICOLON, ";"},
    {token.EOF, ""},
  }
  l := New(input)
  for i, tt := range tests {
    tok := l.NextToken()
    if tok.Type != tt.expectedType {
      t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
      i, tt.expectedType, tok.Type)
    }
    if tok.Literal != tt.expectedLiteral {
      t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
      i, tt.expectedLiteral, tok.Literal)
    }
  }
}
-/
import InterpreterBook.Token

opaque Lexer : Type

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
  -- let l : Lexer := Lexer.mk input
  -- for tt in tests do
  --   let tok := l.nextToken
  --   if tok.type
  sorry
