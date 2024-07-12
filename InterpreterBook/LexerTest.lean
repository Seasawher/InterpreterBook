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
import InterpreterBook.Basic

opaque Lexer : Type

def Lexer.nextToken (l : Lexer) : Token := sorry

-- def testNextToken : IO Unit := Id.run do
--   let input := "=+(){},;"
--   let tests := #[
--   ]
--   sorry
