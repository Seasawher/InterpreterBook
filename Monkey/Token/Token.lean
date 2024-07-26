
inductive TokenType where
  | ILLEGAL
  | EOF
  | IDENT
  | INT
  | ASSIGN
  | PLUS
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET
deriving Repr, DecidableEq

def TokenType.toString (t : TokenType) : String :=
  match t with
  | .ILLEGAL => "ILLEGAL"
  | .EOF => "EOF"
  | .IDENT => "IDENT"
  | .INT => "INT"
  | .ASSIGN => "="
  | .PLUS => "+"
  | .COMMA => ","
  | .SEMICOLON => ";"
  | .LPAREN => "("
  | .RPAREN => ")"
  | .LBRACE => "{"
  | .RBRACE => "}"
  | .FUNCTION => "FUNCTION"
  | .LET => "LET"

instance : ToString TokenType where
  toString := TokenType.toString

structure Token where
  type : TokenType
  literal : String
deriving Repr, BEq, DecidableEq
