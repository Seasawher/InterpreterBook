structure Lexer where
  input : String
  position : Nat := 0
  readPosition : Nat := 0
  ch : UInt8 := 0
deriving Repr

def Lexer.new (input : String) (position readPosition : Nat := 0) (ch : UInt8 := 0) : Lexer :=
  { input := input, position := position, readPosition := readPosition, ch := ch }
