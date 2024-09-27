import Monkey.Token.Token

/-- 式 -/
inductive Expression where
  /-- 識別子 -/
  | identifier (token : Token) (value : String) : Expression

deriving Repr, DecidableEq

/-- Expression を文字列に変換する -/
def Expression.tokenLiteral (e : Expression) : String :=
  match e with
  | .identifier token _ => s!"{token}"

/-- 文。本文とは異なる実装を採用しており、`statementNode()` に相当するものは不要。-/
inductive Statement where
  /-- let 文 -/
  | letStmt (token : Token) (name : String) (value : Expression) : Statement

deriving Repr, DecidableEq

instance : ToString Statement where
  toString s := reprStr s

#check Repr.reprPrec

/-- Statement の ToString 関数に相当するもの -/
def Statement.tokenLiteral (s : Statement) : String :=
  match s with
  | .letStmt token _ _ => s!"{token}"

/-- AST のノード -/
inductive Node where
  /-- 文 -/
  | ofStmt (s : Statement) : Node

  /-- 式 -/
  | ofExpr (e : Expression) : Node

/-- プログラムを文の集まりとして定義する -/
abbrev Program := List Statement

-- deriving DecidableEq, Repr

/-- Program の ToString 関数に相当するもの -/
def Program.tokenLiteral (p : Program) : String :=
  match p with
  | [] => ""
  | p :: _ => p.tokenLiteral

-- instance : ToString Program := inferInstance
