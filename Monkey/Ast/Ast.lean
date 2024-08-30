
/-- 式 -/
inductive Expression where

/-- 文 -/
inductive Statement where

/-- Statement の ToString 関数に相当するもの -/
def Statement.tokenLiteral (s : Statement) : String := "hello Statement!"

/-- AST のノード -/
inductive Node where
  /-- 文 -/
  | ofStmt (s : Statement) : Node

  /-- 式 -/
  | ofExpr (e : Expression) : Node

/-- プログラムを文の集まりとして定義する -/
def Program := List Statement

/-- Program の ToString 関数に相当するもの -/
def Program.tokenLiteral (p : Program) : String :=
  match p with
  | [] => ""
  | p :: _ => p.tokenLiteral
