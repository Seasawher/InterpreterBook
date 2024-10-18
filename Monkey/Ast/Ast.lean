import Monkey.Token.Token

/-- 証明に対する sorryAx -/
axiom sorryAxForProof {P : Prop} : P

/-- 証明を省略するため専用の sorry -/
macro "sorry_proof" : tactic => `(tactic| apply sorryAxForProof)

/-- 式 -/
inductive Expression where
  /-- 識別子 -/
  | identifier (token : Token) (h : ∃ name, token = .IDENT name := by decide) : Expression

  /-- `Expression` の未実装の部分を表す -/
  | notImplemented

deriving Repr, DecidableEq

/-- Expression を文字列に変換する -/
def Expression.toString (e : Expression) : String :=
  match e with
  | .identifier token _ => s!"{token}"
  | .notImplemented => "notImplemented"

instance : ToString Expression where
  toString e := e.toString

/-- Expression を文字列に変換する -/
def Expression.tokenLiteral (e : Expression) : String :=
  -- match e with
  -- | .identifier token _ => s!"{token}"
  -- | .notImplemented => default
  "Expression.tokenLiteral is not implemented"

/-- 文。本文とは異なる実装を採用しており、`statementNode()` に相当するものは不要。-/
inductive Statement where
  /-- let 文 -/
  | letStmt (name : String) (value : Expression) : Statement

  /-- Statement の未実装の部分を表す -/
  | notImplemented

deriving Repr, DecidableEq

/-- Statement を文字列に変換する -/
def Statement.toString (stmt: Statement) : String :=
  match stmt with
  | .letStmt name value => s!"let {name} = {value}"
  | .notImplemented => "notImplemented"

/-- Repr インスタンスから ToString インスタンスを生成する -/
instance : ToString Statement where
  toString s := s.toString

def Statement.tokenLiteral (s : Statement) : String :=
  -- match s with
  -- | .letStmt token _ _ => s!"{token}"
  -- | .notImplemented => default
  "Statement.tokenLiteral is not implemented"

/-- AST のノード -/
inductive Node where
  /-- 文 -/
  | ofStmt (s : Statement) : Node

  /-- 式 -/
  | ofExpr (e : Expression) : Node

/-- プログラムを文の集まりとして定義する -/
abbrev Program := List Statement

/-- Program の ToString 関数に相当するもの -/
def Program.tokenLiteral (p : Program) : String :=
  -- match p with
  -- | [] => ""
  -- | p :: _ => p.tokenLiteral
  "Program.tokenLiteral is not implemented"
