import Monkey.Token.Token

/-- 証明に対する sorryAx -/
axiom sorryAxForProof {P : Prop} : P

/-- 証明を省略するため専用の sorry -/
macro "sorry_proof" : tactic => `(tactic| apply sorryAxForProof)

/-- 式 -/
inductive Expression where
  /-- 識別子 -/
  | identifier (name : String) : Expression

  /-- 整数リテラル -/
  | integerLiteral (value : Int) : Expression

  /-- 前置演算子 -/
  | prefix (operator : Token) (right : Expression) : Expression

  /-- `Expression` の未実装の部分を表す -/
  | notImplemented

deriving Repr, DecidableEq

/-- Expression を文字列に変換する -/
def Expression.toString (e : Expression) : String :=
  match e with
  | .identifier name => s!"{name}"
  | .integerLiteral value => s!"{value}"
  | .prefix operator right => s!"({operator} {Expression.toString right})"
  | .notImplemented => "notImplemented"

instance : ToString Expression where
  toString e := e.toString

/-- 文。本文とは異なる実装を採用しており、`statementNode()` に相当するものは不要。-/
inductive Statement where
  /-- let 文 -/
  | letStmt (name : String) (value : Expression) : Statement

  /-- return 文 -/
  | returnStmt (returnValue : Expression) : Statement

  /-- 式文。式だけからなる行を持たせるために必要 -/
  | exprStmt (expr : Expression) : Statement

  /-- Statement の未実装の部分を表す -/
  | notImplemented

deriving Repr, DecidableEq

/-- Statement を文字列に変換する -/
def Statement.toString (stmt: Statement) : String :=
  match stmt with
  | .letStmt name value => s!"let {name} = {value}"
  | .returnStmt returnValue => s!"return {returnValue}"
  | .exprStmt expr => s!"{expr}"
  | .notImplemented => "notImplemented"

/-- Repr インスタンスから ToString インスタンスを生成する -/
instance : ToString Statement where
  toString s := s.toString

/-- AST のノード -/
inductive Node where
  /-- 文 -/
  | ofStmt (s : Statement) : Node

  /-- 式 -/
  | ofExpr (e : Expression) : Node

/-- プログラムを文の集まりとして定義する。AST に相当する。-/
abbrev Program := List Statement

/-- `;` と改行で区切ってプログラムを表示する -/
instance : ToString Program where
  toString p := String.intercalate "\n" <| p.map (fun x => Statement.toString x ++ ";")

#eval toString ([Statement.letStmt "myVar" Expression.notImplemented] : Program)
