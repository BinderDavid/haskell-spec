import HaskellSpec.Lexer.Tokens
import HaskellSpec.Lexer.RegExp

/-
Rules
-/

inductive Rule : Type where
  | mk : RE → (String → Token) → Rule

def re : Rule → RE := λ x =>
  match x with
  | Rule.mk re _ => re

/-- `p` is a prefix of `z` -/
def Prefix (p z : List Char) : Prop :=
  ∃ s, p ++ s = z

/-- `p` is the longest prefix of `z` matching a rule in the set `R` -/
def MaxPref (R : List Rule) (p z : List Char) : Prop :=
  /- `p` is a prefix of  `z`-/
  Prefix p z ∧
  /- `p` matches against a rule in `R`-/
  (∃ r ∈ R, Matching (re r) p) ∧
  /- All longer prefixes don't match -/
  (∀ p', Prefix p' z ∧ p.length < p'.length →
         ∀ r' ∈ R, ¬ (Matching (re r') p'))
