import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Haskell.Tokens

/-
Rules
-/

structure Rule : Type where
  re : RE
  action : String → Token

def MatchingR (R : Rule) (ls : List Char) : Prop :=
  Matching R.re ls

/-- `p` is a prefix of `z` -/
def Prefix (p z : List Char) : Prop :=
  ∃ s, p ++ s = z

/-- `p` is the longest prefix of `z` matching a rule in the set `R` -/
def MaxPref (R : List Rule) (p z : List Char) : Prop :=
  /- `p` is a prefix of  `z`-/
  Prefix p z ∧
  /- `p` matches against a rule in `R`-/
  (∃ r ∈ R, MatchingR r p) ∧
  /- All longer prefixes don't match -/
  (∀ p', Prefix p' z ∧ p.length < p'.length →
         ∀ r' ∈ R, ¬ (MatchingR r' p'))

inductive Index {α : Type } : Nat → α → List α → Prop where
  | ZERO : Index Nat.zero x (x :: xs)
  | SUCC : Index n x xs → Index (Nat.succ n) x (y :: xs)

def FirstToken (R : List Rule) (tok : Token) (pre z : List Char) : Prop :=
  /- `pre` must not be empty -/
  pre ≠ [] ∧
  /- `pre` must be a maximal prefix of `z` matching a rule in `R` -/
  MaxPref R pre z ∧
  /- There is a rule `r` in `R` which matches `pre` and produces token `tok` -/
  (∃ n r, Index n r R ∧
          MatchingR r pre ∧
          r.action pre.toString = tok ∧
          /- All other rules which occur earlier in `R` do not match -/
          (∀ r' n', n' < n → Index n' r' R → ¬ MatchingR r' pre))


inductive Tokens : List Rule → List Token →  List Char → List Char → Prop where
  | NIL :
    Tokens R [] xs xs
  | CONS :
    z = pre ++ s →
    FirstToken R tok pre z →
    Tokens R  toks u s →
    Tokens R (tok :: toks) u (z ++ ps)
