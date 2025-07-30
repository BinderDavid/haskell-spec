import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Rules

/-- Returns `true` if the regular expression matches the empty string -/
def nullable (re : RE) : Bool :=
  match re with
  | RE.Symbol _ => false
  | RE.Empty => false
  | RE.Epsilon => true
  | RE.Union re₁ re₂ => or (nullable re₁) (nullable re₂)
  | RE.App re₁ re₂ => and (nullable re₁) (nullable re₂)
  | RE.Star _ => true
  | RE.Plus re => nullable re

theorem nullable_correct (re : RE) :
  nullable re = true ↔ Matching re [] := by
  apply Iff.intro
  case mp =>
    intros H
    induction re with
    | Symbol _ => cases H
    | Empty => cases H
    | Epsilon => apply Matching.EPSILON
    | Union re₁ re₂ IH₁ IH₂ =>
      have H_or : nullable re₁ = true ∨ nullable re₂ = true := by
        apply Bool.or_eq_true_iff.mp
        exact H
      apply Or.elim H_or
      case left =>
        intro H₁
        apply Matching.UNION_L
        exact IH₁ H₁
      case right =>
        intro H₂
        apply Matching.UNION_R
        exact IH₂ H₂
    | App re₁ re₂ IH₁ IH₂ =>
      have H_and : nullable re₁ ∧ nullable re₂ := by
        apply Bool.and_eq_true_iff.mp
        exact H
      specialize IH₁ H_and.left
      specialize IH₂ H_and.right
      show Matching (RE.App re₁ re₂) ([] ++ [])
      apply Matching.APP
      . exact IH₁
      . exact IH₂
    | Star => apply Matching.STAR_0
    | Plus re IH =>
      have H_nullable : nullable re = true := H
      specialize IH H_nullable
      show Matching (RE.Plus re) ([] ++ [])
      apply Matching.PLUS
      . exact IH
      . apply Matching.STAR_0
  case mpr =>
    intros H
    induction re with
    | Symbol _ => cases H
    | Empty => cases H
    | Epsilon => rfl
    | Union re₁ re₂ IH₁ IH₂ =>
      apply Bool.or_eq_true_iff.mpr
      cases H with
      | UNION_L H_matching =>
        specialize IH₁ H_matching
        exact Or.inl IH₁
      | UNION_R H_matching =>
        specialize IH₂ H_matching
        exact Or.inr IH₂
    | App re₁ re₂ IH₁ IH₂ =>
      apply Bool.and_eq_true_iff.mpr
      generalize H_eq : [] = x at H
      cases H with
      | APP w₁ w₂ H₁ H₂ =>
        have H_w : w₁ = [] ∧ w₂ = [] := by
          apply List.eq_nil_of_append_eq_nil
          symm
          assumption
        rw [H_w.left] at H₁
        rw [H_w.right] at H₂
        exact ⟨IH₁ H₁, IH₂ H₂⟩
    | Star => rfl
    | Plus re IH =>
      generalize H_eq : [] = x at H
      cases H with
      | PLUS w₁ w₂ H₁ H₂ =>
        have H_w : w₁ = [] ∧ w₂ = [] := by
          apply List.eq_nil_of_append_eq_nil
          symm
          assumption
        rw [H_w.left] at H₁
        apply IH
        exact H₁



/--
Computes the Brzozowski derivative of the regular expression `re` with
respect to the character `a`.
-/
def derivative (a : Char) (re : RE) : RE :=
  match re with
  | RE.Empty => RE.Empty
  | RE.Epsilon => RE.Empty
  | RE.Symbol c => if c == a then RE.Epsilon else RE.Empty
  | RE.Union re₁ re₂ => RE.Union (derivative a re₁) (derivative a re₂)
  | RE.Star re => RE.App (derivative a re) (RE.Star re)
  | RE.Plus re => RE.App (derivative a re) (RE.Star re)
  | RE.App re₁ re₂ => RE.Union (RE.App (derivative a re₁) re₂)
                               (if nullable re₁ then (derivative a re₂) else RE.Empty)

theorem derivative_correct (re : RE) (x : Char) (xs : List Char) :
  Matching (derivative x re) xs ↔ Matching re (x :: xs) :=
  sorry

def derivative_rec (s : List Char) (re : RE) : RE :=
  match s with
  | [] => re
  | s :: ss => derivative_rec ss (derivative s re)

def matching (s : List Char) (re : RE) : Bool :=
  nullable (derivative_rec s re)

theorem matching_correct (re : RE) (xs : List Char) :
  Matching re xs ↔ matching xs re = true :=
  sorry

def maxpref_one_rec (best : Option (List Char × List Char))
        (left right : List Char)
        (re : RE)
        : Option (List Char × List Char) :=
  match right with
  | [] => if nullable re
          then some (left, right)
          else best
  | s :: right' => let re' := derivative s re
               let left' := left ++ [s]
               if nullable re'
               then maxpref_one_rec (some (left', right')) left' right' re'
               else maxpref_one_rec best left' right' re'

/--
Given a string and a rule, compute the longest prefix that matches this rule.
If the regular expression matches successfully, return the computed token,
the length of the consumed input, and the remainder of the output.
-/
def maxpref_one (s : List Char) (r : Rule) : Option (Token × Int × List Char) :=
  match maxpref_one_rec none [] s r.re with
  | none => none
  | some (pre, rest) => some (r.action (String.mk pre), pre.length, rest)


def max_pref_rec (best : Option (Token × Int × List Char))
                 (input : List Char)
                 (rules : List Rule)
                 : Option (Token × Int × List Char) :=
    match rules with
    | [] => best
    | (r :: rules) =>
       match maxpref_one input r with
       | none => max_pref_rec best input rules
       | some (tok, len, rest) =>
          match best with
          | none => max_pref_rec (some (tok, len, rest)) input rules
          | some best => if len > best.2.1
                         then max_pref_rec (some (tok, len, rest)) input rules
                         else max_pref_rec (some best) input rules


def max_pref (input : List Char) (rules : List Rule) : Option (Token × List Char) :=
    match max_pref_rec none input rules with
    | none => none
    | some (tok, _, rest) => (tok, rest)


/- This function is actually total, since `len(rest) < len(input)`! -/
partial def lex (input : List Char) (rules : List Rule) : List Token × List Char :=
  match max_pref input rules with
  | none => ([], input)
  | some (tok, rest) =>
    let (toks, rest') := lex rest rules
    (tok :: toks, rest')
