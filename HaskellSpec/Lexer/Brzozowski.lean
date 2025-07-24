import HaskellSpec.Lexer.RegExp

/-- Returns `true` if the regular expression matches the  empty string -/
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
  nullable re = true ↔ Matching re [] :=
  sorry

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

def maxpref_one (s : List Char) (re : RE) : Option (List Char × List Char) :=
  maxpref_one_rec none [] s re
