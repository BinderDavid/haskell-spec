import HaskellSpec.Names
import HaskellSpec.Source.Literals

namespace Source

/--
```text
  p ∈ Pattern       → x
                    | K p₁ … pₙ      k ≥ 0
                    | K {fp₁ … fpₙ}  k ≥ 0
                    | v@p
                    | ~p
                    | _
                    | literal
  fp ∈ FieldPattern → x = p
```
NOTE: n+k patterns have been removed from the formalization.
--/
inductive Pattern : Type where
  | var : QVariable → Pattern
  | constr_pat : QConstructor → List Pattern → Pattern
  | constr_fieldPat : QConstructor → List (Variable × Pattern) → Pattern
  | at : Variable → Pattern → Pattern
  | lazy : Pattern → Pattern
  | wildcard : Pattern
  | lit : Literal → Pattern

end Source
