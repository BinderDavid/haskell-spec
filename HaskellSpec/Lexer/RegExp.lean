/-- Regular Expressions -/
inductive RE : Type where
    /-- The regular expression corresponding to the empty language: `L(Empty) = ∅` -/
  | Empty
    /-- The regular expression matching an empty string: `L(Epsilon) = {""} ` -/
  | Epsilon
    /-- Kleene star: Zero or more repetitions -/
  | Star : RE → RE
    /-- One or more repetitions -/
  | Plus : RE → RE
    /-- Choice between two alternatives: `L(Union(re₁, re₂) = L(re₁) ∪ L(re₂)` -/
  | Union : RE → RE → RE
    /-- Concatenation of two words: `L(App(re₁,re₂)) = { w₁w₂ | w₁ ∈ L(re₁), w₂ ∈ L(re₂)` }-/
  | App : RE → RE → RE
    /-- The regular expression matching the corresponding symbol: `L(Symbol('a')) = { "a" }` -/
  | Symbol : Char → RE


inductive Matching : RE → List Char → Prop where
  | EPSILON :
    Matching RE.Epsilon []
  | PLUS :
    Matching re w₁ →
    Matching (RE.Star re) w₂ →
    Matching (RE.Plus re) (w₁ ++ w₂)
  | STAR_0 :
    Matching (RE.Star re) []
  | STAR_N :
    Matching re w₁ →
    Matching (RE.Star re) w₂ →
    Matching (RE.Star re) (w₁ ++ w₂)
  | UNION_L :
    Matching re₁ w →
    Matching (RE.Union re₁ re₂) w
  | UNION_R :
    Matching re₂ w →
    Matching (RE.Union re₁ re₂) w
  | APP :
    Matching re₁ w₁ →
    Matching re₂ w₂ →
    Matching (RE.App re₁ re₂) (w₁ ++ w₂)
  | SYMBOL :
    Matching (RE.Symbol c) [c]
