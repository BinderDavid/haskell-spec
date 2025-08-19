structure NonEmpty (α : Type) where
  head : α
  tail : List α

def singleton (a : α) : NonEmpty α := NonEmpty.mk a []

def fromList : (List α) -> Option (NonEmpty α)
  | List.nil => Option.none
  | List.cons a as => Option.some (NonEmpty.mk a as)

def concat {α : Type} (xs ys : NonEmpty α) : NonEmpty α :=
  sorry
