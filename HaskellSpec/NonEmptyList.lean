structure NonEmptyList (α : Type) where
  Mk ::
  head : α
  tail : List α

def singleton (a : α) : NonEmptyList α := NonEmptyList.Mk a []

def fromList : (List α) -> Option (NonEmptyList α)
  | List.nil => Option.none
  | List.cons a as => Option.some (NonEmptyList.Mk a as)
