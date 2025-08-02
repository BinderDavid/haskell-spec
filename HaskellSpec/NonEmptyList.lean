structure NonEmptyList (α : Type) where
  Mk ::
  head : α
  tail : List α

def fromList : (List α) -> Option (NonEmptyList α)
  | List.nil => Option.none
  | List.cons a as => Option.some (NonEmptyList.Mk a as)
