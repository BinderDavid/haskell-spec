structure NonEmpty (α : Type) where
  head : α
  tail : List α
  deriving Repr

instance instMembershipNonEmpty : Membership α (NonEmpty α) where
  mem ys x := (x = ys.head) ∨ x ∈ ys.tail

def toList {α : Type}(xs : NonEmpty α) : List α :=
  xs.head :: xs.tail

def singleton (a : α) : NonEmpty α := NonEmpty.mk a []

def fromList : (List α) -> Option (NonEmpty α)
  | List.nil => Option.none
  | List.cons a as => Option.some (NonEmpty.mk a as)

def concat {α : Type} (xs ys : NonEmpty α) : NonEmpty α :=
  NonEmpty.mk xs.head (xs.tail ++ toList ys)

def foldl {α : Type} (f : α → α → α) (xs : NonEmpty α) : α :=
  List.foldl f xs.head xs.tail
