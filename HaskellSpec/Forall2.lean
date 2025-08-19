import HaskellSpec.NonEmpty

inductive Forall2 : List α → List β → (α → β → Prop) → Prop where
  | Nil : Forall2 [] [] P
  | Cons :
     P x y →
     Forall2 xs ys P →
     Forall2 (x :: xs) (y :: ys) P

inductive Forall2NE : NonEmpty α → NonEmpty β → (α → β → Prop) → Prop where
  | Mk : P x y →
         Forall2 xs ys P  →
         Forall2NE { head := x, tail := xs } { head := y, tail := ys } P


inductive Forall3 : List α → List β → List γ →  (α → β → γ → Prop) → Prop where
  | Nil : Forall3 [] [] [] P
  | Cons :
     P x y z →
     Forall3 xs ys zs P →
     Forall3 (x :: xs) (y :: ys) (z :: zs) P

inductive Forall3NE : NonEmpty α → NonEmpty β → NonEmpty γ → (α → β → γ → Prop) → Prop where
  | Mk : P x y z →
         Forall3 xs ys zs P  →
         Forall3NE { head := x, tail := xs } { head := y, tail := ys } { head := z, tail := zs} P

inductive Forall4 : List α → List β → List γ → List δ →  (α → β → γ → δ → Prop) → Prop where
  | Nil : Forall4 [] [] [] [] P
  | Cons :
     P x y z v →
     Forall4 xs ys zs vs P →
     Forall4 (x :: xs) (y :: ys) (z :: zs) (v :: vs) P

inductive Forall4NE : NonEmpty α → NonEmpty β → NonEmpty γ → NonEmpty δ → (α → β → γ → δ → Prop) → Prop where
  | Mk : P x y z v →
         Forall4 xs ys zs vs P  →
         Forall4NE { head := x, tail := xs } { head := y, tail := ys } { head := z, tail := zs} { head := v , tail := vs } P
