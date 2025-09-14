import HaskellSpec.Names

/-!
# Semantic Types

Semantic types are defined in Fig. 4 of the paper.
-/
namespace SemTy

/--
```text
κ ∈ Kind → *
         | κ → κ
```
-/
inductive Kind : Type where
  | Star : Kind
  | Fun : Kind → Kind → Kind
  deriving BEq

deriving instance BEq for Kind

export Kind (Star Fun)



/--
```text
Γ ∈ Class name → Cᵏ
```
-/
structure SClass_Name : Type where
  name : OClass_Name
  kind : Kind
  deriving BEq


/--
```text
χ ∈ Type constructor → Tᵏ
```
-/
inductive Type_Constructor : Type where
  | Mk : OType_Name → Kind → Type_Constructor
  deriving BEq

/--
```text
α ∈ Type variable → uᵏ
```
-/
inductive Type_Variable : Type where
  | Mk : Type_Variable → Kind → Type_Variable

deriving instance BEq for Type_Variable

/--
```text
τ ∈ Type → α
         | χ
         | τ₁ τ₂
```
-/
inductive TypeS : Type where
  | Variable : Type_Variable → TypeS
  | TypeConstructor : Type_Constructor → TypeS
  | App : TypeS → TypeS → TypeS
  deriving BEq

/--
Make a type list of α
-/
def mk_list (α: TypeS): TypeS :=
 SemTy.TypeS.App
  (SemTy.TypeS.TypeConstructor
    (SemTy.Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.List) SemTy.Kind.Star))
  α


/--
```text
θ ∈ Context → (Γ₁ τ₁, … , Γₙ τₙ)
```
-/

abbrev Context := List (SClass_Name × TypeS)

/--
```text
σ ∈ Type Scheme →  ∀ α₁ … αₖ.θ ⇒ τ
```
-/
inductive TypeScheme : Type where
  | Forall : List Type_Variable → Context → TypeS → TypeScheme
  deriving BEq

/--
This is written as follows in the paper:
```
∀ α. Γ α ⇒c σ
```
It is used to assign types to typeclass methods. The example given in the paper is:
```
ceiling : ∀ α. RealFrac α ⇒c ∀ β. Integral β ⇒ α → β
```
-/
inductive ClassTypeScheme : Type where
  | Forall : Type_Variable → SClass_Name → TypeScheme → ClassTypeScheme
  deriving BEq



def type_subst (τ : SemTy.TypeS)
               (args : List SemTy.TypeS)
               (vars : List SemTy.Type_Variable)
               : SemTy.TypeS := sorry

end SemTy
