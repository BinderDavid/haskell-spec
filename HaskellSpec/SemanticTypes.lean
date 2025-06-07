import HaskellSpec.Names

/-!
# Semantic Types

Semantic types are defined in Fig. 4 of the paper.
-/
namespace SemanticTypes

/--
```text
κ ∈ Kind → *
         | κ → κ
```
-/
inductive Kind : Type where
  | Star : Kind
  | Fun : Kind → Kind → Kind

/--
```text
C ∈ Original class name → B
                        | M!B
```
-/
inductive Original_Class_Name : Type where
  | Unqualified : Class_Name → Original_Class_Name
  | Qualified : Module_Name → Class_Name → Original_Class_Name


/--
```text
Γ ∈ Class name → Cᵏ
```
-/
inductive Class_Name : Type where
  | Mk : Original_Class_Name → KindS → Class_Name


/--
```text
T ∈ Original type name → S
                       | M!S
                       | Σ
```
-/
inductive Original_Type_Name : Type where
  | Unqualified : Type_Name → Original_Type_Name
  | Qualified : Module_Name → Type_Name → Original_Type_Name
  | Special : Special_Type_Constructor → Original_Type_Name


/--
```text
χ ∈ Type constructor → Tᵏ
```
-/
inductive Type_Constructor : Type where
  | Mk : Original_Type_Name → Kind → Type_Constructor

/--
```text
α ∈ Type variable → uᵏ
```
-/
inductive Type_Variable : Type where
  | Mk : Type_Variable → Kind → Type_Variable

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


/--
```text
θ ∈ Context → (Γ₁ τ₁, … , Γₙ τₙ)
```
-/
inductive Context : Type where
  | Mk : List (Class_Name × TypeS) → Context

/--
```text
σ ∈ Type Scheme →  ∀ α₁ … αₖ.θ ⇒ τ
```
-/
inductive TypeScheme : Type where
  | Forall : List Type_Variable → Context → TypeS → TypeScheme

end SemanticTypes
