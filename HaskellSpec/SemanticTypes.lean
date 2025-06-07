import HaskellSpec.Names

/-!
# Semantic Types

Semantic types are defined in Fig. 4 of the paper.
-/

/--
```text
κ ∈ Kind → *
         | κ → κ
```
-/
inductive KindS : Type where
  | Star : KindS
  | Fun : KindS → KindS → KindS

/--
```text
C ∈ Original class name → B
                        | M!B
```
-/
inductive Original_Class_NameS : Type where
  | Unqualified : Class_Name → Original_Class_NameS
  | Qualified : Module_Name → Class_Name → Original_Class_NameS


/--
```text
Γ ∈ Class name → Cᵏ
```
-/
inductive Class_NameS : Type where
  | Mk : Original_Class_NameS → KindS → Class_NameS


/--
```text
T ∈ Original type name → S
                       | M!S
                       | Σ
```
-/
inductive Original_Type_NameS : Type where
  | Unqualified : Type_Name → Original_Type_NameS
  | Qualified : Module_Name → Type_Name → Original_Type_NameS
  | Special : Original_Type_NameS


/--
```text
χ ∈ Type constructor → Tᵏ
```
-/
inductive Type_ConstructorS : Type where
  | Mk : Original_Type_NameS → KindS → Type_ConstructorS

/--
```text
α ∈ Type variable → uᵏ
```
-/
inductive Type_VariableS : Type where
  | Mk : Type_Variable → KindS → Type_VariableS

/--
```text
τ ∈ Type → α
         | χ
         | τ₁ τ₂
```
-/
inductive TypeS : Type where
  | Variable : Type_VariableS → TypeS
  | TypeConstructor : Type_ConstructorS → TypeS
  | App : TypeS → TypeS → TypeS


/--
```text
θ ∈ Context → (Γ₁ τ₁, … , Γₙ τₙ)
```
-/
inductive ContextS : Type where
  | Mk : List (Class_NameS × TypeS) → ContextS

/--
```text
σ ∈ Type Scheme →  ∀ α₁ … αₖ.θ ⇒ τ
```
-/
inductive TypeSchemeS : Type where
  | Forall : List Type_VariableS → ContextS → TypeS → TypeSchemeS
