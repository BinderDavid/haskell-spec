/-!
# Semantic Types

Semantic types are defined in Fig. 4 of the paper.
-/

/--
```text
σ ∈ Type Scheme →  ∀ α₁ … αₖ.θ ⇒ τ
```
-/
inductive TypeSchemeS : Type where
/--
```text
θ ∈ Context → (Γ₁ τ₁, … , Γₙ τₙ)
```
-/
inductive ContextS : Type where

/--
```text
τ ∈ Type → α
         | χ
         | τ₁ τ₂
```
-/
inductive TypeS : Type where

/--
```text
χ ∈ Type constructor → Tᵏ
```
-/
inductive Type_ConstructorS : Type where

/--
```text
α ∈ Type variable → uᵏ
```
-/
inductive Type_VariableS : Type where

/--
```text
Γ ∈ Class name → Cᵏ
```
-/
inductive Class_NameS : Type where

/--
```text
T ∈ Original type name → S
                       | M!S
                       | Σ
```
-/
inductive Original_Type_NameS : Type where

/--
```text
C ∈ Original class name → B
                        | M!B
```
-/
inductive Original_Class_NameS : Type where

/--
```text
κ ∈ Kind → *
         | κ → κ
```
-/
inductive KindS : Type where
