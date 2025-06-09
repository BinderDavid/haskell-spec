import HaskellSpec.SemanticTypes
import HaskellSpec.Source.SourceLang
import HaskellSpec.Environments

/-!
# Kind System

The rules are defined in fig. 8, 9, 10 of the paper.
-/

namespace Kinding

open Source Env SemTy


/-
Fig. 10 (Kind inference, type expressions)
```text
KE ⊢ t : κ
```
-/
inductive ktype : KE → TypeExpression → Kind → Prop where
  | Kind_TVar :
      (KindEnv_Name.u u, κ) ∈ ke
    → ---------------------------
      ktype ke (type_var u) κ

  | Kind_TCon :
      (KindEnv_Name.T T, κ) ∈ ke
    → ---------------------------
      ktype ke (type_var u) κ

  | Kind_App :
      ktype ke t₁ (Fun κ₁ κ₂)
    → ktype ke t₂ κ₁
    → --------------------------
      ktype ke (type_cons t₁ t₂) κ₂

/--
Defined in section 3.1.1
-/
inductive KindOrdering : SemTy.Kind → SemTy.Kind → Prop where
  | Star_LT : KindOrdering SemTy.Kind.Star κ
  | Fun_Cong : KindOrdering κ₁ κ₁'
             → KindOrdering κ₂ κ₂'
             → KindOrdering (SemTy.Kind.Fun κ₁ κ₂) (SemTy.Kind.Fun κ₁' κ₂')

/--
Cp. fig 8
```text
KE ⊢ ctDecls : KE
```
-/
inductive kctDecls : KE
                   → Source.ClassesAndTypes
                   → KE
                   → Prop where

/--
Cp. fig 8
```text
KE ⊢ ctDecl₁; … ; ctDeclₙ : KE
```
-/
inductive kgroup : KE
                 → List Source.ClassOrType
                 → KE
                 → Prop where

/--
Cp. fig 8
```text
KE ⊢ ctDecl : KE
```
-/
inductive kctDecl : KE
                  → Source.ClassOrType
                  → KE
                  → Prop where

/--
Cp. fig 9
```text
KE ⊢ conDecl
```
-/
inductive kconDecl : KE
                   → Source.ConstructorDecl
                   → Prop where

/--
Cp. fig 9
```text
KE ⊢ sigs
```
-/
inductive ksigs : KE
                → Source.Signatures
                → Prop where

/--
Cp. fig 9
```text
KE ⊢ sig
```
-/
inductive ksig : KE
               → Source.Signature
               → Prop where


open  Source ClassAssertion Context

/-!
```text
{C₁ : κ₁, ..., Cₙ : κₙ} ⊆ KE
i ∈ [1, n] : KE ⊢^ktype tᵢ : κᵢ
------------------------------ KIND CTX
KE ⊢^kctx C₁ t₁, ... Cₙ tₙ
```
-/
inductive kctx : KE
               → Source.Context
               → Prop where
  | KIND_CTX {CAS ke} :
      (∀ CA K,
        CA ∈ CAS →
        (KindEnv_Name.C (classAssertionName CA), K) ∈ ke →
        ktype ke (classAssertionType CA) K
      )
      → ---------------------------------------------
      kctx ke (Context.cx CAS)


end Kinding
