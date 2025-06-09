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
-/
inductive ktype : KindEnv → TypeExpression → Kind → Prop where
  | Kind_TVar :
      (KindEnv_Name.u u, κ) ∈ KE
    → ---------------------------
      ktype KE (type_var u) κ

  | Kind_TCon :
      (KindEnv_Name.T T, κ) ∈ KE
    → ---------------------------
      ktype KE (type_var u) κ

  | Kind_App :
      ktype KE t₁ (Fun κ₁ κ₂)
    → ktype KE t₂ κ₁
    → --------------------------
      ktype KE (type_cons t₁ t₂) κ₂

/--
Defined in section 3.1.1
-/
inductive KindOrdering : SemTy.Kind → SemTy.Kind → Prop where
  | Star_LT : KindOrdering SemTy.Kind.Star κ
  | Fun_Cong : KindOrdering κ₁ κ₁'
             → KindOrdering κ₂ κ₂'
             → KindOrdering (SemTy.Kind.Fun κ₁ κ₂) (SemTy.Kind.Fun κ₁' κ₂')

inductive kctDecls : KindEnv → Source.ClassesAndTypes → KindEnv → Prop where

inductive kgroup : KindEnv → List Source.ClassOrType → KindEnv → Prop where

inductive kctDecl : KindEnv → Source.ClassOrType → KindEnv → Prop where

inductive kconDecl : KindEnv → Source.ConstructorDecl → Prop where

inductive ksigs : KindEnv → Source.Signatures → Prop where

inductive ksig : KindEnv → Source.Signature → Prop where


open  Source ClassAssertion Context

/-!
```text
{C₁ : κ₁, ..., Cₙ : κₙ} ⊆ KE
i ∈ [1, n] : KE ⊢^ktype tᵢ : κᵢ
------------------------------ KIND CTX
KE ⊢^kctx C₁ t₁, ... Cₙ tₙ
```
-/
inductive kctx : KindEnv → Source.Context → Prop where
  | KIND_CTX {CAS KE} :
      (∀ CA K,
        CA ∈ CAS →
        (KindEnv_Name.C (classAssertionName CA), K) ∈ KE →
        ktype KE (classAssertionType CA) K
      )
      → ---------------------------------------------
      kctx KE (Context.cx CAS)


end Kinding
