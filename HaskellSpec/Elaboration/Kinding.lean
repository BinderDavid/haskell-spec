import HaskellSpec.SemanticTypes
import HaskellSpec.Source.SourceLang
import HaskellSpec.Environments

/-!
# Kind System

The rules are defined in fig. 8, 9, 10 of the paper.
-/

namespace Kinding

inductive ktype : Environment.KE → Source.TypeExpression → SemanticTypes.Kind → Prop where

inductive KindOrdering : SemanticTypes.Kind → SemanticTypes.Kind → Prop where

inductive kctDecls : Environment.KE → Source.ClassesAndTypes → Environment.KE → Prop where

inductive kgroup : Environment.KE → List Source.ClassOrType → Environment.KE → Prop where

inductive kctDecl : Environment.KE → Source.ClassOrType → Environment.KE → Prop where

inductive kconDecl : Environment.KE → Source.ConstructorDecl → Prop where

inductive ksigs : Environment.KE → Source.Signatures → Prop where

inductive ksig : Environment.KE → Source.Signature → Prop where


open Environment Source ClassAssertion Context

/-!
```text
{C₁ : κ₁, ..., Cₙ : κₙ} ⊆ KE
i ∈ [1, n] : KE ⊢^ktype tᵢ : κᵢ
------------------------------ KIND CTX
KE ⊢^kctx C₁ t₁, ... Cₙ tₙ
```
-/
inductive kctx : Environment.KE → Source.Context → Prop where
  | KIND_CTX {CAS KE} :
      (∀ CA K,
        CA ∈ CAS →
        (KE_Name.C (classAssertionName CA), K) ∈ KE →
        ktype KE (classAssertionType CA) K
      )
      → ---------------------------------------------
      kctx KE (Context.cx CAS)


end Kinding
