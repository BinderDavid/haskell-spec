import HaskellSpec.SemanticTypes
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
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
  | KIND_TVAR :
      (KE_Name.u u, κ) ∈ ke
    → ---------------------------
      ktype ke (TypeExpression.var u) κ

  | KIND_TCON :
      (KE_Name.T T, κ) ∈ ke
    → ---------------------------
      ktype ke (TypeExpression.typename T) κ

  | KIND_APP :
      ktype ke t₁ (Fun κ₁ κ₂)
    → ktype ke t₂ κ₁
    → --------------------------
      ktype ke (TypeExpression.app t₁ t₂) κ₂

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
KE ⊢ ctDecls : KE
```
-/
inductive kctDecls : KE
                   → Source.ClassesAndTypes
                   → KE
                   → Prop where
  | KCTDECLS :
    kgroup _ _ _ →
    kctDecls _ _ _ →
    kctDecls ke (Source.ClassesAndTypes.decls grp rest) _
  | KCTEMPTY :
    kctDecls ke Source.ClassesAndTypes.empty []



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
  | KIND_DATA :
    kctDecl ke (Source.ClassOrType.ct_data _ _ _ _) _
  | KIND_TYPE :
    kctDecl ke (Source.ClassOrType.ct_type _ _ _) _
  | KIND_CLASS :
    kctDecl ke (Source.ClassOrType.ct_class _ _ _ _ _) _

/--
Cp. fig 9
```text
KE ⊢ conDecl
```
-/
inductive kconDecl : KE
                   → Source.ConstructorDecl
                   → Prop where
  | KIND_POSCON :
    (∀ τ, τ ∈ tys → ktype ke τ SemTy.Kind.Star) →
    kconDecl ke (ConstructorDecl.conDecl_simple c tys)
  | KIND_LABCON :
    (∀ l τ, (l,τ) ∈ lbls → ktype ke τ SemTy.Kind.Star) →
    kconDecl ke (ConstructorDecl.conDecl_record c lbls)

/--
Cp. fig 9
```text
KE ⊢ sigs
```
-/
inductive ksigs : KE
                → List Source.Signature
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
inductive kctx : KE → Source.Context → Prop where

  | Kind_Ctx_nil :
      -----------------------
      kctx ke (Context.cx [])

  | Kind_Ctx_cons :
        (KE_Name.C (classAssertionName CA), κ) ∈ ke
      → ktype ke (classAssertionType CA) κ
      → kctx ke (Context.cx CAS)
      → ---------------------------------------------
        kctx ke (Context.cx (CA :: CAS))

end Kinding
