import HaskellSpec.SemanticTypes
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Environments

/-!
# Kind System

The rules are defined in fig. 8, 9, 10 of the paper.
-/

namespace Kinding

/--
Defined in section 3.1.1
-/
inductive KindOrdering : SemTy.Kind
                       → SemTy.Kind
                       → Prop where
  | STAR_LT :
    -------------------------------
    KindOrdering SemTy.Kind.Star κ

  | FUN_CONG :
    KindOrdering κ₁ κ₁' →
    KindOrdering κ₂ κ₂' →
    ------------------------------------------------------------
    KindOrdering (SemTy.Kind.Fun κ₁ κ₂) (SemTy.Kind.Fun κ₁' κ₂')

def KindEnvOrdering (ke1 ke2 : Env.KE) : Prop := sorry


/--
Fig. 10 (Kind inference, type expressions)
```text
KE ⊢ t : κ
```
-/
inductive ktype : Env.KE
                → TypeExpression
                → Kind
                → Prop where
  | KIND_TVAR :
    (Env.KE_Name.u u, κ) ∈ ke →
    ----------------------------------------
    ktype ke (Source.TypeExpression.var u) κ

  | KIND_TCON :
    (Env.KE_Name.T T, κ) ∈ ke →
    ----------------------------------------------
    ktype ke (Source.TypeExpression.typename T) κ

  | KIND_APP :
    ktype ke t₁ (SemTy.Fun κ₁ κ₂) →
    ktype ke t₂ κ₁ →
    ---------------------------------------------
    ktype ke (Source.TypeExpression.app t₁ t₂) κ₂

/--
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
  | KIND_CTX_NIL :
    ------------------------------
    kctx ke (Source.Context.cx [])

  | KIND_CTX_CONS :
    (Env.KE_Name.C (Source.classAssertionName CA), κ) ∈ ke →
    ktype ke (Source.classAssertionType CA) κ →
    kctx ke (Source.Context.cx CAS) →
    ---------------------------------------------------------
    kctx ke (Source.Context.cx (CA :: CAS))


/--
Cp. fig 9
```text
KE ⊢ sig
```
-/
inductive ksig : Env.KE
               → Source.Signature
               → Prop where
  | KIND_SIG :
    kctx _ /- (Env.oplus ke ke') -/ cx →
    ktype _ /- (Env.oplus ke ke') -/ t SemTy.Kind.Star →
    -----------------------------------------------------
    ksig ke (Source.Signature.sig v cx t)

/--
Cp. fig 9
```text
KE ⊢ sigs
```
-/
inductive ksigs : Env.KE
                → List Source.Signature
                → Prop where
  | KIND_SIGS_NIL :
    ---------------
    ksigs ke []

  | KIND_SIGS_CONS :
    ksig ke sig →
    ksigs ke sigs →
    ----------------------
    ksigs ke (sig :: sigs)

/--
Cp. fig 9
```text
KE ⊢ conDecl
```
-/
inductive kconDecl : Env.KE
                   → Source.ConstructorDecl
                   → Prop where
  | KIND_POSCON :
    (∀ τ, τ ∈ tys → ktype ke τ SemTy.Kind.Star) →
    -------------------------------------------------
    kconDecl ke (Source.ConstructorDecl.poscon c tys)

  | KIND_LABCON :
    (∀ l τ, (l,τ) ∈ lbls → ktype ke τ SemTy.Kind.Star) →
    ----------------------------------------------------
    kconDecl ke (Source.ConstructorDecl.labcon c lbls)

/--
Cp. fig 8
```text
KE ⊢ ctDecl : KE
```
-/
inductive kctDecl : Env.KE
                  → Source.ClassOrType
                  → Env.KE
                  → Prop where
  | KIND_DATA :
    kctx _ _ →
    kconDecl _ _ →
    --------------------------------------------------
    kctDecl ke (Source.ClassOrType.ct_data _ _ _ _) _

  | KIND_TYPE :
    ktype _ _ _ →
    ------------------------------------------------
    kctDecl ke (Source.ClassOrType.ct_type _ _ _) _

  | KIND_CLASS :
    kctx _ _ →
    ksigs _ _ →
    ----------------------------------------------------
    kctDecl ke (Source.ClassOrType.ct_class _ _ _ _ _) _


/--
Cp. fig 8
```text
KE ⊢ ctDecl₁; … ; ctDeclₙ : KE
```
-/
inductive kgroup : Env.KE
                 → List Source.ClassOrType
                 → Env.KE
                 → Prop where
  | KGROUP :
    kctDecl _ _ _ →
    ---------------
    kgroup _ _ _

/--
Cp. fig 8
```text
KE ⊢ ctDecls : KE
```
-/
inductive kctDecls : Env.KE
                   → Source.ClassesAndTypes
                   → Env.KE
                   → Prop where
  | KCTDECLS :
    kgroup _ _ _ →
    kctDecls _ _ _ →
    -----------------------------------------------------
    kctDecls ke (Source.ClassesAndTypes.decls grp rest) _

  | KCTEMPTY :
    -------------------------------------------
    kctDecls ke Source.ClassesAndTypes.empty []

end Kinding
