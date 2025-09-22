import HaskellSpec.SemanticTypes
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Environments
import HaskellSpec.Forall2

/-!
# Kind System

The rules are defined in fig. 8, 9, 10 of the paper.
-/

namespace Kinding

set_option quotPrecheck false in
set_option hygiene false in
notation  "《kindord》" κ₁ "≼" κ₂ "▪" => KindOrdering κ₁ κ₂

/--
Defined in section 3.1.1
-/
inductive KindOrdering : SemTy.Kind
                       → SemTy.Kind
                       → Prop where
  | STAR_LT :
    -------------------------------
    《kindord》SemTy.Kind.Star ≼ κ ▪

  | FUN_CONG :
    《kindord》κ₁ ≼ κ₁' ▪ →
    《kindord》κ₂ ≼ κ₂' ▪ →
    ------------------------------------------------------------
    《kindord》SemTy.Kind.Fun κ₁ κ₂ ≼ SemTy.Kind.Fun κ₁' κ₂' ▪


def KindEnvOrdering (ke₁ ke₂ : Env.KE) : Prop :=
  ∀ x ∈ ke₁, ∃ x' ∈ ke₂, x.fst = x'.fst ∧ 《kindord》 x.snd ≼ x'.snd ▪

notation  "《kindenvord》" ke₁ "≼" ke₂ "▪" => KindEnvOrdering ke₁ ke₂

/--
If `MinKindEnv f ke_min` holds, then `ke_min` is the smallest kind environment satisfying `f`.
This is used to implement kind defaulting.
-/
def MinKindEnv (f : Env.KE → Prop) (ke : Env.KE) : Prop :=
  f ke
  ∧
  (∀ ke', f ke → 《kindenvord》 ke ≼ ke' ▪)


set_option quotPrecheck false in
set_option hygiene false in
notation  "《ktype》" ke "⊢" t "፥" κ "▪"=> ktype ke t κ

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
    ----------------------------------------------
    《ktype》ke ⊢ Source.TypeExpression.var u ፥ κ ▪

  | KIND_TCON :
    (Env.KE_Name.T T, κ) ∈ ke →
    ----------------------------------------------
    《ktype》ke ⊢ Source.TypeExpression.typename T ፥ κ ▪

  | KIND_APP :
    《ktype》ke ⊢ t₁ ፥ SemTy.Fun κ₁ κ₂ ▪ →
    《ktype》ke ⊢ t₂ ፥ κ₁ ▪ →
    ---------------------------------------------
    《ktype》ke ⊢ Source.TypeExpression.app t₁ t₂ ፥ κ₂ ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《kclassassertion》" ke "⊢" ca "▪"=> kclassassertion ke ca

inductive kclassassertion : Env.KE
                          → Source.ClassAssertion
                          → Prop where
  | KIND_CLASS_ASSERTION :
    (Env.KE_Name.C ca.name, κ) ∈ ke →
    《ktype》ke ⊢ Source.classAssertionType ca ፥ κ ▪ →
    --------------------------------------------------
    kclassassertion ke ca

set_option quotPrecheck false in
set_option hygiene false in
notation  "《ksig》" ke "⊢" sig "▪"=> ksig ke sig

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
     /- ke' = {u₁ : κ₁,…, uₙ : κₙ } → -/
     (∀ ca ∈ cx, 《kclassassertion》  _ /- (Env.oplus ke ke') -/ ⊢ ca ▪ ) →
    《ktype》 _ /- (Env.oplus ke ke') -/ ⊢ t ፥ SemTy.Kind.Star ▪ →
    -----------------------------------------------------
    《ksig》 ke ⊢ Source.Signature.mk v cx t ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《kconDecl》" ke "⊢" condecl "▪"=> kconDecl ke condecl

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
    (∀ τ, τ ∈ tys → 《ktype》 ke ⊢ τ ፥ SemTy.Kind.Star ▪) →
    -------------------------------------------------------
    《kconDecl》 ke ⊢ Source.ConstructorDecl.poscon c tys ▪

  | KIND_LABCON :
    (∀ l τ, (l,τ) ∈ lbls → 《ktype》ke ⊢ τ ፥ SemTy.Kind.Star ▪) →
    --------------------------------------------------------
    《kconDecl》 ke ⊢ Source.ConstructorDecl.labcon c lbls ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《kctDecl》" ke "⊢" kctdecl "፥" ke' "▪"=> kctDecl ke kctdecl ke'

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
    (∀ ca ∈ cx, 《kclassassertion》 _ ⊢ ca ▪ )→
    (∀ conDecl ∈  cons,《kconDecl》 _ ⊢ conDecl ▪) →
    -----------------------------------------------------------
    《kctDecl》 ke ⊢ (Source.ClassOrType.ct_data cx S us cons) ፥ [⟨Env.KE_Name.T _,κ⟩] ▪

  | KIND_TYPE :
    《ktype》 _ ⊢ t ፥ κ ▪ →
    ------------------------------------------------------
    《kctDecl》ke ⊢ Source.ClassOrType.ct_type S us t ፥ [⟨Env.KE_Name.T _,_⟩] ▪

  | KIND_CLASS :
    (∀ ca ∈ cx, 《kclassassertion》_ ⊢ ca ▪ )→
    (∀ sig ∈ sigs, 《ksig》_ ⊢ sig ▪ ) →
    -----------------------------------------------------------
    《kctDecl》ke ⊢ Source.ClassOrType.ct_class cx B u sigs b ፥ [⟨Env.KE_Name.C _,κ⟩] ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《kgroup》" ke "⊢" classortypes "፥" ke' "▪"=> kgroup ke classortypes ke'
/--
Cp. fig 8
```text
KE ⊢ ctDecl₁; … ; ctDeclₙ : KE
```
-/
inductive kgroup : Env.KE
                 → NonEmpty Source.ClassOrType
                 → Env.KE
                 → Prop where
  | KGROUP :
    Forall2NE class_or_types kes (λ ctDecl keᵢ => 《kctDecl》 ke ⊢ ctDecl ፥ keᵢ ▪) →
    -----------------------------------
    《kgroup》ke ⊢ class_or_types ፥ _ ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《kctDecls》" ke "⊢" classandtypes "፥" ke' "▪"=> kctDecls ke classandtypes ke'

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
    MinKindEnv (λ ke' => 《kgroup》 _ /-Env.oplus ke ke' -/ ⊢ grp ፥ ke' ▪) ke_decls →
    《kctDecls》 _ /-Env.oplus ke ke_decls -/ ⊢ rest ፥ ke_groups ▪ →
    -----------------------------------------------------
    《kctDecls》ke ⊢ Source.ClassesAndTypes.decls grp rest ፥ _ /- ke_decls ⊕ ke_groups -/▪

  | KCTEMPTY :
    -------------------------------------------
    《kctDecls》 ke ⊢ Source.ClassesAndTypes.empty ፥ [] ▪

end Kinding
