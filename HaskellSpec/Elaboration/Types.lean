import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes
import HaskellSpec.Forall2

/-- A typename `T` applied to a list of arguments:
```text
type_apply T [t₁,t₂,t₃] = T t₁ t₂ t₃
```
TODO: Reverse order!
-/
def type_apply (T : QType_Name)
               (ts : List Source.TypeExpression)
               : Source.TypeExpression :=
  match ts with
  | [] => Source.TypeExpression.typename T
  | (t :: ts) => Source.TypeExpression.app (type_apply T ts) t

set_option quotPrecheck false in
set_option hygiene false in
notation  "《type》" te "," h "⊢" t "፥"  τ "▪" => type te h t τ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《types》" te "," h "⊢" ts "፥"  τs "▪" => types te h ts τs

mutual
  /--
  Cp. Fig 18
  ```text
  TE, h ⊢ t : τ
  ```
  -/
  inductive type : Env.TE → Int
                 → Source.TypeExpression
                 → SemTy.TypeS
                 → Prop where
    | TVAR :
      ⟨u, α⟩ ∈ te₂ →
      ---------------------------------------------------------------------------
      《type》⟨te₁,te₂⟩, h ⊢ Source.TypeExpression.var u ፥ SemTy.TypeS.Variable α ▪

    | TCON :
      ⟨T, Env.TE_Item.DataType χ⟩ ∈ te₁ →
      --------------------------------------------------------------------------------------
      《type》⟨te₁,te₂⟩,h ⊢ Source.TypeExpression.typename T ፥ SemTy.TypeS.TypeConstructor χ ▪

    | TSYN :
      ⟨T, Env.TE_Item.TypeSynonym χ g αs τ⟩ ∈ te₁ →
      g < h →
      《types》te, h ⊢ ts ፥ τs ▪ →
      Env.rng subst = τs →
      Env.dom subst = αs →
      ---------------------------------------------------------------------------
      《type》⟨te₁,te₂⟩,h ⊢ type_apply T ts ፥ SemTy.Substitute.substitute subst τ ▪

    | TAPP :
      《type》te,h ⊢ t₁ ፥ τ₁ ▪ →
      《type》te,h ⊢ t₂ ፥ τ₂ ▪ →
      ----------------------------------------------------------------------
      《type》te,h ⊢ Source.TypeExpression.app t₁ t₂ ፥ SemTy.TypeS.App τ₁ τ₂ ▪

  inductive types : Env.TE → Int
                  → List Source.TypeExpression
                  → List SemTy.TypeS
                  → Prop where
    | NIL :
      -------------------------
      《types》te,h ⊢ [] ፥ [] ▪

    | CONS :
      《types》te,h ⊢ ts ፥ τs ▪ →
      《type》 te,h ⊢ t  ፥ τ ▪ →
      -----------------------------------
      《types》te,h ⊢ t :: ts ፥ τ :: τs ▪
end

set_option quotPrecheck false in
set_option hygiene false in
notation  "《class》" ce "," te "," h "⊢" cls "፥" Γ "," τ "▪" => classR ce te h cls Γ τ

/--
Cp. Fig 25
```text
CE, TE, h ⊢ class : Γ τ
```
-/
inductive classR : Env.CE → Env.TE → Int
                 → Source.ClassAssertion
                 → SemTy.SClass_Name
                 → SemTy.TypeS
                 → Prop where
  | CLASS :
    (_, Env.CEEntry.mk Γ h' x C ie) ∈ ce →
    h' < h →
    《type》te, h'' ⊢ List.foldl Source.TypeExpression.app (Source.TypeExpression.var u) ts ፥ τ ▪ →
    ------------------------------------------------------------------------------------------------
    《class》ce,te,h ⊢ Source.ClassAssertion.mk C u ts ፥ Γ , τ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《context》" ce "," te "," h "⊢" cx "፥" θ "▪" => context ce te h cx θ

/--
Cp. Fig 25
```text
CE, TE, h ⊢ cx : θ
```
-/
inductive context : Env.CE → Env.TE → Int
                  → Source.Context
                  → SemTy.Context
                  → Prop where
  | CONTEXT :
    Forall3 class_assertions Γs τs (λ classᵢ Γᵢ τᵢ => 《class》ce,te,h ⊢ classᵢ ፥ Γᵢ ,τᵢ ▪) →
    -----------------------------------------------------------------------------------------
    《context》ce,te,h ⊢ class_assertions ፥ _ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《sig》" ge "⊢" sign "፥" ve "▪" => sig ge sign ve

/--
Cp. Fig 24
```text
GE ⊢ sig : VE
```
-/
inductive sig : Env.GE
              → Source.Signature
              → Env.VE
              → Prop where
  | SIG :
    ke = Env.kindsOf ce te →
    《type》_,_ ⊢ _ ፥ _ ▪ →
    《context》_,_,_ ⊢ cx ፥ θ ▪ →
    ---------------------------------------------------------
    《sig》⟨ce,te,de⟩ ⊢ (Source.Signature.mk v cx _) ፥ [⟨v,_⟩] ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《sigs》" ge "⊢" sign "፥" ve "▪" => sigs ge sign ve

/--
Cp. Fig 24
```text
GE ⊢ sigs : VE
```
-/
inductive sigs : Env.GE
               → List Source.Signature
               → Env.VE
               → Prop where
  | SIGS_NIL :
    ----------------------
    《sigs》ge ⊢ [] ፥ [] ▪

  | SIGS_CONS :
    《sig》 ge ⊢ s  ፥ ve ▪ →
    《sigs》ge ⊢ ss ፥ ves ▪ →
    《oplus》ve ⊞ ves ≡ ve_res ▪ →
    ------------------------------
    《sigs》ge ⊢ s :: ss ፥ ve_res ▪
