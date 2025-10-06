import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes

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
    ---------------------------------------------------------
    《sig》⟨ce,te,de⟩ ⊢ (Source.Signature.mk v _ _) ፥ [⟨v,_⟩] ▪

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
    -------------------------------------------
    《sigs》ge ⊢ s :: ss ፥ List.append ve ves ▪
