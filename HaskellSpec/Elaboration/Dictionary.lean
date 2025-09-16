import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes

set_option quotPrecheck false in
set_option hygiene false in
notation  "《dict》" ie "⊢" e "፥" τ "▪"  => dict ie e τ

/--
Cp. Fig 28
```text
IE ⊢ e : (Γ₁ τ₁,…,Γₙ τₙ)
```
-/
inductive dict : Env.IE
               → Target.Expression
               → SemTy.Context
               → Prop where
  | DICT_TUPLE :
    ---------------------
    《dict》 ie ⊢ _ ፥ _ ▪

  | DICT_VAR :
    Env.IE_Entry.BoundInDictionaryAbstraction v class_name α τs ∈ ie →
    ------------------------------------------------------------------------------------------------------------------------------------
    《dict》 ie ⊢ (Target.Expression.var (QVariable.Unqualified v)) ፥ [⟨class_name, τs.foldl SemTy.TypeS.App (SemTy.TypeS.Variable α)⟩] ▪

  | DICT_INST :
    (Env.IE_Entry.DictioanryFromInstanceDeclaration x αs θ Γ χ _) ∈ ie ->
    (τsForαs : SemTy.VarSubst) → (Env.dom τsForαs) = αs →
    《dict》ie ⊢ e ፥ SemTy.Substitute.substitute τsForαs θ ▪ →
    τs = Env.rng τsForαs →
    e_target = (Target.Expression.app (match τs with
                                       | [] => Target.Expression.var x
                                       | (t :: ts)  => Target.Expression.typ_app (Target.Expression.var x) (NonEmpty.mk t ts)) e) →
    ------------------------------------------------------------------------------------------
    《dict》 ie ⊢ e_target ፥ [⟨Γ, τs.foldl SemTy.TypeS.App (SemTy.TypeS.TypeConstructor χ)⟩] ▪

  | DICT_SUPER :
    Env.IE_Entry.ExtractsADictionaryForTheSuperclass x α Γ Γ' ∈ ie →
    《dict》 ie ⊢ e ፥ [⟨Γ', τ⟩] ▪ →
    -----------------------------------------------------------------------------------------------------------------------
    《dict》 ie ⊢ Target.Expression.app (Target.Expression.typ_app (Target.Expression.var x) (singleton τ)) e ፥ [⟨Γ, τ⟩] ▪
