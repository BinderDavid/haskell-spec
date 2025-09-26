import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes


mutual
  inductive dict_single : Env.IE
                        → Target.Expression
                        → SemTy.SClass_Name × SemTy.TypeS
                        → Prop where
    | DICT_VAR :
      Env.IE_Entry.BoundInDictionaryAbstraction v class_name α τs ∈ ie →
      --------------------------------------------------------------------------------------------------------------------------------
      dict_single ie (Target.Expression.var (QVariable.Unqualified v)) ⟨class_name, τs.foldl SemTy.TypeS.App (SemTy.TypeS.Variable α)⟩

    | DICT_INST :
      (Env.IE_Entry.DictionaryFromInstanceDeclaration x αs θ Γ χ _) ∈ ie ->
      (τsForαs : SemTy.VarSubst) → (Env.dom τsForαs) = αs →
      dict_ctx ie e (SemTy.Substitute.substitute τsForαs θ) →
      τs = Env.rng τsForαs →
      e_target = (Target.Expression.app (match τs with
                                         | [] => Target.Expression.var x
                                         | (t :: ts)  => Target.Expression.typ_app (Target.Expression.var x) (NonEmpty.mk t ts)) e) →
      -------------------------------------------------------------------------------------------------------------------------------
      dict_single ie _ _

    | DICT_SUPER :
      Env.IE_Entry.ExtractsADictionaryForTheSuperclass x α Γ Γ' ∈ ie →
      dict_single ie e ⟨Γ', τ⟩ →
      -----------------------------------------
      dict_single ie (Target.Expression.app (Target.Expression.typ_app (Target.Expression.var x) (singleton τ)) e) ⟨Γ, τ⟩

  inductive dict_ctx : Env.IE
                     → Target.Expression
                     → SemTy.Context
                     → Prop where
    | DICT_TUPLE :
      dict_ctx _ _ _
end


set_option quotPrecheck false in
set_option hygiene false in
notation  "《dict》" ie "⊢" e "፥" τ "▪"  => dict ie e τ
