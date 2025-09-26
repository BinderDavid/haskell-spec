import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes


def make_tuple (es : List Target.Expression) : Target.Expression :=
  sorry



set_option quotPrecheck false in
set_option hygiene false in
notation  "《dict-single》" ie "⊢" e "፥" ca "▪"  => dict_single ie e ca

set_option quotPrecheck false in
set_option hygiene false in
notation  "《dict-many》" ie "⊢" es "፥" θ "▪"  => dict_many ie es θ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《dict》" ie "⊢" e "፥" θ "▪"  => dict ie e θ

mutual
  inductive dict_single : Env.IE
                        → Target.Expression
                        → SemTy.SClass_Name × SemTy.TypeS
                        → Prop where
    | DICT_VAR :
      Env.IE_Entry.BoundInDictionaryAbstraction v class_name α τs ∈ ie →
      --------------------------------------------------------------------------------------------------------------------------------
      《dict-single》ie ⊢ Target.Expression.var (QVariable.Unqualified v) ፥ ⟨class_name, τs.foldl SemTy.TypeS.App (SemTy.TypeS.Variable α)⟩ ▪

    | DICT_INST :
      (Env.IE_Entry.DictionaryFromInstanceDeclaration x αs θ Γ χ _) ∈ ie ->
      (τsForαs : SemTy.VarSubst) → (Env.dom τsForαs) = αs →
      《dict》ie ⊢ e ፥ (SemTy.Substitute.substitute τsForαs θ) ▪ →
      τs = Env.rng τsForαs →
      e_target = (Target.Expression.app (match τs with
                                         | [] => Target.Expression.var x
                                         | (t :: ts)  => Target.Expression.typ_app (Target.Expression.var x) (NonEmpty.mk t ts)) e) →
      -------------------------------------------------------------------------------------------------------------------------------
      《dict-single》ie ⊢ e_target ፥ ⟨Γ, τs.foldl SemTy.TypeS.App (SemTy.TypeS.TypeConstructor χ)⟩ ▪

    | DICT_SUPER :
      Env.IE_Entry.ExtractsADictionaryForTheSuperclass x α Γ Γ' ∈ ie →
      《dict-single》ie ⊢ e ፥ ⟨Γ', τ⟩ ▪ →
      -------------------------------------------------------------------------------------------------------------------------
      《dict-single》ie ⊢ Target.Expression.app (Target.Expression.typ_app (Target.Expression.var x) (singleton τ)) e ፥ ⟨Γ, τ⟩ ▪

  inductive dict_many : Env.IE
                      → List Target.Expression
                      → SemTy.Context
                      → Prop where
    | NIL :
      --------------------------
      《dict-many》ie ⊢ [] ፥ [] ▪

    | CONS :
      《dict-single》ie ⊢ e ፥ c ▪ →
      《dict-many》ie ⊢ es ፥ cs ▪ →
      ------------------------------------
      《dict-many》ie ⊢ e::es ፥ c::cs ▪

  inductive dict : Env.IE
                     → Target.Expression
                     → SemTy.Context
                     → Prop where
    | DICT_TUPLE :
      《dict-many》ie ⊢ es ፥ θ ▪ →
      -------------------------------
      《dict》ie ⊢ make_tuple es ፥ θ ▪
end
