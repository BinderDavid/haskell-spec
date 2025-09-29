import HaskellSpec.Environments
import HaskellSpec.Forall2
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Patterns
import HaskellSpec.Source.Literals
import HaskellSpec.Target.Lang
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.Bindings
import HaskellSpec.Elaboration.Dictionary
import HaskellSpec.Elaboration.Literals
import HaskellSpec.Elaboration.Patterns
import HaskellSpec.NonEmpty
import HaskellSpec.Prelude


set_option quotPrecheck false in
set_option hygiene false in
notation  "《exp》" ge "," ie "," ve "⊢" e "⇝" e' "፥" t "▪" => exp ge ie ve e e' t

set_option quotPrecheck false in
set_option hygiene false in
notation  "《quals》" ge "," ie "," ve "⊢" q "⇝" q' "፥" ve' "▪" => quals ge ie ve q q' ve'

set_option quotPrecheck false in
set_option hygiene false in
notation  "《stmts》" ge "," ie "," ve "⊢" s "⇝" e "፥" τ "▪" => stmts ge ie ve s e τ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《gde》" ge "," ie "," ve "⊢" gde' "⇝" gde'' "፥" τ "▪" => gde ge ie ve gde' gde'' τ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《gdes》" ge "," ie "," ve "⊢" gdes' "⇝" gdes'' "፥" τ "▪" => gdes ge ie ve gdes' gdes'' τ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《match》" ge "," ie "," ve "⊢" match' "⇝" match'' "፥" τ "▪" => matchR ge ie ve match' match'' τ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《bind》" ge "," ie "," ve "⊢" bind' "⇝" bind'' "፥" ve' "▪" => bind ge ie ve bind' bind'' ve'

mutual
  /--
  Cp. Fig 40
  ```text
  GE, IE, VE ⊢ quals ⇝ quals : VE
  ```
  -/
  inductive quals : Env.GE → Env.IE → Env.VE
                  → Source.Qualifiers
                  → Target.Qualifiers
                  → Env.VE
                  → Prop where
    | QGEN :
      《exp》  ge,ie,ve                     ⊢ e  ⇝ e'  ፥ Prelude.mk_list τ ▪ →
      《pat》  ge,ie                        ⊢ p  ⇝ p'  ፥ ve_p, τ ▪ →
      《quals》ge,ie,Env.oplusarrow ve ve_p ⊢ qs ⇝ qs' ፥ ve_quals ▪ →
      -------------------------------------------------------------------------------------------------------------------------------
      《quals》ge,ie,ve ⊢ Source.Qualifiers.list_bind p e qs ⇝ Target.Qualifiers.list_bind p' e' qs' ፥ Env.oplusarrow ve_p ve_quals ▪

    | QLET :
      《binds》ge,ie,ve ⊢ bs ⇝ bs' ፥ ve_binds ▪ →
      《quals》ge,ie,Env.oplusarrow ve ve_binds ⊢ qs ⇝ qs' ፥  ve_quals ▪ →
      -----------------------------------------------------------------------------------------------------------------
      《quals》ge,ie,ve ⊢ Source.Qualifiers.lbind bs qs ⇝ Target.Qualifiers.lbind bs' qs' ፥ Env.oplusarrow ve ve_binds ▪

    | QFILTER :
     《exp》  ge,ie,ve ⊢ e  ⇝ e'  ፥ Prelude.bool ▪ →
     《quals》ge,ie,ve ⊢ qs ⇝ qs' ፥ ve_quals ▪ →
     -----------------------------------------------------------------------------------------------
     《quals》 ge,ie,ve ⊢ Source.Qualifiers.guard e qs ⇝ Target.Qualifiers.guard e' qs' ፥ ve_quals ▪

    | QEMPTY :
     ----------------------------------------------------------------------------
     《quals》ge,ie,ve ⊢ Source.Qualifiers.empty ⇝ Target.Qualifiers.empty ፥ [] ▪

  /--
  Cp. Fig 36. 38. 39. 42
  ```text
  GE, IE, VE ⊢ e ⇝ e : τ
  ```
  -/
  inductive exp : Env.GE → Env.IE → Env.VE
                → Source.Expression
                → Target.Expression
                → SemTy.TypeS
                → Prop where
    | VAR_1 :
      ⟨x, (Env.VE_Item.Ordinary x (SemTy.TypeScheme.Forall αs θ τ))⟩ ∈ ve →
      (τsForαs : SemTy.VarSubst) → (Env.dom τsForαs) = αs →
      《dict》 ie ⊢ e ፥ SemTy.Substitute.substitute τsForαs θ ▪ →
      ---------------------------------------------------------------------------------------
      exp ge ie ve (Source.Expression.var x)
        (Target.Expression.app (Target.typ_app_ (Target.Expression.var x) (Env.rng τsForαs)) e)
        (SemTy.Substitute.substitute τsForαs τ)

    | VAR_2 :
      ⟨x, (Env.VE_Item.Class x (SemTy.ClassTypeScheme.Forall α Γ (SemTy.TypeScheme.Forall αs θ τ) ))⟩ ∈ ve →
      《dict》 ie ⊢ e1 ፥ [(Γ, τ)] ▪ →
      (τsForαs : SemTy.VarSubst) →
      (Env.dom τsForαs) = αs →
      《dict》 ie ⊢ e2 ፥ SemTy.Substitute.substitute τsForαs θ ▪ →
      --------------------------------------------------------------------------------------
      exp ge ie ve (Source.Expression.var x)
        (Target.Expression.app (Target.typ_app_ (Target.Expression.app (Target.Expression.typ_app (Target.Expression.var x) (singleton τ)) e1) (Env.rng τsForαs)) e2)
        (SemTy.Substitute.substitute (List.cons (α, τ) τsForαs) τ)

    | LITERAL :
      《literal》  ie ⊢ lit ⇝ e ፥ τ ▪ →
      ------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.lit lit ⇝ e ፥ τ ▪

    | LAMBDA :
      Forall4NE ps ps' ves τs (λ p p' ve' τ' => 《pat》ge,ie ⊢ p ⇝ p' ፥ ve', τ' ▪) →
      《exp》ge,ie,_ ⊢ e ⇝ e' ፥ τ ▪ →
      ---------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.abs ps e ⇝ Target.Expression.abs ps' e' ፥ _ ▪

    | APP :
      《exp》ge,ie,ve ⊢ e₁ ⇝ e₁' ፥ Prelude.mk_funt τ' τ ▪ →
      《exp》ge,ie,ve ⊢ e₂ ⇝ e₂' ፥ τ' ▪ →
      ----------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.app e₁ e₂ ⇝ Target.Expression.app e₁' e₂' ፥ τ ▪

    | LET :
      《binds》ge,ie,ve ⊢ source_binds ⇝ bs ፥ ve_binds ▪ →
      《exp》ge,ie,Env.oplusarrow ve ve_binds ⊢ e ⇝ e' ፥ τ ▪ →
      ---------------------------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.let_bind source_binds e ⇝ Target.Expression.let_bind bs e' ፥ τ ▪

    | CASE :
      《exp》ge,ie,ve ⊢ e ⇝ e' ፥ τ' ▪ →
      /-Forall2NE ms ms' (λ m m' => matchR ge ie ve m m' (SemTy.TypeS.App (SemTy.TypeS.App SemTy.prelude_fun τ') τ)) → -/
      -----------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.case e ms ⇝ Target.Expression.case e' ms' ፥ τ ▪

    | LIST_COMP :
      《quals》ge,ie,ve                         ⊢ quals_source ⇝ quals_target ፥ ve_quals ▪ →
      《exp》  ge,ie,Env.oplusarrow ve ve_quals ⊢ e_source     ⇝ e_target     ፥ τ ▪ →
      -------------------------------------------------------------------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.listComp e_source quals_source ⇝ Target.Expression.listComp e_target quals_target ፥ Prelude.mk_list τ ▪

    | DO :
      《stmts》ge,ie,ve ⊢ s ⇝ e ፥ τ ▪ →
      --------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.do_block s ⇝ e ፥ τ ▪

    | CON :
      ------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.constr _ ⇝ _ ፥ _ ▪

    | UPD :
      ---------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.recUpd _ _ ⇝  _ ፥ _ ▪

    | LABCON :
      -----------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.recConstr _ _ ⇝ _ ፥ _ ▪

    | ENUM_FROM_THEN_TO :
      《exp》ge,ie,ve ⊢ e₁ ⇝ e₁' ፥ τ ▪ →
      《exp》ge,ie,ve ⊢ e₂ ⇝ e₂' ፥ τ ▪ →
      《exp》ge,ie,ve ⊢ e₃ ⇝ e₃' ፥ τ ▪ →
      《dict》     ie ⊢ e ፥ [⟨Prelude.enum, τ⟩] ▪ →
      ------------------------------------------------------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.listRange e₁ (some e₂) (some e₃) ⇝ apply_enumFromThenTo τ e e₁' e₂' e₃' ፥ Prelude.mk_list τ ▪

    | ENUM_FROM_TO :
      《exp》ge,ie,ve ⊢ e₁ ⇝ e₁' ፥ τ ▪ →
      《exp》ge,ie,ve ⊢ e₂ ⇝ e₂' ፥ τ ▪ →
      《dict》     ie ⊢ e ፥ [⟨Prelude.enum, τ⟩] ▪ →
      ------------------------------------------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.listRange e₁ none (some e₂) ⇝ apply_enumFromTo τ e e₁' e₂' ፥ Prelude.mk_list τ ▪

    | ENUM_FROM_THEN :
      《exp》ge,ie,ve ⊢ e₁ ⇝ e₁' ፥ τ ▪ →
      《exp》ge,ie,ve ⊢ e₂ ⇝ e₂' ፥ τ ▪ →
      《dict》     ie ⊢ e ፥ [⟨Prelude.enum, τ⟩]  ▪ →
      --------------------------------------------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.listRange e₁ (some e₂) none ⇝ apply_enumFromThen τ e e₁' e₂' ፥ Prelude.mk_list τ ▪

    | ENUM_FROM :
      《exp》ge,ie,ve ⊢ e₁ ⇝ e₁' ፥ τ ▪ →
      《dict》     ie ⊢ e ፥ [⟨Prelude.enum, τ⟩] ▪ →
      --------------------------------------------------------------------------------------------------------
      《exp》ge,ie,ve ⊢ Source.Expression.listRange e₁ none none ⇝ apply_enumFrom τ e e₁' ፥ Prelude.mk_list τ ▪

  /--
  Cp. Fig 41
  ```text
  GE, IE, VE ⊢ stmts ⇝ e : τ
  ```
  -/
  inductive stmts : Env.GE → Env.IE → Env.VE
                  → Source.Statements
                  → Target.Expression
                  → SemTy.TypeS
                  → Prop where
    | SBIND :
      《exp》 ge,ie,ve ⊢ e ⇝ e₁ ፥ SemTy.TypeS.App τ τ₁ ▪ →
      《pat》    ge,ie ⊢ p ⇝ p' ፥ veₚ,  τ₁ ▪ →
      《stmts》ge,ie,_ ⊢ s ⇝ e₂ ፥ SemTy.TypeS.App τ τ₂ ▪ →
      《dict》      ie ⊢ ed ፥ [⟨Prelude.monad, τ⟩] ▪ →
      -----------------------------------------------------------------------------
      《stmts》ge,ie,ve ⊢ Source.Statements.mbind p e s ⇝ _ ፥ SemTy.TypeS.App τ τ₂ ▪

    | SLET :
      《binds》ge,ie,ve ⊢ bs ⇝ bs' ፥ ve_binds ▪ →
      《stmts》ge,ie,Env.oplusarrow ve ve_binds ⊢ s ⇝ e ፥ τ ▪ →
      ----------------------------------------------------------------------------------------
      《stmts》ge,ie,ve ⊢ Source.Statements.lbind bs s ⇝ Target.Expression.let_bind bs' e ፥ τ ▪

    | STHEN :
      《exp》  ge,ie,ve ⊢ e ⇝ e₁ ፥ SemTy.TypeS.App τ τ₁ ▪ →
      《stmts》ge,ie,ve ⊢ s ⇝ e₂ ፥ SemTy.TypeS.App τ τ₂ ▪ →
      《dict》       ie ⊢ ed ፥ [⟨Prelude.monad, τ⟩] ▪ →
      --------------------------------------------------------------------------
      《stmts》ge,ie,ve ⊢ Source.Statements.seq e s ⇝ _ ፥ SemTy.TypeS.App τ τ₂ ▪

    | SRET :
      《exp》ge,ie,ve ⊢ e ⇝ e' ፥ SemTy.TypeS.App τ τ₁ ▪ →
      《dict》     ie ⊢ _ ፥ [⟨Prelude.monad, τ⟩] ▪ →
      -------------------------------------------------------------------
      《stmts》ge,ie,ve ⊢ Source.Statements.last e ⇝ e' ፥ SemTy.TypeS.App τ τ₁ ▪

  /--
  Cp. Fig 35
  ```text
  GE, IE, VE ⊢ gde ⇝ gde : τ
  ```
  -/
  inductive gde : Env.GE → Env.IE → Env.VE
                → Source.GuardedExp
                → Target.GuardedExp
                → SemTy.TypeS
                → Prop where
    | GDE :
      《exp》ge,ie,ve ⊢ e1 ⇝ e1' ፥ Prelude.bool ▪ →
      《exp》ge,ie,ve ⊢ e2 ⇝ e2' ፥ τ ▪ →
      ---------------------------------------------------------------------------------
      《gde》ge,ie,ve ⊢ Source.GuardedExp.mk e1 e2 ⇝ Target.GuardedExp.mk e1' e2' ፥ τ ▪

  /--
  Cp. Fig 35
  ```text
  GE, IE, VE ⊢ gdes ⇝ gdes : τ
  ```
  -/
  inductive gdes : Env.GE → Env.IE → Env.VE
                 → Source.GuardedExprs
                 → Target.GuardedExprs
                 → SemTy.TypeS
                 → Prop where
    | GDES :
      /- Forall2NE gs gs' (λ g g' => gde ge ie (Env.oplusarrow ve ve_binds) g g' τ) → -/
      《binds》ge,ie,ve ⊢ bs ⇝ bs' ፥ ve_binds ▪ →
      -------------------------------------------------------------------------------
      《gdes》ge,ie,ve ⊢ Source.GuardedExprs.mk gs bs ⇝ Target.GuardedExprs.mk gs' bs' ፥ τ ▪

  /--
  Cp. Fig 35
  ```text
  GE, IE, VE ⊢ match ⇝ match : τ
  ```
  -/
  inductive matchR : Env.GE
                   → Env.IE
                   → Env.VE
                   → Source.Match
                   → Target.Match
                   → SemTy.TypeS
                   → Prop where
    | MATCH :
      Forall3NE patts patts' ves (λ p p' ve' => 《pat》ge,ie ⊢ p ⇝ p' ፥ ve',τ ▪) →
      《gdes》ge,ie,_  ⊢ gs ⇝ gs' ፥ τ ▪ →
      -----------------------------------------------------------------------------
      《match》ge,ie,ve ⊢ Source.Match.mk patts gs ⇝ Target.Match.mk patts' gs' ፥ _ ▪

  /--
  Cp. Fig 34
  ```text
  GE, IE, VE ⊢ bind ⇝ bind : VE
  ```
  -/
  inductive bind : Env.GE → Env.IE → Env.VE
                 → Source.Binding
                 → Target.Binding
                 → Env.VE
                 → Prop where
    | FUNBIND :
      /- Forall2NE match_sources match_targets (λ match_sourceᵢ match_targetᵢ =>
      matchR ge ie ve match_sourceᵢ match_targetᵢ τ) → -/
      x = QVariable.Unqualified (Variable.Mk "x") →
      bind ge ie ve
        (Source.Binding.bind_match x match_sources)
        (Target.Binding.bind_match x match_targets)
        (List.singleton (Prod.mk x (Env.VE_Item.Ordinary x (SemTy.TypeScheme.Forall [] [] τ))))

    | PATBIND :
      《pat》ge,ie ⊢ p_source ⇝ p_target ፥ veₚ,  τ ▪ →
      《gdes》ge,ie,ve ⊢ gdes_source ⇝ gedes_target ፥ τ ▪ →
      bind ge ie ve (Source.Binding.bind_pat p_source gdes_source) (Target.Binding.bind_pat p_target gdes_target) veₚ

end

set_option quotPrecheck false in
set_option hygiene false in
notation  "《monobinds》" ge "," ie "," ve "⊢" bs "⇝" bs' "፥" ve' "▪" => monobinds ge ie ve bs bs' ve'

/--
Cp. Fig 34
```text
GE, IE, VE ⊢ bindG ⇝ binds : VE
```
-/
inductive monobinds : Env.GE → Env.IE → Env.VE
                    → List Source.Binds
                    → Target.Binds
                    → Env.VE
                    → Prop where
  | MONOBINDS :
    Forall3 bs bs' ves (λ b b' veᵢ => 《bind》ge,ie,ve ⊢ b ⇝ b' ፥ veᵢ ▪ ) →
    ------------------------------------
    《monobinds》ge,ie,ve ⊢ _ ⇝ _ ፥ _ ▪
