import HaskellSpec.Environments
import HaskellSpec.Forall2
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Patterns
import HaskellSpec.Source.Literals
import HaskellSpec.Target.Lang
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.Modules
import HaskellSpec.Elaboration.Bindings
import HaskellSpec.NonEmpty

def fromRationalAfterRatio (n d : Int) : Target.Expression :=
  Target.Expression.app
   (Target.Expression.var SemTy.prelude_fromrational)
   (Target.Expression.app
     (Target.Expression.app
       (Target.Expression.var SemTy.ratio_percent)
       (Target.Expression.lit (Target.Literal.integer n))
     )
     (Target.Expression.lit (Target.Literal.integer d)))

def fromInteger (i : Int) : Target.Expression :=
  Target.Expression.app
    (Target.Expression.var SemTy.prelude_frominteger)
    (Target.Expression.lit (Target.Literal.integer i))

/--
Cp. Fig 37
```text
IE ⊢ literal ⇝ e : τ
```
-/
inductive literal : Env.IE
                  → Source.Literal
                  → Target.Expression
                  → SemTy.TypeS
                  → Prop where
  | LIT_CHAR :
    literal ie
            (Source.Literal.char c)
            (Target.Expression.lit (Target.Literal.char c))
            SemTy.prelude_char

  | LIT_STRING :
    literal ie
            (Source.Literal.string s)
            (Target.Expression.lit (Target.Literal.string s))
            (SemTy.TypeS.App SemTy.prelude_list SemTy.prelude_char)

  | LIT_INTEGER :
    dict ie (fromInteger i) [⟨SemTy.prelude_num, τ⟩] →
    literal ie
            (Source.Literal.integer i)
            (fromInteger i)
            τ

  | LIT_FLOAT :
    dict ie (fromRationalAfterRatio n d) [⟨SemTy.prelude_fractional, τ⟩] →
    literal ie
            (Source.Literal.float n d)
            (fromRationalAfterRatio n d)
            τ

def unqual_var (var : QVariable) : Variable :=
  match var with
    | (QVariable.Qualified _m x) => x
    | (QVariable.Unqualified x) => x

/- Applying typeclass methods to their type, dictionary, and term arguments. -/

/- Prelude.== τ ed e -/
def apply_equals : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression := λ τ ed e =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.typ_app
        (Target.Expression.var SemTy.prelude_equals)
        (NonEmpty.mk τ []))
    ed)
    e

/- Prelude!enumFromThenTo τ e e1' e2' e3' -/
def apply_enumFromThenTo : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression  → Target.Expression → Target.Expression :=
  λ τ e e1' e2' e3' =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.app
        (Target.Expression.app
          (Target.Expression.typ_app (Target.Expression.var SemTy.prelude_enum_from_then_to) (NonEmpty.mk τ []))
        e)
      e1')
    e2')
  e3'

/- Prelude!enumFromTo τ e e1' e2' -/
def apply_enumFromTo : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression → Target.Expression :=
  λ τ e e1' e2' =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.app
        (Target.Expression.typ_app (Target.Expression.var SemTy.prelude_enum_from_to) (NonEmpty.mk τ []))
      e)
    e1')
  e2'

/- Prelude!enumFromThen τ e e1' e2' -/
def apply_enumFromThen : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression → Target.Expression :=
  λ τ e e1' e2' =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.app
        (Target.Expression.typ_app (Target.Expression.var SemTy.prelude_enum_from_then) (NonEmpty.mk τ []))
      e)
    e1')
  e2'

/- Prelude!enumFrom τ e e1' -/
def apply_enumFrom : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression :=
  λ τ e e1' =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.typ_app (Target.Expression.var SemTy.prelude_enum_from) (NonEmpty.mk τ []))
    e)
  e1'

/--
Cp. Fig 43. 44.
```
GE, IE ⊢ p ⇝ p : VE, τ
```
-/
inductive pat : Env.GE → Env.IE
              → Source.Pattern
              → Target.Pattern
              → Env.VE
              → SemTy.TypeS
              → Prop where
  | PVAR :
    σ = (SemTy.TypeScheme.Forall [] [] τ) →
    pat
      ge
      ie
      (Source.Pattern.var x)
      (Target.Pattern.var (unqual_var x) σ)
      [(x, Env.VE_Item.Ordinary x σ)]
      τ

  | PIRR :
    pat ge ie p₁ p₂ ve τ →
    pat ge ie (Source.Pattern.lazy p₁) (Target.Pattern.lazy p₂) ve τ

  | PWILD :
    pat ge ie Source.Pattern.wildcard Target.Pattern.wildcard [] τ

  | PCHAR :
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.char c))
      (Target.Pattern.char c)
      []
      SemTy.prelude_char

  | PSTRING :
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.string s))
      (Target.Pattern.string s)
      []
      (SemTy.TypeS.App SemTy.prelude_list SemTy.prelude_char)

  | PINTEGER :
    literal ie (Source.Literal.integer i) e τ →
    dict ie ed [⟨SemTy.prelude_eq, τ⟩] →
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.integer i))
      (Target.Pattern.exp (apply_equals τ ed e))
      []
      τ

  | PFLOAT :
    literal ie (Source.Literal.float n d) e τ →
    dict ie ed [⟨SemTy.prelude_eq, τ⟩]→
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.float n d))
      (Target.Pattern.exp (apply_equals τ ed e))
      []
      τ

-- These are helpers we need for type variable substitution in VAR_1
-- below.
--  BEGIN
def SemTyVarSubst := Env.Env SemTy.Type_Variable SemTy.TypeS

def applySubstTypeS (subst : SemTyVarSubst) : SemTy.TypeS → SemTy.TypeS
  | SemTy.TypeS.Variable τ => Option.getD (List.lookup τ subst) (SemTy.TypeS.Variable τ)
  | SemTy.TypeS.TypeConstructor χ => SemTy.TypeS.TypeConstructor χ
  | SemTy.TypeS.App τ1 τ2 => SemTy.TypeS.App (applySubstTypeS subst τ1) (applySubstTypeS subst τ2)

def applySubstContext (subst : SemTyVarSubst) : SemTy.Context → SemTy.Context :=
  List.map (Prod.map id (applySubstTypeS subst))
-- END

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
      exp ge ie ve e e' (SemTy.TypeS.App SemTy.prelude_list τ) →
      pat ge ie p p' ve_p τ →
      quals ge ie (Env.oplusarrow ve ve_p) qs qs' ve_quals →
      quals ge ie ve
        (Source.Qualifiers.list_bind p e qs)
        (Target.Qualifiers.list_bind p' e' qs')
        (Env.oplusarrow ve_p ve_quals)

    | QLET :
      binds ge ie ve bs (Target.Binds.cons b' bs') ve_binds →
      quals ge ie (Env.oplusarrow ve ve_binds) qs qs' ve_quals →
      quals ge ie ve (Source.Qualifiers.lbind bs qs)
        (Target.Qualifiers.lbind (Target.Binds.cons b' bs') qs')
        (Env.oplusarrow ve ve_binds)

    | QFILTER :
      exp ge ie ve e e' SemTy.prelude_bool →
      quals ge ie ve qs qs' ve_quals →
      quals ge ie ve
        (Source.Qualifiers.guard e qs)
        (Target.Qualifiers.guard e' qs')
        ve_quals

    | QEMPTY :
      quals ge ie ve Source.Qualifiers.empty Target.Qualifiers.empty []
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
      (x : QVariable) → (ve : Env.VE) →
      ⟨x, (Env.VE_Item.Ordinary x (SemTy.TypeScheme.Forall αs θ τ))⟩ ∈ ve →
      (τsForαs : SemTyVarSubst) → (Env.dom τsForαs) = αs →
      dict ie e (applySubstContext τsForαs θ) →
      ---------------------------------------------------------------------------------------
      exp ge ie ve (Source.Expression.var x)
        (Target.Expression.app (Target.typ_app_ (Target.Expression.var x) (Env.rng τsForαs)) e)
        (applySubstTypeS τsForαs τ)

    | VAR_2 :
      (x : QVariable) → (ve : Env.VE) →
      ⟨x, (Env.VE_Item.Class x (SemTy.ClassTypeScheme.Forall α Γ (SemTy.TypeScheme.Forall αs θ τ) ))⟩ ∈ ve →
      dict ie e1 [(Γ, τ)] →
      (τsForαs : SemTyVarSubst) → (Env.dom τsForαs) = αs →
      dict ie e2 (applySubstContext τsForαs θ) →
      --------------------------------------------------------------------------------------
      exp ge ie ve (Source.Expression.var x)
        (Target.Expression.app (Target.typ_app_ (Target.Expression.app (Target.Expression.typ_app (Target.Expression.var x) (singleton τ)) e1) (Env.rng τsForαs)) e2)
        (applySubstTypeS (List.cons (α, τ) τsForαs) τ)

    | LITERAL :
      literal ie lit e τ →
      exp ge ie ve (Source.Expression.lit lit) e τ

    | LAMBDA :
      Forall4NE ps ps' ves τs (λ p p' ve' τ' => pat ge ie p p' ve' τ') →
      exp ge ie _ e e' τ →
      exp ge
          ie
          ve
          (Source.Expression.abs ps e)
          (Target.Expression.abs ps' e')
          _

    | APP :
      exp ge ie ve e₁ e₁' (SemTy.TypeS.App (SemTy.TypeS.App SemTy.prelude_fun τ') τ) →
      exp ge ie ve e₂ e₂' τ' →
      ------------------------------------
      exp ge
          ie
          ve
          (Source.Expression.app e₁ e₂)
          (Target.Expression.app e₁' e₂')
          τ

    | LET :
      binds ge ie ve source_binds (Target.Binds.cons ve_head ve_tail) ve_binds →
      exp ge ie (Env.oplusarrow ve ve_binds) e e' τ →
      exp ge ie ve (Source.Expression.let_bind source_binds e)
        (Target.Expression.let_bind (Target.Binds.cons ve_head ve_tail) e')
        τ

    | CASE :
      exp ge ie ve e e' τ' →
      /-Forall2NE ms ms' (λ m m' => matchR ge ie ve m m' (SemTy.TypeS.App (SemTy.TypeS.App SemTy.prelude_fun τ') τ)) → -/
      exp ge ie ve
        (Source.Expression.case e ms)
        (Target.Expression.case e' ms')
        τ

    | LIST_COMP :
      quals ge ie ve quals_source quals_target ve_quals ->
      exp ge ie (Env.oplusarrow ve ve_quals) e_source e_target  τ ->
      exp ge ie ve
       (Source.Expression.listComp e_source quals_source)
       (Target.Expression.listComp e_target quals_target)
       (SemTy.mk_list τ)

    | DO :
      stmts ge ie ve s e τ →
      exp ge ie ve (Source.Expression.do_block s) e τ

    | CON :
      exp ge ie ve (Source.Expression.constr _) _ _

    | UPD :
      exp ge ie ve (Source.Expression.recUpd _ _) _ _

    | LABCON :
      exp ge ie ve (Source.Expression.recConstr _ _) _ _

    | ENUM_FROM_THEN_TO :
      exp ge ie ve e1 e1' τ →
      exp ge ie ve e2 e2' τ →
      exp ge ie ve e3 e3' τ →
      dict ie e [⟨SemTy.prelude_enum, τ⟩] →
      exp ge ie ve
        (Source.Expression.listRange e1 (some e2) (some e3))
        (apply_enumFromThenTo τ e e1' e2' e3')
        (SemTy.TypeS.App SemTy.prelude_list τ)

    | ENUM_FROM_TO :
      exp ge ie ve e1 e1' τ →
      exp ge ie ve e2 e2' τ →
      dict ie e [⟨SemTy.prelude_enum, τ⟩] →
      exp ge ie ve
        (Source.Expression.listRange e1 none (some e2))
        (apply_enumFromTo τ e e1' e2')
        (SemTy.TypeS.App SemTy.prelude_list τ)

    | ENUM_FROM_THEN :
      exp ge ie ve e1 e1' τ →
      exp ge ie ve e2 e2' τ →
      dict ie e [⟨SemTy.prelude_enum, τ⟩] →
      exp ge ie ve
        (Source.Expression.listRange e1 (some e2) none)
        (apply_enumFromThen τ e e1' e2')
        (SemTy.TypeS.App SemTy.prelude_list τ)

    | ENUM_FROM :
      exp ge ie ve e1 e1' τ →
      dict ie e [⟨SemTy.prelude_enum, τ⟩] →
      exp ge ie ve
        (Source.Expression.listRange e1 none none)
        (apply_enumFrom τ e e1')
        (SemTy.TypeS.App SemTy.prelude_list τ)

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
      exp ge ie ve e e₁ (SemTy.TypeS.App τ τ₁) →
      pat ge ie p p' veₚ τ₁ →
      stmts ge ie _ s e₂ (SemTy.TypeS.App τ τ₂) →
      dict ie ed [⟨SemTy.prelude_monad, τ⟩] →
      stmts ge ie ve (Source.Statements.mbind p e s) _ (SemTy.TypeS.App τ τ₂)

    | SLET :
      binds ge ie ve bs (Target.Binds.cons b' bs') ve_binds →
      stmts ge ie (Env.oplusarrow ve ve_binds) s e τ →
      stmts ge ie ve (Source.Statements.lbind bs s)
        (Target.Expression.let_bind (Target.Binds.cons b' bs') e) τ

    | STHEN :
      exp ge ie ve e e₁ (SemTy.TypeS.App τ τ₁) →
      stmts ge ie ve s e₂ (SemTy.TypeS.App τ τ₂) →
      dict ie ed [⟨SemTy.prelude_monad, τ⟩] →
      stmts ge ie ve (Source.Statements.seq e s) _ (SemTy.TypeS.App τ τ₂)

    | SRET :
      exp ge ie ve e e' (SemTy.TypeS.App τ τ₁) →
      dict ie _ [⟨SemTy.prelude_monad, τ⟩] →
      stmts ge ie ve (Source.Statements.last e) e' (SemTy.TypeS.App τ τ₁)

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
      exp ge ie ve e1 e1' SemTy.prelude_bool →
      exp ge ie ve e2 e2' τ →
      gde ge ie ve (Source.GuardedExp.mk e1 e2) (Target.GuardedExp.mk e1' e2') τ

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
      /- Forall2NE gs gs' (λ g g' => gde ge ie _ g g' τ) → -/
      binds ge ie ve bs bs' ve_binds →
      gdes ge ie ve (Source.GuardedExprs.mk gs bs) _ τ

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
      Forall3NE pats pats' ves (λ p p' ve' => pat ge ie p p' ve' τ) →
      gdes ge ie _ gs gs' τ →
      matchR ge ie ve (Source.Match.mk pats gs) (Target.Match.mk pats' gs') _

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
      pat ge ie p_source p_target veₚ τ ->
      gdes ge ie ve gdes_source gedes_target τ ->
      bind ge ie ve (Source.Binding.bind_pat p_source gdes_source) (Target.Binding.bind_pat p_target gdes_target) veₚ

end
