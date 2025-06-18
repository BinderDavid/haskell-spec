import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Patterns
import HaskellSpec.Source.Literals
import HaskellSpec.Target.Lang
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.Modules


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
    literal env
            (Source.Literal.char c)
            (Target.Expression.expr_lit (Target.Literal.char c))
            SemTy.prelude_char

  | LIT_STRING :
    literal env
            (Source.Literal.string s)
            (Target.Expression.expr_lit (Target.Literal.string s))
            (SemTy.TypeS.App SemTy.prelude_list SemTy.prelude_char)

def unqual_var (var : QVariable) : Variable :=
  match var with
    | (QVariable.Qualified _m x) => x
    | (QVariable.Unqualified x) => x

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
    σ = (SemTy.TypeScheme.Forall [] (SemTy.Context.Mk []) τ) →
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
      (Target.Pattern.lit (Target.Literal.char c))
      []
      prelude_char

  | PSTRING :
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.string s))
      (Target.Pattern.lit (Target.Literal.string s))
      []
      (SemTy.TypeS.App SemTy.prelude_list SemTy.prelude_char)

  | PINTEGER :
    literal ie (Source.Literal.integer i) e τ →
    dict ie ed [⟨SemTy.prelude_eq, τ⟩] →
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.integer i))
      _ /- { (Prelude.== τ ed e) } -/
      []
      τ

  | PFLOAT :
    literal ie (Source.Literal.float n d) e τ →
    dict ie ed [⟨SemTy.prelude_eq, τ⟩]→
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.float n d))
      _ /- { (Prelude.== τ ed e) } -/
      []
      τ

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
    -- i ∈ [1,k] : GE, IE ⊢ pᵢ ⇝ pᵢ' : VEᵢ, τ →
    -- GE, IE, _ ⊢ gdes ⇝ gdes'  : τ →
    matchR ge ie ve (Source.Match.mk pats gdes) (Target.Match.mk pats' gdes') _



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
    gdes _ _ _ _ _ _

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
    exp ge ie ve (Source.Expression.var v) _ _

  | VAR_2 :
    exp ge ie ve (Source.Expression.var v) _ _

  | LITERAL :
    literal ie lit e τ →
    exp ge ie ve (Source.Expression.lit lit) e τ

  | LAMBDA :
    exp ge ie ve (Source.Expression.abs _ _) _ _

  | APP :
    exp ge ie ve (Source.Expression.app e₁ e₂) _ _

  | LET :
    exp ge ie ve (Source.Expression.let_bind _ _) _ _

  | CASE :
    exp ge ie ve (Source.Expression.case _ _) _ _

  | LIST_COMP :
    exp ge ie ve (Source.Expression.listComp _ _) _ _

  | DO :
    exp ge ie ve (Source.Expression.do_block _) _ _

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
      e /- Prelude!enumFromThenTo τ e e1' e2' e3' -/
      (SemTy.TypeS.App SemTy.prelude_list τ)

  | ENUM_FROM_TO :
    exp ge ie ve e1 e1' τ →
    exp ge ie ve e2 e2' τ →
    dict ie e [⟨SemTy.prelude_enum, τ⟩] →
    exp ge ie ve
      (Source.Expression.listRange e1 none (some e2))
      e /- Prelude!enumFromTo τ e e1' e2' -/
      (SemTy.TypeS.App SemTy.prelude_list τ)

  | ENUM_FROM_THEN :
    exp ge ie ve e1 e1' τ →
    exp ge ie ve e2 e2' τ →
    dict ie e [⟨SemTy.prelude_enum, τ⟩] →
    exp ge ie ve
      (Source.Expression.listRange e1 (some e2) none)
      e /- Prelude!enumFromThen τ e e1' e2' -/
      (SemTy.TypeS.App SemTy.prelude_list τ)

  | ENUM_FROM :
    exp ge ie ve e1 e1' τ →
    dict ie e [⟨SemTy.prelude_enum, τ⟩] →
    exp ge ie ve
          (Source.Expression.listRange e1 none none)
          e /- Prelude!enumFrom τ e e1' -/
          (SemTy.TypeS.App SemTy.prelude_list τ)

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
    gde ge ie ve (Source.GuardedExp.gExp_eq e1 e2) (Target.GuardedExp.gExp_eq e1' e2') τ

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
    stmts ge ie ve (Source.Statements.mbind _ _ _) _ _

  | SLET :
    stmts ge ie ve (Source.Statements.lbind _ _) _ _

  | STHEN :
    stmts ge ie ve (Source.Statements.seq _ _) _ _

  | SRET :
    exp ge ie ve e e' (SemTy.TypeS.App τ τ₁) →
    dict ie _ [⟨SemTy.prelude_monad, τ⟩] →
    stmts ge ie ve (Source.Statements.last e) e' (SemTy.TypeS.App τ τ₁)
