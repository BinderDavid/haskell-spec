import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Patterns
import HaskellSpec.Source.Literals
import HaskellSpec.Target.Lang
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.Modules

/--
Cp. Fig 35
```text
GE, IE, VE ⊢ match ⇝ match : τ
```
-/
inductive matchR : Env.GE → Env.IE → Env.VE
                 → Source.Match
                 → Target.Match
                 → SemTy.TypeS
                 → Prop where

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
  | EnumFromThenTo :
      exp ge ie ve e1 e1' τ
    → exp ge ie ve e2 e2' τ
    → exp ge ie ve e3 e3' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
          (Source.Expression.listRange e1 (some e2) (some e3))
          e /- Prelude!enumFromThenTo τ e e1' e2' e3' -/
          (SemTy.TypeS.App SemTy.prelude_list τ)
  | EnumFromTo :
      exp ge ie ve e1 e1' τ
    → exp ge ie ve e2 e2' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
          (Source.Expression.listRange e1 none (some e2))
          e /- Prelude!enumFromTo τ e e1' e2' -/
          (SemTy.TypeS.App SemTy.prelude_list τ)
  | EnumFromThen :
      exp ge ie ve e1 e1' τ
    → exp ge ie ve e2 e2' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
          (Source.Expression.listRange e1 (some e2) none)
          e /- Prelude!enumFromThen τ e e1' e2' -/
          (SemTy.TypeS.App SemTy.prelude_list τ)
  | EnumFrom :
      exp ge ie ve e1 e1' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
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
  | Gde : exp ge ie ve e1 e1' SemTy.prelude_bool
        → exp ge ie ve e2 e2' τ
        → gde ge ie ve (Source.GuardedExp.gExp_eq e1 e2) (Target.GuardedExp.gExp_eq e1' e2') τ

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
  | LitChar : literal
                  env
                  (Source.Literal.char c)
                  (Target.Expression.expr_lit (Target.Literal.char c))
                  prelude_char
  | LitString : literal
                  env
                  (Source.Literal.string s)
                  (Target.Expression.expr_lit (Target.Literal.string s))
                  (SemTy.TypeS.App SemTy.prelude_list SemTy.prelude_char)

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
  | PVar :
    σ = (SemTy.TypeScheme.Forall [] (SemTy.Context.Mk []) τ) →
    pat
      ge
      ie
      (Source.Pattern.var x)
      (Target.Pattern.var (unqual_var x) σ)
      [(x, Env.VE_Item.Ordinary x σ)]
      τ
  | PIrr :
    pat ge ie p₁ p₂ ve τ →
    pat ge ie (Source.Pattern.lazy p₁) (Target.Pattern.lazy p₂) ve τ
  | PWild :
    pat ge ie Source.Pattern.wildcard Target.Pattern.wildcard [] τ
  | PChar :
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.char c))
      (Target.Pattern.lit (Target.Literal.char c))
      []
      prelude_char
  | PString :
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.string s))
      (Target.Pattern.lit (Target.Literal.string s))
      []
      (SemTy.TypeS.App prelude_list prelude_char)
  | PInteger :
    literal ie (Source.Literal.integer i) e τ →
    dict ie ed →
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.integer i))
      _ /- { (Prelude.== τ ed e) } -/
      []
      τ
  | PFloat :
    literal ie (Source.Literal.float n d) e τ →
    dict ie ed →
    pat
      ge
      ie
      (Source.Pattern.lit (Source.Literal.float n d))
      _ /- { (Prelude.== τ ed e) } -/
      []
      τ
