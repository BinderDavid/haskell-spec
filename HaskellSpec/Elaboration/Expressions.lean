import HaskellSpec.Environments
import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.Modules

/--
The type `Prelude!Char`
-/
def prelude_char : SemTy.TypeS :=
  SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Qualified (Module_Name.Mk "Prelude") (Type_Name.Mk "Char")) SemTy.Kind.Star)

/--
The type `Prelude!Bool`
-/
def prelude_bool : SemTy.TypeS :=
  SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Qualified (Module_Name.Mk "Prelude") (Type_Name.Mk "Bool")) SemTy.Kind.Star)

/--
The type `[] : * → *`
-/
def prelude_list : SemTy.TypeS :=
SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.List) (SemTy.Kind.Fun SemTy.Kind.Star SemTy.Kind.Star))


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
          (SemTy.TypeS.App prelude_list τ)
  | EnumFromTo :
      exp ge ie ve e1 e1' τ
    → exp ge ie ve e2 e2' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
          (Source.Expression.listRange e1 none (some e2))
          e /- Prelude!enumFromTo τ e e1' e2' -/
          (SemTy.TypeS.App prelude_list τ)
  | EnumFromThen :
      exp ge ie ve e1 e1' τ
    → exp ge ie ve e2 e2' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
          (Source.Expression.listRange e1 (some e2) none)
          e /- Prelude!enumFromThen τ e e1' e2' -/
          (SemTy.TypeS.App prelude_list τ)
  | EnumFrom :
      exp ge ie ve e1 e1' τ
    /- → dict ie e (Prelude!Enum^* τ) -/
    → exp ge ie ve
          (Source.Expression.listRange e1 none none)
          e /- Prelude!enumFrom τ e e1' -/
          (SemTy.TypeS.App prelude_list τ)

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
  | Gde : exp ge ie ve e1 e1' prelude_bool
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
                  (SemTy.TypeS.App prelude_list prelude_char)

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
