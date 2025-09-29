import HaskellSpec.Target.Lang
import HaskellSpec.Prelude
import HaskellSpec.Elaboration.Literals
import HaskellSpec.Source.Patterns

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
        (Target.Expression.var Prelude.equals)
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
          (Target.Expression.typ_app (Target.Expression.var Prelude.enum_from_then_to) (NonEmpty.mk τ []))
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
        (Target.Expression.typ_app (Target.Expression.var Prelude.enum_from_to) (NonEmpty.mk τ []))
      e)
    e1')
  e2'

/- Prelude!enumFromThen τ e e1' e2' -/
def apply_enumFromThen : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression → Target.Expression :=
  λ τ e e1' e2' =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.app
        (Target.Expression.typ_app (Target.Expression.var Prelude.enum_from_then) (NonEmpty.mk τ []))
      e)
    e1')
  e2'

/- Prelude!enumFrom τ e e1' -/
def apply_enumFrom : SemTy.TypeS → Target.Expression → Target.Expression → Target.Expression :=
  λ τ e e1' =>
  Target.Expression.app
    (Target.Expression.app
      (Target.Expression.typ_app (Target.Expression.var Prelude.enum_from) (NonEmpty.mk τ []))
    e)
  e1'


set_option quotPrecheck false in
set_option hygiene false in
notation  "《pat》" ge "," ie "⊢" s "⇝" t "፥" ve "," τ "▪" => pat ge ie s t ve τ

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
    -----------------------------------------------------------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.var x ⇝ Target.Pattern.var (unqual_var x) σ ፥ [(x, Env.VE_Item.Ordinary x σ)] , τ ▪

  | PIRR :
    《pat》ge,ie ⊢ p₁ ⇝ p₂ ፥ ve, τ ▪ →
    -----------------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.lazy p₁ ⇝ Target.Pattern.lazy p₂ ፥ ve, τ ▪

  | PWILD :
    --------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.wildcard ⇝ Target.Pattern.wildcard ፥ [], τ ▪

  | PCHAR :
    -------------------------------------------------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.literal (Source.Literal.char c) ⇝ Target.Pattern.char c ፥ [],Prelude.char ▪

  | PSTRING :
    ---------------------------------------------------------------------------------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.literal (Source.Literal.string s) ⇝ Target.Pattern.string s ፥ [], Prelude.mk_list Prelude.char ▪


  | PINTEGER :
    《literal》 ie ⊢ (Source.Literal.integer i) ⇝ e ፥ τ ▪  →
    《dict》 ie ⊢ ed ፥ [⟨Prelude.eq, τ⟩] ▪ →
    -----------------------------------------------------------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.literal (Source.Literal.integer i) ⇝ Target.Pattern.exp (apply_equals τ ed e) ፥ [], τ ▪

  | PFLOAT :
    《literal》 ie ⊢ (Source.Literal.float n d) ⇝ e ፥ τ ▪ →
    《dict》 ie ⊢ ed ፥ [⟨Prelude.eq, τ⟩] ▪ →
    -----------------------------------------------------------------------------------------------------------------
    《pat》ge,ie ⊢ Source.Pattern.literal (Source.Literal.float n d) ⇝ Target.Pattern.exp (apply_equals τ ed e) ፥ [], τ ▪
