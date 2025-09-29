import HaskellSpec.Source.Literals
import HaskellSpec.Target.Lang
import HaskellSpec.Elaboration.Dictionary
import HaskellSpec.Prelude

def fromRationalAfterRatio (n d : Int) : Target.Expression :=
  Target.Expression.app
   (Target.Expression.var Prelude.fromrational)
   (Target.Expression.app
     (Target.Expression.app
       (Target.Expression.var Prelude.ratio_percent)
       (Target.Expression.lit (Target.Literal.integer n))
     )
     (Target.Expression.lit (Target.Literal.integer d)))

def fromInteger (i : Int) : Target.Expression :=
  Target.Expression.app
    (Target.Expression.var Prelude.frominteger)
    (Target.Expression.lit (Target.Literal.integer i))

set_option quotPrecheck false in
set_option hygiene false in
notation  "《literal》" ie "⊢" l "⇝" e "፥" t "▪"=> literal ie l e t

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
    -------------------------------------------------------------------------------------------------------
    《literal》 ie ⊢ Source.Literal.char c ⇝ Target.Expression.lit (Target.Literal.char c) ፥ Prelude.char ▪

  | LIT_STRING :
    --------------------------------------------------------------------------------------------------------------------------------------------
    《literal》 ie ⊢ (Source.Literal.string s) ⇝ (Target.Expression.lit (Target.Literal.string s)) ፥ Prelude.mk_list Prelude.char ▪

  | LIT_INTEGER :
    《dict》 ie ⊢ fromInteger i ፥ [⟨Prelude.num, τ⟩] ▪ →
    ---------------------------------------------------------------
    《literal》 ie ⊢ Source.Literal.integer i ⇝ fromInteger i ፥ τ ▪

  | LIT_FLOAT :
    《dict》 ie ⊢ fromRationalAfterRatio n d ፥ [⟨Prelude.fractional, τ⟩] ▪ →
    ----------------------------------------------------------------------------
    《literal》 ie ⊢ Source.Literal.float n d ⇝ fromRationalAfterRatio n d ፥ τ ▪
