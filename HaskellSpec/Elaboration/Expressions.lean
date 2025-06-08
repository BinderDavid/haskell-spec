import HaskellSpec.Environments
import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang
import HaskellSpec.SemanticTypes

/--
Cp. Fig 35
```text
GE, IE, VE ⊢ match ⇝ match : τ
```
-/
inductive matchR : Environment.GE → Environment.IE → Environment.VE → Source.Match → Target.Match → SemanticTypes.TypeS → Prop where

/--
Cp. Fig 35
```text
GE, IE, VE ⊢ gdes ⇝ gdes : τ
```
-/
inductive gdes : Environment.GE → Environment.IE → Environment.VE → Source.GuardedExprs → Target.GuardedExprs → SemanticTypes.TypeS → Prop where


/--
Cp. Fig 35
```text
GE, IE, VE ⊢ gde ⇝ gde : τ
```
-/
inductive gde : Environment.GE → Environment.IE → Environment.VE → Source.GuardedExp → Target.GuardedExp → SemanticTypes.TypeS → Prop where

/--
Cp. Fig 36. 38. 39. 42
```text
GE, IE, VE ⊢ e ⇝ e : τ
```
-/
inductive exp : Environment.GE → Environment.IE → Environment.VE → Source.Expression → Target.Expression → SemanticTypes.TypeS → Prop where

/--
Cp. Fig 37
```text
IE ⊢ literal ⇝ e : τ
```
-/
inductive literal : Environment.IE → Source.Literal → Target.Expression → SemanticTypes.TypeS → Prop where

/--
Cp. Fig 40
```text
GE, IE, VE ⊢ quals ⇝ quals : VE
```
-/
inductive quals : Environment.GE → Environment.IE → Environment.VE → Source.Qualifiers → Target.Qualifiers → Environment.VE → Prop where

/--
Cp. Fig 41
```text
GE, IE, VE ⊢ stmts ⇝ e : τ
```
-/
inductive stmts : Environment.GE → Environment.IE → Environment.VE → Source.Statements → Target.Expression → SemanticTypes.TypeS → Prop where

/--
Cp. Fig 43. 44.
```
GE, IE ⊢ p ⇝ p : VE, τ
```
-/
inductive pat : Environment.GE → Environment.IE → Source.Pattern → Target.Pattern → Environment.VE → SemanticTypes.TypeS → Prop where
