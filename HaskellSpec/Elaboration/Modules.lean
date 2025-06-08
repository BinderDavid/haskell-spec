import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes


/-!
# Modules

The rules are defined in fig. 11 of the paper.
-/


/--
Cp. Fig 11
```text
ME ⊢ mod ⇝ mod, ME
```
-/
inductive module : Environment.ME → Source.Module → Target.Module → Environment.ME → Prop where


/--
Cp. Fig 15
-/
inductive body : Module_Name → Environment.FE → Source.ModuleBody → Target.ModuleBody → Environment.FE → Environment.SE → Prop where

/--
Cp. Fig 17
-/
inductive ctdecls : Environment.GE → Environment.IE → Environment.VE → Source.ClassesAndTypes → Target.ClassesAndTypes → Environment.FE → Prop where


/--
Cp. Fig 18
-/
inductive type : Environment.TE → Source.TypeExpression → SemanticTypes.TypeS → Prop where


/--
Cp. Fig 19
-/
inductive ctdecl : Environment.GE → Environment.VE → Environment.IE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Environment.CE → Environment.TE → Environment.KE → Environment.IE → Environment.VE
                 → Prop where

/--
Cp. Fig 20
-/
inductive condecl : Environment.TE → SemanticTypes.Context → SemanticTypes.TypeS
                  → Source.ConstructorDecl
                  → Target.ConstructorDecl
                  → Environment.DE → Environment.VE → Environment.LE
                  → SemanticTypes.Context
                  → Prop where
