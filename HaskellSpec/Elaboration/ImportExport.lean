import HaskellSpec.Environments
import HaskellSpec.Source.SourceLang
import HaskellSpec.Names


/-!
# Import + Export Declarations
-/
/--
Cp. Fig 12
```text
FE, SE ⊢ ent : FE
```
-/
inductive Export : Env.FE → Env.SE
                 → Source.Entity
                 → Env.FE
                 → Prop where

/--
Cp. Fig 13
```text
ME ⊢ imp, FE, SE
```
-/
inductive Import : Env.ME
                 → Source.Import
                 → Env.FE → Env.SE
                 → Prop where

/--
Cp. Fig 13
```text
M, EE ⊢ implist, EE
```
-/
inductive Implist : Module_Name → Env.EE
                  → Source.ImportList
                  → Env.EE
                  → Prop where

/--
Cp. Fig 13
```text
EE ⊢ qualifier, EE
```
-/
inductive Qualifier : Env.EE
                    → Source.Qualifier
                    → Env.EE
                    → Prop where

/--
Cp. Fig 14
```text
EE ⊢ ent, EE
```
-/
inductive Entity : Env.EE
                 → Source.Entity
                 → Env.EE
                 → Prop where
