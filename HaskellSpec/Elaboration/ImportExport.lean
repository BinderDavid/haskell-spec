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
inductive Export : Environment.FE → Environment.SE → Source.Entity → Environment.FE → Prop where

/--
Cp. Fig 13
```text
ME ⊢ imp, FE, SE
```
-/
inductive Import : Environment.ME → Source.Import → Environment.FE → Environment.SE → Prop where

/--
Cp. Fig 13
```text
M, EE ⊢ implist, EE
```
-/
inductive Implist : Module_Name → Environment.EE → Source.ImportList → Environment.EE → Prop where

/--
Cp. Fig 13
```text
EE ⊢ qualifier, EE
```
-/
inductive Qualifier : Environment.EE → Source.Qualifier → Environment.EE → Prop where

/--
Cp. Fig 14
```text
EE ⊢ ent, EE
```
-/
inductive Entity : Environment.EE → Source.Entity → Environment.EE → Prop where
