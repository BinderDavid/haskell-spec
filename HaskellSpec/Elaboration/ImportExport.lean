import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
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
inductive Entity : Env.EE → Source.Entity → Env.EE → Prop where
  | Var_Ent : (ve : Env.VE) → (x : QVariable) → (x ∈ Env.dom ve)
    → (ce : Env.CE)
    → (te : Env.TE)
    → (de : Env.DE)
    → (ve : Env.VE)
    → Entity ⟨ce, te, de, ve⟩ (Source.Entity.var x) ⟨_, _, _, _⟩ --- TODO continue here
  | Type_Some : Entity _ _ _
  | Type_All : Entity _ _ _
  | Type_Syn : Entity _ _ _
  | Class_Some : Entity _ _ _
  | Class_All : Entity _ _ _
