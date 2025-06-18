import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Names


/-!
# Import + Export Declarations
-/

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
  | VAR_ENT :
    Entity _ _ _

  | TYPE_SOME :
    Entity _ _ _

  | TYPE_ALL :
    Entity _ _ _

  | TYPE_SYN :
    Entity _ _ _

  | CLASS_SOME :
    Entity _ _ _

  | CLASS_ALL :
    Entity _ _ _

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
  | EXPORT_MODULE :
    Export fe se _ fe

  | EXPORT_ENTITY :
    Entity _ _ _ →
    Export fe se _ fe


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
  | LIST_SOME :
     Implist _ _ _ _

  | HIDE_SOME :
    Implist _ _ _ _

  | ALL :
    Implist _ _ _ _

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
  | QUALIFIED :
    Qualifier _ _ _

  | UNQUALIFIED :
    Qualifier _ _ _

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
  | IMPORT :
    Import _ _ _ _
