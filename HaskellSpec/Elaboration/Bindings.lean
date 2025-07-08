import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Target.Lang

/--
Cp. Fig. 29
```text
GE, IE, VE ⊢ binds ⇝ binds : VE
```
-/
inductive binds : Env.GE → Env.IE → Env.VE
                → Source.Binds
                → Target.Binds
                → Env.VE
                → Prop where
  | BINDS :
    binds _ _ _ _ _ _

  | EMPTY_BINDS :
    binds _ _ _ _ _ _

/--
Cp. Fig 30
```text
GE, IE, VE ⊢ sigs;bindG ⇝ binds : VE
```
-/
inductive bindG : Env.GE → Env.IE → Env.VE
                → Target.Binds
                → Env.VE
                → Prop where
  | BINDG :
    bindG _ _ _ _ _

/--
Cp. Fig 34
```text
GE, IE, VE ⊢ bindG ⇝ binds : VE
```
-/
inductive monobinds : Env.GE → Env.IE → Env.VE
                    → Source.BindGroup
                    → Target.Binds
                    → Env.VE
                    → Prop where
  | MONOBINDS :
    monobinds _ _ _ _ _ _

/--
Cp. Fig 34
```text
GE, IE, VE ⊢ bind ⇝ bind : VE
```
-/
inductive bind : Env.GE → Env.IE → Env.VE
               → Source.Binding
               → Target.Binding
               → Env.VE
               → Prop where
  | FUNBIND :
    bind _ _ _ _ _ _

  | PATBIND :
    bind _ _ _ _ _ _
