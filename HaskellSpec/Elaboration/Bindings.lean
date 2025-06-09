import HaskellSpec.Environments
import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang

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
