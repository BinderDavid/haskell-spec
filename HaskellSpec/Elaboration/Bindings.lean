import HaskellSpec.Environments
import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang

/--
Cp. Fig. 29
```text
GE, IE, VE ⊢ binds ⇝ binds : VE
```
-/
inductive binds : Environment.GE → Environment.IE → Environment.VE
                → Source.Binds
                → Target.Binds
                → Environment.VE
                → Prop where


/--
Cp. Fig 30
```text
GE, IE, VE ⊢ sigs;bindG ⇝ binds : VE
```
-/
inductive bindG : Environment.GE → Environment.IE → Environment.VE
                → Target.Binds
                → Environment.VE
                → Prop where

/--
Cp. Fig 34
```text
GE, IE, VE ⊢ bindG ⇝ binds : VE
```
-/
inductive monobinds : Environment.GE → Environment.IE → Environment.VE
                    → Source.BindGroup
                    → Target.Binds
                    → Environment.VE
                    → Prop where

/--
Cp. Fig 34
```text
GE, IE, VE ⊢ bind ⇝ bind : VE
```
-/
inductive bind : Environment.GE → Environment.IE → Environment.VE
               → Source.Binding
               → Target.Binding
               → Environment.VE
               → Prop where
