import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Target.Lang

/-
A circle with a cross inside

There is an unverified side condition:
The intersection of the domains of the two environments must be empty
-/
def cross (a b: Env.VE): Env.VE :=
  a ++ b

/-
A circle with a cross inside, with a right arrow on top

Asymmetric version of ⊕ where entries in E2 shadow E if names collide
-/
def cross_arrow (_: Env.VE) (_: Env.VE): Env.VE := sorry


def concat_target_binds (n m: Target.Binds): Target.Binds :=
  -- TODO Looks like Target.Binds isn't completely
  -- defined yet. But when it is, we should define
  -- this.
  sorry

def concat_source_binds (n m : Source.Binds): Source.Binds :=
  sorry

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
      binds ge ie ve                a_source_binds binds' ve_bindg ->
      binds ge ie (cross_arrow a b) b_source_binds binds'' ve_binds ->
      binds ge ie ve (concat_source_binds a_source_binds b_source_binds)
        (concat_target_binds binds' binds'')
        (cross ve_bindg ve_binds)

  | EMPTY_BINDS :
    binds ge ie ve Source.Binds.empty Target.Binds.empty List.nil

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
                    → List Source.Binds
                    → Target.Binds
                    → Env.VE
                    → Prop where
  | MONOBINDS :
    monobinds _ _ _ _ _ _

