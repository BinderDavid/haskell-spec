import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Target.Lang

/-
A circle with a cross inside
-/
def cross (_: Env.VE) (_: Env.VE): Env.VE := sorry

/-
A circle with a cross inside, with a right arrow on top
-/
def cross_arrow (_: Env.VE) (_: Env.VE): Env.VE := sorry

def extract_binds (bg: Source.BindGroup) : NonEmptyList Source.Binding :=
  match bg with
  | Source.BindGroup.bind_group binds => binds

def concat_bind_groups (n m: Source.BindGroup): Source.BindGroup :=
  let
    nel : NonEmptyList Source.Binding :=
      { head := (extract_binds n).head
      , tail :=  (extract_binds n).tail ++
                [(extract_binds m).head] ++
                 (extract_binds m).tail
      }
  Source.BindGroup.bind_group nel

def concat_target_binds (n m: Target.Binds): Target.Binds :=
  -- TODO Looks like Target.Binds isn't completely
  -- defined yet. But when it is, we should define
  -- this.
  sorry

def concat_source_binds (n m : Source.Binds): Source.Binds :=
  match n with
  | Source.Binds.binds_binds a_sigs a_bindgroup n_binds =>
    match m with
    | Source.Binds.binds_binds b_sigs b_bindgroup _ =>
      Source.Binds.binds_binds
        (a_sigs ++ b_sigs)
        (concat_bind_groups a_bindgroup b_bindgroup)
        n_binds -- TODO This is arbitrarily chosen.

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
