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



def concat_target_binds (n m: Target.Binds): Target.Binds :=
  -- TODO Looks like Target.Binds isn't completely
  -- defined yet. But when it is, we should define
  -- this.
  sorry


set_option quotPrecheck false in
set_option hygiene false in
notation  "《bindG》" ge "," ie "," ve "⊢" sgs ";" bs "⇝" bs' "፥" ve' "▪" => bindG ge ie ve sgs bs bs' ve'

/--
Cp. Fig 30
```text
GE, IE, VE ⊢ sigs;bindG ⇝ binds : VE
```
-/
inductive bindG : Env.GE → Env.IE → Env.VE
                → List Source.Signature
                → List Source.Binds
                → Target.Binds
                → Env.VE
                → Prop where
  | BINDG :
    -------------------------------
    《bindG》_,_,_ ⊢ _ ; _ ⇝ _ ፥ _ ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《binds》" ge "," ie "," ve "⊢" bs "⇝" bs' "፥" ve' "▪" => binds ge ie ve bs bs' ve'

/--
Cp. Fig. 29
```text
GE, IE, VE ⊢ binds ⇝ binds : VE
```xs
-/
inductive binds : Env.GE → Env.IE → Env.VE
                → Source.Binds
                → Target.Binds
                → Env.VE
                → Prop where
  | BINDS :
    《bindG》ge,ie, ve                         ⊢ sgs ; bnds ⇝ binds' ፥ ve_bindg ▪ →
    《binds》ge,ie, Env.oplusarrow ve ve_bindg ⊢ bnds' ⇝ binds'' ፥ ve_binds ▪ →
    ----------------------------xs----------------------------------------------------------------------------------------------------------
    《binds》ge,ie,ve ⊢ Source.Binds.cons sgs bnds bnds' ⇝ concat_target_binds binds' binds'' ፥ cross ve_bindg ve_binds ▪

  | EMPTY_BINDS :
    ------------------------------------------------------------------
    《binds》ge,ie,ve ⊢ Source.Binds.empty ⇝ Target.Binds.non_recursive [] ፥ [] ▪
