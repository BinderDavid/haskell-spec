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
    x ∈ Env.dom ve →
    Entity ⟨ce, te, de, ve⟩ (Source.Entity.var x) ⟨[], ⟨[],[]⟩, _, _⟩

  | TYPE_SOME :
    -- T : χ \in te →
    -- xs ⊆ fields(de,χ) →
    -- Ks ⊆ constrs(de,χ) →
    Entity ⟨ce, te, de, ve⟩ (Source.Entity.type_some T xs Ks) ⟨[], _, _, _⟩

  | TYPE_ALL :
    -- T : χ \in te →
    -- xs = fields(de,χ) →
    -- Ks = constrs(de,χ) →
    Entity ⟨ce, te, de, ve⟩ (Source.Entity.type_all T) ⟨[], _, _, _⟩

  | TYPE_SYN :
    -- T : ⟨χ, h, Λα₁ … αₙ τ ⟩ ∈ te →
    Entity ⟨ce, te, de, ve⟩ _ ⟨[], _, ⟨[], []⟩, []⟩

  | CLASS_SOME :
    -- C : ⟨Γ, h, x_def, α, IE_sup⟩ ∈ ce →
    -- xs ⊆ ops(ve, Γ) →
    Entity ⟨ce, te, de, ve⟩ (Source.Entity.class_some C xs) ⟨_, ⟨[], []⟩, ⟨[], []⟩, _⟩

  | CLASS_ALL :
    -- C : ⟨Γ, h, x_def, α, IE_sup⟩ ∈ ce →
    -- xs = ops(ve, Γ) →
    Entity ⟨ce, te, de, ve⟩ (Source.Entity.class_all C) ⟨_, ⟨[], []⟩, ⟨[], []⟩, _⟩

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
    -- Cs = {C | C : M ∈ CS} →
    -- Ts = {T | T : M ∈ TS} →
    -- Ks = {K | K : M ∈ KS} →
    -- xs = {x | x : M ∈ VS} ∪ {x | x : M ∈ DS} →
    -- fe = … →
    Export ⟨ce, te, de, ie, ve⟩ _ (Source.Entity.module m) fe

  | EXPORT_ENTITY :
    Entity ⟨ce, te, de, ve⟩ ent ⟨ce', te', de', ve'⟩ →
    Export ⟨ce, te, de, ie, ve⟩ se ent _


/--
Cp. Fig 13
```text
M, EE ⊢ implist, EE
```
-/
inductive Implist : Module_Name
                  → Env.EE
                  → Source.ImportList
                  → Env.EE
                  → Prop where
  | LIST_SOME :
    Implist M ee (Source.ImportList.list_some ents) _

  | HIDE_SOME :
    Implist M ee (Source.ImportList.hide_some ents) _

  | ALL :
    Implist M ee Source.ImportList.empty _

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
    Qualifier ee Source.Qualifier.qualified _

  | UNQUALIFIED :
    Qualifier ee Source.Qualifier.unqualified ee

/--
Cp. Fig 13
```text
ME ⊢ imp, FE, SE
```
-/
inductive Import : Env.ME
                 → Source.Import
                 → Env.FE
                 → Env.SE
                 → Prop where
  | IMPORT :
    -- M : ⟨ CE, TE, DE, IE, VE ⟩ ∈ ME →
    Implist M' ⟨ CE, TE, DE, VE ⟩ implist ee →
    Qualifier ee qualifier ⟨ce', te', de', ve'⟩ →
    -- SE = ⟨ dome(CE'), dom(TE'), dom(DE'), dom(VE') ⟩ : M →
    Import me (Source.Import.mk qualifier M M' implist) ⟨ce', te', de', ie, ve'⟩ se
