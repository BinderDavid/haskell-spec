import HaskellSpec.Environments
import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Names
import HaskellSpec.Forall2


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
    -------------------------
    Entity ⟨ce, te, ⟨de₁,de₂⟩, ve⟩
           (Source.Entity.var x)
           ⟨[], ⟨[],[]⟩, ⟨[],Env.restrict de₂ [x]⟩, Env.restrict ve [x]⟩

  | TYPE_SOME :
    ⟨T, Env.TE_Item.DataType χ⟩ ∈ te₁ →
    xs ⊆ Env.fields de₂ χ →
    Ks ⊆ Env.constrs de₁ χ →
    -------------------------
    Entity ⟨ce, ⟨te₁,te₂⟩, ⟨de₁,de₂⟩, ve⟩
           (Source.Entity.type_some T xs Ks)
           ⟨[], ⟨Env.restrict te [T],[]⟩, ⟨Env.restrict de₁ Ks, Env.restrict de₂ xs⟩, Env.restrict ve xs⟩

  | TYPE_ALL :
    ⟨T, Env.TE_Item.DataType χ⟩ ∈ te₁ →
    xs = Env.fields de₂ χ →
    Ks = Env.constrs de₁ χ →
    -------------------------
    Entity ⟨ce, ⟨te₁,te₂⟩, ⟨de₁,de₂⟩, ve⟩
           (Source.Entity.type_all T)
           ⟨[], ⟨Env.restrict te [T],[]⟩, ⟨Env.restrict de₁ Ks,Env.restrict de₂ xs⟩, Env.restrict ve xs⟩

  | TYPE_SYN :
    ⟨T, Env.TE_Item.TypeSynonym χ h αs τ⟩ ∈ te₁ →
    -------------------------
    Entity ⟨ce, ⟨te₁,te₂⟩, de, ve⟩
           (Source.Entity.type_some T [] [])
           ⟨[], ⟨Env.restrict te₁ [T],[]⟩, ⟨[], []⟩, []⟩

  | CLASS_SOME :
    ⟨C, Env.CEEntry.mk Γ h x_def α ie_sup⟩ ∈ ce →
    xs ⊆ Env.ops ve Γ →
    -------------------------
    Entity ⟨ce, te, de, ve⟩
           (Source.Entity.class_some C xs)
           ⟨Env.restrict ce [C], ⟨[], []⟩, ⟨[], []⟩, Env.restrict ve xs⟩

  | CLASS_ALL :
    ⟨C, Env.CEEntry.mk Γ h x_def α ie_sup⟩ ∈ ce →
    xs = Env.ops ve Γ →
    -------------------------
    Entity ⟨ce, te, de, ve⟩
           (Source.Entity.class_all C)
           ⟨Env.restrict ce [C], ⟨[], []⟩, ⟨[], []⟩, Env.restrict ve xs⟩

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
    Forall2 ents ees (λ ent eeᵢ => Entity ee ent eeᵢ) →
    ee' = Env.ee_unions ees →
    -------------------------
    Implist M ee
            (Source.ImportList.list_some ents)
            (Env.OplusTilde.oplustilde ee' _ /- M.EE' -/)

  | HIDE_SOME :
    Forall2 ents ees (λ ent eeᵢ => Entity ee ent eeᵢ) →
    -- ⟨CE, TE, DE, VE ⟩ = EE \ (EE₁ ∪ … ∪ EEₙ) →
    -- Ks = { K | K ∈ {ent₁, …, entₙ}} →
    -- EE' = ⟨CE, TE, DE \ Ks, VE ⟩ →
    -------------------------
    Implist M ee
           (Source.ImportList.hide_some ents)
           (Env.OplusTilde.oplustilde ee' _ /- M.EE' -/)

  | ALL :
    -------------------------
    Implist M ee
            Source.ImportList.empty
            (Env.OplusTilde.oplustilde ee _ /- M.EE -/)

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
    -------------------------
    Qualifier ee Source.Qualifier.qualified (Env.JustQs.justQs ee)

  | UNQUALIFIED :
    -------------------------
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
    -------------------------
    Import me (Source.Import.mk qualifier M M' implist) ⟨ce', te', de', ie, ve'⟩ se
