import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.ImportExport
import HaskellSpec.Elaboration.Kinding
import HaskellSpec.Elaboration.Expressions

/-!
# Modules

The rules are defined in fig. 11 of the paper.
-/

/-- A typename `T` applied to a list of arguments:
```text
type_apply T [t₁,t₂,t₃] = T t₁ t₂ t₃
```
TODO: Reverse order!
-/
def type_apply (T : QType_Name)
               (ts : List Source.TypeExpression)
               : Source.TypeExpression :=
  match ts with
  | [] => Source.TypeExpression.typename T
  | (t :: ts) => Source.TypeExpression.app (type_apply T ts) t

set_option quotPrecheck false in
set_option hygiene false in
notation  "《type》" te "," h "⊢" t "፥"  τ "▪" => type te h t τ

set_option quotPrecheck false in
set_option hygiene false in
notation  "《types》" te "," h "⊢" ts "፥"  τs "▪" => types te h ts τs

mutual
  /--
  Cp. Fig 18
  ```text
  TE, h ⊢ t : τ
  ```
  -/
  inductive type : Env.TE → Int
                 → Source.TypeExpression
                 → SemTy.TypeS
                 → Prop where
    | TVAR :
      ⟨u, α⟩ ∈ te₂ →
      ---------------------------------------------------------------------------
      《type》⟨te₁,te₂⟩, h ⊢ Source.TypeExpression.var u ፥ SemTy.TypeS.Variable α ▪

    | TCON :
      ⟨T, Env.TE_Item.DataType χ⟩ ∈ te₁ →
      --------------------------------------------------------------------------------------
      《type》⟨te₁,te₂⟩,h ⊢ Source.TypeExpression.typename T ፥ SemTy.TypeS.TypeConstructor χ ▪

    | TSYN :
      ⟨T, Env.TE_Item.TypeSynonym χ g αs τ⟩ ∈ te₁ →
      g < h →
      《types》te, h ⊢ ts ፥ τs ▪ →
      Env.rng subst = τs →
      Env.dom subst = αs →
      ---------------------------------------------------------------------------
      《type》⟨te₁,te₂⟩,h ⊢ type_apply T ts ፥ SemTy.Substitute.substitute subst τ ▪

    | TAPP :
      《type》te,h ⊢ t₁ ፥ τ₁ ▪ →
      《type》te,h ⊢ t₂ ፥ τ₂ ▪ →
      ----------------------------------------------------------------------
      《type》te,h ⊢ Source.TypeExpression.app t₁ t₂ ፥ SemTy.TypeS.App τ₁ τ₂ ▪

  inductive types : Env.TE → Int
                  → List Source.TypeExpression
                  → List SemTy.TypeS
                  → Prop where
    | NIL :
      -------------------------
      《types》te,h ⊢ [] ፥ [] ▪

    | CONS :
      《types》te,h ⊢ ts ፥ τs ▪ →
      《type》 te,h ⊢ t  ፥ τ ▪ →
      -----------------------------------
      《types》te,h ⊢ t :: ts ፥ τ :: τs ▪
end

set_option quotPrecheck false in
set_option hygiene false in
notation  "《ctdecl》" ge "," ie "," ve "⊢" cot "⇝"  cot' "፥" ve' "▪" => ctdecl ge ie ve cot cot' ve'

/--
Cp. Fig 19 & Fig 23
```text
GE, IE, VE ⊢ ctDecl ⇝ typeDecls; binds : ⟨ CE, TE, KE, IE, VE⟩
```
-/
inductive ctdecl : Env.GE → Env.IE → Env.VE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Env.FE
                 → Prop where
  | DATA_DECL :
    -----------------------------------------------------------------------------------------------------------
    《ctdecl》ge,ie,ve ⊢ Source.ClassOrType.ct_data cx S us conDecls ⇝ Target.ClassOrType.ct_data _ _ _ _ ፥ _ ▪

  | TYPE_DECL :
    Kinding.ktype (Env.kindsOf _ _) t κ →
    /- kindsOf() ⊢ktype t : κ -/
    《type》_ /- TE ⊕ TE₁ ⊕ … ⊕ TEₖ -/ , h ⊢ t ፥ τ ▪ →
    /- i ∈ [1,k] : TEᵢ = { uᵢ : uᵢ^κᵢ} -/

    te' = [⟨_, _⟩]/- {S : ⟨ S^… , h, Λu₁^… , uₙ^κ τ⟩ }-/ →
    -----------------------------------------------------
    《ctdecl》⟨ce,te,de⟩,ie,ve ⊢ Source.ClassOrType.ct_type S us t ⇝ _ ፥ ⟨[], ⟨[], te'⟩, ⟨[],[]⟩, [], []⟩ ▪

  | CLASS_DECL :
    --------------------------------------------------------------------
    《ctdecl》ge,ie,ve ⊢ Source.ClassOrType.ct_class _ _ _ _ _ ⇝ _ ፥ _ ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《ctdecls》" ge "," ie "," ve "⊢" cot "⇝"  cot' "፥" fe "▪" => ctdecls ge ie ve cot cot' fe

/--
Cp. Fig 17
```text
GE, IE, VE ⊢ ctDecls ⇝ typeDecls; binds : FE
```
-/
inductive ctdecls : Env.GE → Env.IE → Env.VE
                  → Source.ClassesAndTypes
                  → Target.ClassesAndTypes
                  → Env.FE
                  → Prop where
  | CTDECL :
    Forall3NE ctDecls typeDecls fes
      (λ ctDeclᵢ typeDeclsᵢ feᵢ =>《ctdecl》ge,ie,ve ⊢ ctDeclᵢ ⇝ typeDeclsᵢ ፥ feᵢ ▪) →
    《ctdecls》ge,ie,ve ⊢ ctDecls₀ ⇝ typeDecls₀ ፥ fe₀ ▪ →
    typeDecls' = Target.ClassesAndTypes.decls typeDecls typeDecls₀ →
    fe' = Env.FE_union fe (foldl Env.FE_union fes) →
    ----------------------------------------------------------------------------------------
    《ctdecls》ge,ie,ve ⊢ Source.ClassesAndTypes.decls ctDecls ctDecls₀ ⇝ typeDecls' ፥ fe' ▪

  | EMPTY_CTDECL :
    --------------------------------------------------------------------------------------------------------------
    《ctdecls》ge,ie,ve ⊢ Source.ClassesAndTypes.empty ⇝ Target.ClassesAndTypes.empty ፥ ⟨[],⟨[],[]⟩,⟨[],[]⟩,[],[]⟩ ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《body》" M "," fe "⊢" sm "⇝"  tm "፥" fe' "," se "▪" => body M fe sm tm fe' se

/--
Cp. Fig 15
```text
M, FE ⊢ body ⇝ typeDecls;binds : FE, SE
```
-/
inductive body : Module_Name
               → Env.FE
               → Source.ModuleBody
               → Target.ModuleBody
               → Env.FE → Env.SE
               → Prop where
  | BODY :
    Kinding.kctDecls _ ctds _ →
    《ctdecls》ge_top,ie_top,ve_top ⊢ ctds ⇝ _ ፥ ⟨ce',te',de',ie',ve'⟩ ▪ →
    /- ge' = ⟨ce',de',te'⟩ → -/
    ge_top = _ → /- ge_top = ge_init ⊕ (⟨ce,te,de⟩ ⊕ ge') → -/
    /- ie_top = ie ⊕ ie' ⊕ ie'' → -/
    /- ve_top = ve ⊕ (ve' ⊕ ve'')-/
    fe = ⟨ce',te',de',_,_⟩ →
    se = ⟨_,_,_,_⟩ →
    -------------------------------------------------------------------------------------------------
    《body》M,⟨ce,te,de,ie,ve⟩ ⊢ Source.ModuleBody.mk ctds _ _ ⇝ Target.ModuleBody.mk _ _ _ ፥ fe, se ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《module》" me "⊢" sm "⇝"  tm "፥" me' "▪" => module me sm tm me'

/--
Cp. Fig 11
```text
ME ⊢ mod ⇝ mod, ME
```
-/
inductive module : Env.ME
                 → Source.Module
                 → Target.Module
                 → Env.ME
                 → Prop where
  | MODULE :
    Forall3 imports fes ses (λ impᵢ feᵢ seᵢ => Import me impᵢ feᵢ seᵢ) →
    fe_imp = _ → /- justSingle(FE₁ ⊕ … ⊕ FEₙ)-/
    /- SE_imp = SE₁ ⊕ … ⊕ SEₙ-/
    《body》M,fe_imp ⊢ bod ⇝ bod' ፥ fe,se ▪ →
    /- i ∈ [1,k] : FE_imp ⊕ [FE]M, SE_imp ⊕ SE ⊢export entᵢ : FE'ᵢ -/
    /- FE_exp = FE'₁ ⊕ … ⊕ FE'ₖ-/
    -------------------------------------------------------------------------------
    《module》me ⊢ Source.Module.mk M ents imports bod ⇝ Target.Module.mk M _ ፥ _ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《condecl》" te "," θ "," τ "⊢" cd "⇝" cd' "፥" de "," ve "," le "," θ' "▪" => condecl te θ τ cd cd' de ve le θ'

/--
Cp. Fig 20
```text
TE, θ, τ ⊢ conDecl ⇝ conDecl : DE, VE, LE, θ
```
-/
inductive condecl : Env.TE
                  → SemTy.Context
                  → SemTy.TypeS
                  → Source.ConstructorDecl
                  → Target.ConstructorDecl
                  → Env.DE → Env.VE → Env.LE
                  → SemTy.Context
                  → Prop where
  | POSCON :
    Forall2 ts ts' (λ tᵢ tᵢ' => 《type》te,_ ⊢ tᵢ ፥ tᵢ' ▪) →
    /- σ = ∀ α₁ … αₙ. θ' ⇒ τ₁ → … → τₙ → χ α₁ … αₖ -/
    /- DE = { J : ⟨J, χ, σ ⟩ }-/
    /- θ' = θ|τ₁…τₖ -/
    -------------------------------------------------------------------------------------------------------------
    《condecl》te,θ,τ ⊢ Source.ConstructorDecl.poscon J ts ⇝ Target.ConstructorDecl.poscon J ts'' ፥ de,ve,le,θ' ▪

  | LABCON :
    ---------------------------------------------------------------------------------------------------------
    《condecl》te,θ,τ ⊢ Source.ConstructorDecl.labcon J _ ⇝ Target.ConstructorDecl.labcon J _ ፥ de,ve,le,θ' ▪



set_option quotPrecheck false in
set_option hygiene false in
notation  "《lcon》" ie "," φ "⊢" τ_old "," ue "," τ_new "▪" => lcon ie φ τ_old ue τ_new

/--
Cp. Fig 22
```text
IE, φ ⊢ τ_old, UE, τ_new
```
-/
inductive lcon : Env.IE
               → Env.LabelInfo
               → SemTy.TypeS
               → Env.UE
               → SemTy.TypeS
               → Prop where
  | LCON :
    《dict》ie ⊢ _ ፥ _ ▪ →
    《dict》ie ⊢ _ ፥ _ ▪ →
    ------------------------------------------------------
    《lcon》ie , Env.LabelInfo.mk αs θ ue τ ⊢ _ , ue , _ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《sig》" ge "⊢" sign "፥" ve "▪" => sig ge sign ve

/--
Cp. Fig 24
```text
GE ⊢ sig : VE
```
-/
inductive sig : Env.GE
              → Source.Signature
              → Env.VE
              → Prop where
  | SIG :
    ke = Env.kindsOf ce te →
    《type》_,_ ⊢ _ ፥ _ ▪ →
    ---------------------------------------------------------
    《sig》⟨ce,te,de⟩ ⊢ (Source.Signature.mk v _ _) ፥ [⟨v,_⟩] ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《sigs》" ge "⊢" sign "፥" ve "▪" => sigs ge sign ve

/--
Cp. Fig 24
```text
GE ⊢ sigs : VE
```
-/
inductive sigs : Env.GE
               → List Source.Signature
               → Env.VE
               → Prop where
  | SIGS_NIL :
    ----------------------
    《sigs》ge ⊢ [] ፥ [] ▪

  | SIGS_CONS :
    《sig》 ge ⊢ s  ፥ ve ▪ →
    《sigs》ge ⊢ ss ፥ ves ▪ →
    -------------------------------------------
    《sigs》ge ⊢ s :: ss ፥ List.append ve ves ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《class》" ce "," te "," h "⊢" cls "፥" Γ "," τ "▪" => classR ce te h cls Γ τ

/--
Cp. Fig 25
```text
CE, TE, h ⊢ class : Γ τ
```
-/
inductive classR : Env.CE → Env.TE → Int
                 → Source.ClassAssertion
                 → SemTy.SClass_Name
                 → SemTy.TypeS
                 → Prop where
  | CLASS :
    (_, Env.CEEntry.mk Γ h' x C ie) ∈ ce →
    h' < h →
    《type》te, h'' ⊢ List.foldl Source.TypeExpression.app (Source.TypeExpression.var u) ts ፥ τ ▪ →
    ------------------------------------------------------------------------------------------------
    《class》ce,te,h ⊢ Source.ClassAssertion.mk C u ts ፥ Γ , τ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《context》" ce "," te "," h "⊢" cx "፥" θ "▪" => context ce te h cx θ

/--
Cp. Fig 25
```text
CE, TE, h ⊢ cx : θ
```
-/
inductive context : Env.CE → Env.TE → Int
                  → Source.Context
                  → SemTy.Context
                  → Prop where
  | CONTEXT :
    Forall3 class_assertions Γs τs (λ classᵢ Γᵢ τᵢ => 《class》ce,te,h ⊢ classᵢ ፥ Γᵢ ,τᵢ ▪) →
    -----------------------------------------------------------------------------------------
    《context》ce,te,h ⊢ class_assertions ፥ _ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《instdecl》" ge "," ie "," ve "⊢" idecl "⇝" idecl' "፥" ie' "▪" => instDecl ge ie ve idecl idecl' ie'

/--
Cp. Fig 26
```text
GE, IE, VE ⊢ instDecl ⇝ binds : IE
```
-/
inductive instDecl : Env.GE → Env.IE → Env.VE
                   → Source.InstanceDecl
                   → Target.InstanceDecl
                   → Env.IE
                   → Prop where
  | INST_DECL :
    ⟨T, χ⟩ ∈ te₁ →
    /- i ∈ [1,k] : αᵢ = uᵢ^kᵢ-/
    ⟨_, Env.CEEntry.mk Γ h x_def α ie_sup⟩ ∈ ce →
    《context》ce,_,_ ⊢ cx ፥ θ ▪ →
    /- i ∈ [1,m] : 《method》ge;_,ve ⊢ bindᵢ ⇝ fbindᵢ ፥ ve_i -/
    -----------------------------------------------------------------
    《instdecl》⟨ce,⟨te₁,te₂⟩,de⟩,ie,ve ⊢ Source.InstanceDecl.mk cx C _ bs ⇝ _ ፥ _ ▪

set_option quotPrecheck false in
set_option hygiene false in
notation  "《instdecls》" ge "," ie "," ve "⊢" idecls "⇝" idecls' "፥" ie' "▪" => instDecls ge ie ve idecls idecls' ie'

/--
Cp. Fig 26
```text
GE, IE, VE ⊢ instDecls ⇝ binds : IE
```
-/
inductive instDecls : Env.GE → Env.IE → Env.VE
                    → Source.InstanceDecls
                    → List Target.InstanceDecl
                    → Env.IE
                    → Prop where
  | INST_DECLS :
    Forall3 inst_decls bs ies (λ instDeclᵢ bindᵢ ieᵢ => 《instdecl》ge,ie,ve ⊢ instDeclᵢ ⇝ bindᵢ ፥ ieᵢ ▪) →
    ----------------------------------------------------------------------------------------------------------
    《instdecls》ge,ie,ve ⊢ inst_decls ⇝ bs ፥ (ies.foldl List.append []) ▪


set_option quotPrecheck false in
set_option hygiene false in
notation  "《method》" ge "," ie "," ve "⊢" sb "⇝" fb "፥" ve' "▪" => method ge ie ve sb fb ve'

/--
Cp. Fig 27
```text
GE, IE, VE ⊢ bind ⇝ fbind : VE
```
-/
inductive method : Env.GE → Env.IE → Env.VE
                 → Source.Binding
                 → Target.FieldBinding
                 → Env.VE
                 → Prop where
  | METHOD :
    《bind》ge,_,ve ⊢ bnd ⇝ bnd' ፥ [⟨x,_⟩] ▪ →
    ----------------------------------------------------------------------
    《method》ge, ie,ve ⊢ bnd ⇝ Target.FieldBinding.fb_bind _ _ ፥ [⟨x,_⟩] ▪
