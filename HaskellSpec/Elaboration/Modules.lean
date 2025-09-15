import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Target.Lang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes
import HaskellSpec.Elaboration.ImportExport
import HaskellSpec.Elaboration.Kinding

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
      types te h ts τs →
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
      ----------------
      types te h [] []

    | CONS :
      types te h ts τs →
      《type》te,h ⊢ t ፥ τ ▪ →
      ------------------------------
      types te h (t :: ts) (τ :: τs)
end

/--
Cp. Fig 19
```text
GE, IE, VE ⊢ ctDecl ⇝ typeDecls; binds : ⟨ CE, TE, KE, IE, VE⟩
```
-/
inductive ctdecl : Env.GE → Env.VE → Env.IE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Env.FE
                 → Prop where
  | DATA_DECL :
    -------------------------------------------------
    ctdecl ge ie ve
      (Source.ClassOrType.ct_data cx S us conDecls)
      (Target.ClassOrType.ct_data _ _ _ _)
      _

  | TYPE_DECL :
    Kinding.ktype (Env.kindsOf _ _) t κ →
    /- kindsOf() ⊢ktype t : κ -/
    《type》_ /- TE ⊕ TE₁ ⊕ … ⊕ TEₖ -/ , h ⊢ t ፥ τ ▪ →
    /- i ∈ [1,k] : TEᵢ = { uᵢ : uᵢ^κᵢ} -/

    te' = [⟨_, _⟩]/- {S : ⟨ S^… , h, Λu₁^… , uₙ^κ τ⟩ }-/ →
    -----------------------------------------------------
    ctdecl ⟨ce,te,de⟩ ie ve
      (Source.ClassOrType.ct_type S us t)
      _
      ⟨[], ⟨[], te'⟩, ⟨[],[]⟩, [], []⟩

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
      (λ ctDeclᵢ typeDeclsᵢ feᵢ =>
        ctdecl ge ve ie ctDeclᵢ typeDeclsᵢ feᵢ
      ) →
    ctdecls ge ie ve ctDecls₀ typeDecls₀ fe₀ →
    typeDecls' = Target.ClassesAndTypes.decls typeDecls typeDecls₀ →
    fe' = Env.FE_union fe (foldl Env.FE_union fes) →
    -----------------------------------------------------
    ctdecls ge ie ve
      (Source.ClassesAndTypes.decls ctDecls ctDecls₀)
      typeDecls'
      fe'

  | EMPTY_CTDECL :
    ------------------------------
    ctdecls ge ie ve
      Source.ClassesAndTypes.empty
      Target.ClassesAndTypes.empty
      ⟨[],⟨[],[]⟩,⟨[],[]⟩,[],[]⟩


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
    ctdecls ge_top ie_top ve_top ctds _ ⟨ce',te',de',ie',ve'⟩ →
    /- ge' = ⟨ce',de',te'⟩ → -/
    ge_top = _ → /- ge_top = ge_init ⊕ (⟨ce,te,de⟩ ⊕ ge') → -/
    /- ie_top = ie ⊕ ie' ⊕ ie'' → -/
    /- ve_top = ve ⊕ (ve' ⊕ ve'')-/
    fe = ⟨ce',te',de',_,_⟩ →
    se = ⟨_,_,_,_⟩ →
    -------------------------------------------------------------
    body M
      ⟨ce,te,de,ie,ve⟩
      (Source.ModuleBody.mk ctds _ _)
      (Target.ModuleBody.mk _ _ _)
      fe se

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
    body M fe_imp bod bod' fe se →
    /- i ∈ [1,k] : FE_imp ⊕ [FE]M, SE_imp ⊕ SE ⊢export entᵢ : FE'ᵢ -/
    /- FE_exp = FE'₁ ⊕ … ⊕ FE'ₖ-/
    module me
      (Source.Module.mk M ents imports bod)
      (Target.Module.mk M _)
      _

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
    Forall2 ts ts' (λ tᵢ tᵢ' => type te _ tᵢ tᵢ') →
    /- σ = ∀ α₁ … αₙ. θ' ⇒ τ₁ → … → τₙ → χ α₁ … αₖ -/
    /- DE = { J : ⟨J, χ, σ ⟩ }-/
    /- θ' = θ|τ₁…τₖ -/
    condecl te θ τ
      (Source.ConstructorDecl.poscon J ts)
      (Target.ConstructorDecl.poscon J ts'')
      de ve le θ'

  | LABCON :
    condecl te θ τ
      (Source.ConstructorDecl.labcon J _)
      (Target.ConstructorDecl.labcon J _)
      de ve le θ'

/--
Cp. Fig 22
```text
IE, φ ⊢ τ_old, UE, τ_new
```
TODO: UE and φ are still not formalized
-/
inductive lcon : Env.IE
               → SemTy.TypeS
               → SemTy.TypeS
               → Prop where
  | LCON :
    ----------
    lcon _ _ _

/--
Cp. Fig 23
```text
GE, IE, VE ⊢ ctDecl ⇝ typeDecls; binds : FE
```
-/
inductive ctDecl : Env.GE → Env.IE → Env.VE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Prop where
  | CLASS_DECL :
    ----------------
    ctDecl _ _ _ _ _

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
    -------------------------------------------------
    sig ⟨ce,te,de⟩ (Source.Signature.mk v _ _) [⟨v,_⟩]

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
    -------------
    sigs ge [] []

  | SIGS_CONS :
    sig ge s ve →
    sigs ge ss ves →
    --------------------------------------
    sigs ge (s :: ss) (List.append ve ves)

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
    classR ce te h (Source.ClassAssertion.mk C u ts) Γ τ

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
    Forall3 class_assertions Γs τs (λ classᵢ Γᵢ τᵢ => classR ce te h classᵢ Γᵢ τᵢ) →
    ---------------------------------------------------------------------------------
    context ce te h class_assertions _

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
    --------------------
    instDecl _ _ _ _ _ _


/--
Cp. Fig 26
```text
GE, IE, VE ⊢ instDecls ⇝ binds : IE
```
-/
inductive instDecls : Env.GE → Env.IE → Env.VE
                    → Source.InstanceDecls
                    → Target.InstanceDecls
                    → Env.IE
                    → Prop where
  | INST_DECLS :
    Forall3 inst_decls binds ies (λ instDeclᵢ bindᵢ ieᵢ => instDecl ge ie ve instDeclᵢ bindᵢ ieᵢ) →
    ------------------------------------------------------------------------------------------------
    instDecls ge ie ve inst_decls (Target.InstanceDecls.instDecls binds) (ies.foldl List.append [])


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
    -------------------
    method _ _ _ _ _ _

set_option quotPrecheck false in
set_option hygiene false in
notation  "《dict》" ie "⊢" e "፥" τ "▪"  => dict ie e τ

/--
Cp. Fig 28
```text
IE ⊢ e : (Γ₁ τ₁,…,Γₙ τₙ)
```
-/
inductive dict : Env.IE
               → Target.Expression
               → SemTy.Context
               → Prop where
  | DICT_TUPLE :
    ----------------
    《dict》 ie ⊢ _ ፥ _ ▪

  | DICT_VAR :
    Env.IE_Entry.BoundInDictionaryAbstraction v class_name α τs ∈ ie →
    ------------------------------------------------------------------------------------------------------------------------------------
    《dict》 ie ⊢ (Target.Expression.var (QVariable.Unqualified v)) ፥ [⟨class_name, τs.foldl SemTy.TypeS.App (SemTy.TypeS.Variable α)⟩] ▪

  | DICT_INST :
    《dict》 ie ⊢ _ ፥ _ ▪

  | DICT_SUPER :
    Env.IE_Entry.ExtractsADictionaryForTheSuperclass x α Γ Γ' ∈ ie →
    《dict》 ie ⊢ e ፥ [⟨Γ', τ⟩] ▪ →
    -----------------------------------------------------------------------------------------------------------------------
    《dict》 ie ⊢ Target.Expression.app (Target.Expression.typ_app (Target.Expression.var x) (singleton τ)) e ፥ [⟨Γ, τ⟩] ▪
