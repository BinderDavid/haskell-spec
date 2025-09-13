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
    ctdecls _ _ _ _ _ _

  | EMPTY_CTDECL :
    ctdecls _ _ _ _ _ _


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
    type ⟨te₁,te₂⟩ h (Source.TypeExpression.var u) (SemTy.TypeS.Variable α)

  | TCON :
    ⟨T, Env.TE_Item.DataType χ⟩ ∈ te₁ →
    type ⟨te₁,te₂⟩ h (Source.TypeExpression.typename T) (SemTy.TypeS.TypeConstructor χ)

  | TSYN :
    ⟨T, Env.TE_Item.TypeSynonym χ g αs τ⟩ ∈ te₁ →
    g < h →
    type ⟨te₁,te₂⟩ h _ _

  | TAPP :
    type te h t₁ τ₁ →
    type te h t₂ τ₂ →
    type te h (Source.TypeExpression.app t₁ t₂) (SemTy.TypeS.App τ₁ τ₂)



/--
Cp. Fig 19
```text
GE, IE, VE ⊢ ctDecl ⇝ typeDecls; binds : ⟨ CE, TE, KE, IE, VE⟩
```
-/
inductive ctdecl : Env.GE → Env.VE → Env.IE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Env.CE → Env.TE → Env.KE → Env.IE → Env.VE
                 → Prop where
  | DATA_DECL :
    ctdecl _ _ _ _ _ _ _ _ _ _

  | TYPE_DECL :
    ctdecl _ _ _ _ _ _ _ _ _ _

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
    sig _ _ _

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
  | Nil : sigs ge [] []
  | Cons : sig ge s ve
         → sigs ge ss ves
         → sigs ge (s :: ss) (List.append ve ves)

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
    (_, Env.CEEntry.mk Γ h' x C ie) ∈ ce ->
    h' < h ->
    type te h'' (List.foldl Source.TypeExpression.app (Source.TypeExpression.var u) ts) τ ->
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
    context ce te h class_assertions _


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
    instDecls _ _ _ _ _ _

/--
Cp. Fig 26
```text
GE, IE, VE ⊢ instDecl ⇝ binds : IE
```
-/
inductive instDecl : Env.GE → Env.IE → Env.VE
                   → Source.InstanceDecl
                   → Target.InstanceDecl
                   → EnvIE
                   → Prop where
  | INST_DECL :
    instDecl _ _ _ _ _ _


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
    method _ _ _ _ _ _

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
    dict _ _ _

  | DICT_VAR :
    dict _ _ _

  | DICT_INST :
    dict _ _ _

  | DICT_SUPER :
    dict _ _ _
