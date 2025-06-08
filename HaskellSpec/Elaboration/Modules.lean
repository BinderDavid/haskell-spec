import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang
import HaskellSpec.Environments
import HaskellSpec.SemanticTypes


/-!
# Modules

The rules are defined in fig. 11 of the paper.
-/


/--
Cp. Fig 11
```text
ME ⊢ mod ⇝ mod, ME
```
-/
inductive module : Environment.ME → Source.Module → Target.Module → Environment.ME → Prop where


/--
Cp. Fig 15
```text
M, FE ⊢ body ⇝ typeDecls;binds : FE, SE
```
-/
inductive body : Module_Name → Environment.FE → Source.ModuleBody → Target.ModuleBody → Environment.FE → Environment.SE → Prop where

/--
Cp. Fig 17
```text
GE, IE, VE ⊢ ctDecls ⇝ typeDecls; binds : FE
```
-/
inductive ctdecls : Environment.GE → Environment.IE → Environment.VE → Source.ClassesAndTypes → Target.ClassesAndTypes → Environment.FE → Prop where


/--
Cp. Fig 18
```text
TE, h ⊢ t : τ
```
-/
inductive type : Environment.TE → Int
               → Source.TypeExpression
               → SemanticTypes.TypeS → Prop where

/--
Cp. Fig 19
```text
GE, IE, VE ⊢ ctDecl ⇝ typeDecls; binds : ⟨ CE, TE, KE, IE, VE⟩
```
-/
inductive ctdecl : Environment.GE → Environment.VE → Environment.IE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Environment.CE → Environment.TE → Environment.KindEnv → Environment.IE → Environment.VE
                 → Prop where

/--
Cp. Fig 20
```text
TE, θ, τ ⊢ conDecl ⇝ conDecl : DE, VE, LE, θ
```
-/
inductive condecl : Environment.TE → SemanticTypes.Context → SemanticTypes.TypeS
                  → Source.ConstructorDecl
                  → Target.ConstructorDecl
                  → Environment.DE → Environment.VE → Environment.LE
                  → SemanticTypes.Context
                  → Prop where


/--
Cp. Fig 22
```text
IE, φ ⊢ τ_old, UE, τ_new
```
TODO: UE and φ are still not formalized
-/
inductive lcon : Environment.IE → SemanticTypes.TypeS → SemanticTypes.TypeS → Prop where


/--
Cp. Fig 23
```text
GE, IE, VE ⊢ ctDecl ⇝ typeDecls; binds : FE
```
-/
inductive ctDecl : Environment.GE → Environment.IE → Environment.VE
                 → Source.ClassOrType
                 → Target.ClassOrType
                 → Prop where

/--
Cp. Fig 24
```text
GE ⊢ sig : VE
```
-/
inductive sig : Environment.GE
              → Source.Signature
              → Environment.VE
              → Prop where

/--
Cp. Fig 24
```text
GE ⊢ sigs : VE
```
-/
inductive sigs : Environment.GE
               → Source.Signatures
               → Environment.VE
               → Prop where
  | Nil : sigs ge (Source.Signatures.sigs []) []
  | Cons : sig ge s ve
         → sigs ge (Source.Signatures.sigs ss) ves
         → sigs ge (Source.Signatures.sigs (s :: ss)) (List.append ve ves)

/--
Cp. Fig 25
```text
CE, TE, h ⊢ class : Γ τ
```
-/
inductive classR : Environment.CE → Environment.TE → Int
                 → Source.ClassAssertion
                 → SemanticTypes.Class_Name
                 → SemanticTypes.TypeS
                 → Prop where
  | classR :  (ce: Environment.CE) ->
              (te: Environment.TE) ->
              (h: Int) ->
              (className : QClassName) →
              (u: Type_Variable) →
              (ts: List Source.TypeExpression) ->
              (Γ : SemanticTypes.Class_Name) ->
              (τ : SemanticTypes.TypeS) ->
              (h' : Int) ->
              (x: Variable) ->
              (ie: Environment.IE) ->
              ((Environment.CEEntry.ceEntry Γ h' x className ie) ∈ ce) ->
              (h' < h) ->
              (h'' : Int) ->
              (type te h'' (List.foldl Source.TypeExpression.type_cons (Source.TypeExpression.type_var u) ts) τ) ->
              classR ce te h (Source.classAssert className u ts) Γ τ

/--
Cp. Fig 25
```text
CE, TE, h ⊢ cx : θ
```
-/
inductive context : Environment.CE → Environment.TE → Int
                  → Source.Context
                  → SemanticTypes.Context
                  → Prop where


/--
Cp. Fig 26
```text
GE, IE, VE ⊢ instDecls ⇝ binds : IE
```
-/
inductive instDecls : Environment.GE → Environment.IE → Environment.VE → Source.InstanceDecls → Target.InstanceDecls →Environment.IE → Prop where

/--
Cp. Fig 26
```text
GE, IE, VE ⊢ instDecl ⇝ binds : IE
```
-/
inductive instDecl : Environment.GE → Environment.IE → Environment.VE → Source.InstanceDecl → Target.InstanceDecl → Environment.IE → Prop where

/--
Cp. Fig 27
```text
GE, IE, VE ⊢ bind ⇝ fbind : VE
```
-/
inductive method : Environment.GE → Environment.IE → Environment.VE → Source.Binding → Target.FieldBinding → Environment.VE → Prop where

/--
Cp. Fig 28
```text
IE ⊢ e : (Γ₁ τ₁,…,Γₙ τₙ)
```
-/
inductive dict : Environment.IE → Source.Expression → Prop where
