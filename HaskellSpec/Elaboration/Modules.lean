import HaskellSpec.Source.Lang
import HaskellSpec.Source.Module
import HaskellSpec.Target.Lang
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
inductive module : Env.ME
                 → Source.Module
                 → Target.Module
                 → Env.ME
                 → Prop where


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
                 → SemTy.Class_Name
                 → SemTy.TypeS
                 → Prop where
  | classR :  (ce: Env.CE) ->
              (te: Env.TE) ->
              (h: Int) ->
              (className : QClassName) →
              (u: Type_Variable) →
              (ts: List Source.TypeExpression) ->
              (Γ : SemTy.Class_Name) ->
              (τ : SemTy.TypeS) ->
              (h' : Int) ->
              (x: Variable) ->
              (ie: Env.IE) ->
              ((Env.CEEntry.ceEntry Γ h' x className ie) ∈ ce) ->
              (h' < h) ->
              (h'' : Int) ->
              (type te h'' (List.foldl Source.TypeExpression.app (Source.TypeExpression.var u) ts) τ) ->
              classR ce te h (Source.ClassAssertion.classAssert className u ts) Γ τ

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

/--
Cp. Fig 28
```text
IE ⊢ e : (Γ₁ τ₁,…,Γₙ τₙ)
```
-/
inductive dict : Env.IE
               → Target.Expression
               → List (SemTy.Class_Name × SemTy.TypeS)
               → Prop where
  | DICT_TUPLE :
    dict _ _ _

  | DICT_VAR :
    dict _ _ _

  | DICT_INST :
    dict _ _ _

  | DICT_SUPER :
    dict _ _ _
