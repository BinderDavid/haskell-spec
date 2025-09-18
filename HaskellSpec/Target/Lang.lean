import HaskellSpec.Names
import HaskellSpec.NonEmpty
import HaskellSpec.SemanticTypes
/-!
Figure 6
-/

namespace Target
/--
From Fig. 3:
```
literal ∈ Literal → char
                  | string
                  | integer
                  | float
```
-/
inductive Literal : Type where
  | char : Char → Literal
  | string : String → Literal
  | integer : Int → Literal


/--
```text
t ∈ Type expression → u
                    | T
                    | t₁ t₂
```
-/
inductive TypeExpression : Type where
  | var      : Type_Variable → TypeExpression
  | typename : Type_Name → TypeExpression
  | app      : TypeExpression → TypeExpression → TypeExpression

/--
```text
class ∈ ClassAssertion → C (u t₁ … tₖ)   k ≥ 0
```
-/
structure ClassAssertion : Type where
  name : Class_Name
  var : Type_Variable
  args : List TypeExpression

/--
```text
cx ∈ Context → (class₁,...,classₖ)
                k ≥ 0
```
-/
def Context : Type := List ClassAssertion

mutual
  /--
  ```text
    p ∈ Pattern → v : σ
                | K p₁ … pₙ      k ≥ 0
                | K {fp₁ … fpₙ}  k ≥ 0
                | v : σ @ p
                | ~p
                | _
                | { e }
                | v : σ {e1, e2}
    fp ∈ FieldPattern → x = p
  ```
  -/
  inductive Pattern : Type where
    | var : Variable → SemTy.TypeScheme →  Pattern
    | constr_pat : QConstructor → List Pattern → Pattern
    | constr_fieldPat : QConstructor → List (Variable × Pattern) → Pattern
    | at : Variable → SemTy.TypeScheme → Pattern → Pattern
    | lazy : Pattern → Pattern
    | wildcard : Pattern
    | exp : Expression → Pattern
    | char : Char → Pattern -- Seems to be omitted in Faxen?
    | string : String → Pattern -- Seems to be omitted in Faxen?

  /--
  ```text
    binds ∈ Bindings → bind₁; ... ; bindₙ n > 0
                     | rec bind₁; ... ; bind n
  ```
  -/
  inductive Binds : Type where
    | recursive : List Binding → Binds
    | non_recursive : List Binding → Binds

  /--
  ```text
    bind ∈ Binding → x match₁ [] … [] matchₙ    n ≥ 1
                   | p gdes
  ```
  -/
  inductive Binding : Type where
    | bind_match : QVariable → NonEmpty Match → Binding
    | bind_pat : Pattern → GuardedExprs → Binding

  /--
  ```text
    match ∈ Match → p₁ … pₖ gdes    k ≥ 1
  ```
  -/
  structure Match : Type where
    patterns : NonEmpty Pattern
    gdes : GuardedExprs

  /--
  ```text
    gdes ∈ GuardedExprs → gde₁ … gdeₙ where binds   n ≥ 1
  ```
  -/
  structure GuardedExprs : Type where
    gdes : NonEmpty GuardedExp
    binds : Binds


  /--
  ```text
    gde ∈ GuardedExpr → | e₁ = e₂
  ```
  -/
  structure GuardedExp : Type where
    guard : Expression
    body : Expression

  /--
  ```text
    e ∈ Expression → x
                   | literal
                   | K
                   | λ p₁ … pₖ → e                     k ≥ 1
                   | e₁ e₂
                   | let binds in e
                   | case e of match₁ [] … [] matchₙ   n ≥ 1
                   | [e | quals]
                   | e ⇐ {fbind₁, …, fbindₖ}           k ≥ 0
                   | e {fbind₁, …, fbindₖ}             k ≥ 0
                   | e τ₁ … τₖ                         k ≥ 1
                   | Λ α₁ … αₖ.e                       k ≥ 1
  ```
  -/
  inductive Expression : Type where
    | var : QVariable → Expression
    | lit : Literal → Expression
    | constr : QConstructor → Expression
    | abs : NonEmpty Pattern → Expression → Expression
    | app : Expression → Expression → Expression
    | let_bind : Binds → Expression → Expression
    | case : Expression → NonEmpty Match → Expression
    | listComp : Expression → Qualifiers → Expression
    | recUpd : Expression → List FieldBinding → Expression
    | recConstr : Expression → List FieldBinding → Expression
    | typ_app : Expression → NonEmpty SemTy.TypeS → Expression
    | typ_abs : NonEmpty Type_Variable → Expression → Expression

  /--
  ```text
  stmts ∈ Statements → p <- e; stmts
                     | let binds; stmts
                     | e; stmts
                     | e
  ```
  -/
  inductive Statements : Type where
      /--
      ```text
      p <- e; stmts
      ```
      -/
    | mbind : Pattern → Expression → Statements → Statements
      /--
      ```text
      let binds; stmts
      ```
      -/
    | lbind : Binds → Statements → Statements
      /--
      ```text
      e; stmts
      ```
      -/
    | seq : Expression → Statements → Statements
    | last : Expression → Statements

  /--
  ```text
  quals ∈ Qualifiers → p <- e, quals
                     | let binds, quals
                     | e, quals
                     | ε
  ```
  -/
  inductive Qualifiers : Type where
    | list_bind : Pattern → Expression → Qualifiers → Qualifiers
    | lbind : Binds → Qualifiers → Qualifiers
    | guard : Expression → Qualifiers → Qualifiers
    | empty : Qualifiers

  /--
  ```text
  fbind ∈ FieldBinding → x = e
  ```
  -/
  inductive FieldBinding : Type where
    | fb_bind : QVariable → Expression → FieldBinding
end

-- A helper to handle the case of an empty list in a typ_app
def typ_app_ (e : Expression) (ts : List SemTy.TypeS) : Expression :=
  (Option.elim (fromList ts) e (Target.Expression.typ_app e))

/--
```text
instDecl ∈ InstanceDecl → instance cx => C t where bind₁; …; bindₙ    n ≥ 0
```
-/
inductive InstanceDecl : Type where
  | instDecl : Context → Class_Name → Type_Expression → List Binding → InstanceDecl

/--
```text
instDecls ∈ InstanceDecls → instDecl₁; …; instDeclₙ   n ≥ 0
```
-/
inductive InstanceDecls : Type where
  | instDecls : List InstanceDecl → InstanceDecls

/--
```text
conDecl ∈ ConstructorDecl → J t₁ … tₙ                 k ≥ 0
                          | J { v₁ ∷ t₁ … vₙ ∷ tₙ }   k ≥ 0
```
-/
inductive ConstructorDecl : Type where
  | poscon: Constructor → List TypeExpression → ConstructorDecl
  | labcon: Constructor → List (QVariable × TypeExpression) → ConstructorDecl

/--
```text
conDecls ∈ ConstructorDecls → conDecl₁ | … | conDeclₙ   n ≥ 1
```
-/
inductive ConstructorDecls : Type where
    | conDecls : NonEmpty ConstructorDecl → ConstructorDecls

/--
```text
sig ∈ Signature → v :: cx => t
```
-/
inductive Signature : Type where
  | sig : QVariable → Context → TypeExpression → Signature

/--
```text
sigs ∈ Signatures → sig₁; …; sigₙ   n ≥ 0
```
-/
inductive Signatures : Type where
  | sigs : List Signature → Signatures


/--
```text
ctDecl ∈ Class or type → type S u₁ ... uₖ = t                              k ≥ 0
                       | data cx => S u₁ ... uₖ = conDecls                 k ≥ 0
                       | class cx => B u where sigs; bind₁; ...; bindₙ     k ≥ 0
```
-/
inductive ClassOrType : Type where
  | ct_type :
      Type_Name
    → List Type_Variable
    → TypeExpression
    → ClassOrType
  | ct_data :
      Context
    → Type_Name
    → List Type_Variable
    → ConstructorDecls
    → ClassOrType
  | ct_class :
      Context
    → Class_Name
    → Type_Variable
    → Signatures
    → List Binding
    → ClassOrType

/--
```text
ctDecls ∈ Classes and types → [ctDecl₁;...;ctDeclₙ then ctDecls]
                              n ≥ 1
```
-/
inductive ClassesAndTypes : Type where
  | empty
  | decls : NonEmpty ClassOrType → ClassesAndTypes → ClassesAndTypes

/--
```text
body ∈ Module body → ctDecls; instDecls; binds
```
-/
structure ModuleBody : Type where
  ctDecls : ClassesAndTypes
  instDecls : InstanceDecls
  binds : Binds

/--
```text
typeDecl ∈ TypeDeclaration → data χ α₁ … αₖ = conDecl₁ | … | conDeclsₙ    k ≥ 0
                                                                          n ≥ 1
```
-/
inductive TypeDeclaration : Type where
  | typeDecl :
      -- Chi ->
      -- List Alphas ->
      NonEmpty ConstructorDecl ->
      TypeDeclaration

/--
```text
typeDecls ∈ TypeDeclarations → typeDecl₁; …; typeDeclₙ    n ≥ 0
```
-/
inductive TypeDeclarations : Type where
  | typeDecls : List TypeDeclaration -> TypeDeclarations

/--
```text
mod ∈ Module → module M where typeDecls; binds
```
-/
structure Module : Type where
  name : Module_Name -- Use QModule:Name in the future?
  decls : List TypeDeclarations

end Target
