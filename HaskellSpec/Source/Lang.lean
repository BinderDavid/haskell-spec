import HaskellSpec.Names
import HaskellSpec.NonEmptyList
import HaskellSpec.Source.Patterns
import HaskellSpec.Source.Literals

/-!
Figure 1 and 2
-/

namespace Source

/--
```text
t ∈ Type expression → u
                    | T
                    | t₁ t₂
```
--/
inductive TypeExpression : Type where
  | var      : Type_Variable → TypeExpression
  | typename : Type_Name → TypeExpression
  | app      : TypeExpression → TypeExpression → TypeExpression

/--
```text
class ∈ ClassAssertion → C (u t₁ … tₖ)   k ≥ 0
```
-/
inductive ClassAssertion : Type where
  | classAssert : QClassName → Type_Variable → List TypeExpression → ClassAssertion

/--
```text
cx ∈ Context → (class₁,...,classₖ)
                k ≥ 0
```
--/
inductive Context : Type where
  | cx : List ClassAssertion → Context

/--
```text
sig  ∈ Signature  → v :: cx => t
sigs ∈ Signatures → sig₁; …; sigₙ   n ≥ 0
```
-/
inductive Signature : Type where
  | sig : QVariable → Context → TypeExpression → Signature

/--
```text
conDecl ∈ ConstructorDecl → J t₁ … tₙ                 k ≥ 0
                          | J { v₁ ∷ t₁ … vₙ ∷ tₙ }   k ≥ 0
```
-/
inductive ConstructorDecl : Type where
  | conDecl_simple: Constructor → List TypeExpression → ConstructorDecl
  | conDecl_record: Constructor → List (QVariable × TypeExpression) → ConstructorDecl

/--
```text
conDecls ∈ ConstructorDecls → conDecl₁ | … | conDeclₙ   n ≥ 1
```
-/
inductive ConstructorDecls : Type where
  | conDecls : NonEmptyList ConstructorDecl → ConstructorDecls

mutual
  /--
  ```text
    binds ∈ Binds → [ sigs; bindG then binds]
  ```
  --/
  inductive Binds : Type where
    | binds_binds : List Signature → BindGroup → Binds → Binds

  /--
  ```text
    bindG ∈ BindGroup → bind₁; …; bindₙ   n ≥ 1
  ```
  --/
  inductive BindGroup : Type where
    | bind_group : NonEmptyList Binding → BindGroup

  /--
  ```text
    bind ∈ Binding → x match₁ [] … [] matchₙ    n ≥ 1
                   | p gdes
  ```
  --/
  inductive Binding : Type where
    | bind_match : QVariable → NonEmptyList Match → Binding
    | bind_pat : Pattern → GuardedExprs → Binding

  /--
  ```text
    match ∈ Match → p₁ … pₖ gdes    k ≥ 1
  ```
  --/
  inductive Match : Type where
    | match_match : NonEmptyList Pattern → GuardedExprs → Match

  /--
  ```text
    gdes ∈ GuardedExprs → gde₁ … gdeₙ where binds   n ≥ 1
  ```
  --/
  inductive GuardedExprs : Type where
    | gExp_where : NonEmptyList GuardedExp → Binds → GuardedExprs

  /--
  ```text
    gde ∈ GuardedExpr → | e₁ = e₂
  ```
  --/
  inductive GuardedExp : Type where
    | gExp_eq : Expression → Expression → GuardedExp

  /--
  ```text
    e ∈ Expression       → x
                         | literal
                         | K
                         | \p₁ … pₖ → e                     k ≥ 1
                         | e₁ e₂
                         | let binds in e
                         | case e of match₁ [] … [] matchₙ   n ≥ 1
                         | do stmts
                         | [e | quals]
                         | [e₁ [,e₂] .. [e₃]]
                         | e ⇐ {fbind₁, …, fbindₖ}           k ≥ 0
                         | K {fbind₁, …, fbindₖ}             k ≥ 0
    fbind ∈ FieldBinding → x = e
  ```
  --/
  inductive Expression : Type where
    | var : QVariable → Expression
    | lit : Literal → Expression
    | constr : QConstructor → Expression
    | abs : NonEmptyList Pattern → Expression → Expression
    | app : Expression → Expression → Expression
    | let_bind : Binds → Expression → Expression
    | case : Expression → NonEmptyList Match → Expression
    | do_block : Statements → Expression
    | listComp : Expression → Qualifiers → Expression
    | listRange : Expression → Option Expression → Option Expression → Expression
    | recUpd : Expression → List (QVariable × Expression) → Expression
    | recConstr : QConstructor → List (QVariable × Expression) → Expression

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
end

/--
```text
ctDecl ∈ Class or type → type S u₁ ... uₖ = t                              k ≥ 0
                       | data cx => S u₁ ... uₖ = conDecls                 k ≥ 0
                       | class cx => B u where sigs; bind₁; ...; bindₙ     k ≥ 0
```
--/
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
    → List Signature
    → List Binding
    → ClassOrType

/--
```text
instDecl ∈ InstanceDecl → instance cx => C t where bind₁; …; bindₙ    n ≥ 0
```
-/
inductive InstanceDecl : Type where
  | instDecl : Context → Class_Name → TypeExpression → List Binding → InstanceDecl

/--
```text
instDecls ∈ InstanceDecls → instDecl₁; …; instDeclₙ   n ≥ 0
```
-/
inductive InstanceDecls : Type where
  | instDecls : List InstanceDecl → InstanceDecls

/--
```text
ctDecls ∈ Classes and types → [ctDecl₁;...;ctDeclₙ then ctDecls]
                              n ≥ 1
```
--/
inductive ClassesAndTypes : Type where
  | ct_empty
  | ct_Decls : NonEmptyList ClassOrType → ClassesAndTypes → ClassesAndTypes



def classAssertionName : ClassAssertion → QClassName
  | ClassAssertion.classAssert C _ _ => C

/-
Reconstruct the type of the class assertion. e.g.
   classAssertionType (C u (t₁ … tₖ)) = u t₁ … tₖ
-/
def classAssertionType : ClassAssertion → TypeExpression
   | ClassAssertion.classAssert _ TV TS => List.foldl TypeExpression.app (TypeExpression.var TV) TS

end Source
