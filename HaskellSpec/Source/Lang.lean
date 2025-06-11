import HaskellSpec.Names
import HaskellSpec.NonEmptyList
/-!
Figure 1 and 2
-/

namespace Source

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
    /--
    A character literal.
    Example: `'a'`
    Character literals are not overloaded and have type `Prelude.Char`.
    -/
  | char : Char → Literal
    /--
    A string literal.
    Example: `"hello"`
    String literals are not overloaded and have type `[Prelude.Char]`.
    -/
  | string : String → Literal
    /--
    An integer literal.
    Example: `5`
    Integer literals are overloaded and have type `Num a => a`.
    -/
  | integer : Int → Literal
    /--
    A floating literal.
    Example: `2.3`
    Floating literals are overloaded and have type `Fractional a => a`.
    -/
  | float : Float → Literal

/--
```text
qualifer ∈ Qualifier → [qualified]
```
--/
inductive Qualifier : Type where
  | qualified
  | unqualified

/--
An entity is something that can be exported or imported by a module.
```text
ent ∈ Entity → x
             | K
             | T (x₁,...,xₖ;K₁,...,Kₙ)   k, n ≥ 0
             | T (..)
             | C (x₁,...,xₖ)             k    ≥ 0
             | C (..)
             | module M
```
--/
inductive Entity : Type where
  | var       : QVariable → Entity
  | cons      : QConstructor → Entity
  | type      : QType_Name → List QVariable → List QConstructor → Entity
  | typeall   : QType_Name → Entity
  | typeclass : QClassName → List QVariable → Entity
  | classall  : QClassName → Entity
  | module    : Module_Name → Entity -- use QModule_Name

/--
```text
implist ∈ Import list → [[hiding] (ent₁,...,entₙ)]
                        n ≥ 0
```
--/
inductive ImportList : Type where
  | imp_hiding  : List Entity → ImportList
  | imp_showing : List Entity → ImportList
  | imp_empty

/--
```text
imp ∈ Import → import qualifier M as M' implist
```
--/
inductive Import : Type where
  | imp :
      Qualifier
    → Module_Name
    → Module_Name
    → ImportList
    → Import

/--
```text
mod ∈ Module → module M (ent₁,..., entₖ) where imp₁;...;impₙ;body
               k, n ≥ 0
```
--/
inductive Module : Type where
  | module :
      Module_Name -- Use QModule:Name in the future?
    → List Entity
    → List Import
    → Module



/--
```text
  p ∈ Pattern       → x
                    | K p₁ … pₙ      k ≥ 0
                    | K {fp₁ … fpₙ}  k ≥ 0
                    | v@p
                    | ~p
                    | _
                    | literal
                    | v+integer
  fp ∈ FieldPattern → x = p
```
--/
inductive Pattern : Type where
  | var : QVariable → Pattern
  | constr_pat : QConstructor → List Pattern → Pattern
  | constr_fieldPat : QConstructor → List (Variable × Pattern) → Pattern
  | at : Variable → Pattern → Pattern
  | lazy : Pattern → Pattern
  | wildcard : Pattern
  | lit : Literal → Pattern
  | n_plus_k : Variable → Int → Pattern

mutual
  /--
  ```text
    binds ∈ Binds → [ sigs; bindG then binds]
  ```
  --/
  inductive Binds : Type where
    | binds_binds : Signatures → BindGroup → Binds → Binds

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
    e ∈ Expression → x
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
    | recUpd : Expression → List FieldBinding → Expression
    | recConstr : QConstructor → List FieldBinding → Expression

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

  /--
  ```text
  body ∈ Module body → ctDecls; instDecls; binds
  ```
  --/
  inductive ModuleBody : Type where
    | body :
        ClassesAndTypes
      → InstanceDecls
      → Binds
      → ModuleBody

  /--
  ```text
  ctDecls ∈ Classes and types → [ctDecl₁;...;ctDeclₙ then ctDecls]
                                n ≥ 1
  ```
  --/
  inductive ClassesAndTypes : Type where
    | ct_empty
    | ct_Decls : NonEmptyList ClassOrType → ClassesAndTypes → ClassesAndTypes

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
      → Signatures
      → List Binding
      → ClassOrType

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
  cx ∈ Context → (class₁,...,classₖ)
                  k ≥ 0
  ```
  --/
  inductive Context : Type where
    | cx : List ClassAssertion → Context
end

def classAssertionName : ClassAssertion → QClassName
  | ClassAssertion.classAssert C _ _ => C

/-
Reconstruct the type of the class assertion. e.g.
   classAssertionType (C u (t₁ … tₖ)) = u t₁ … tₖ
-/
def classAssertionType : ClassAssertion → TypeExpression
   | ClassAssertion.classAssert _ TV TS => List.foldl TypeExpression.app (TypeExpression.var TV) TS

end Source
