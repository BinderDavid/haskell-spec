import HaskellSpec.Names
import HaskellSpec.NonEmptyList
/-!
Figure 1 and 2
-/


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
  | ent_var      : QVariable → Entity
  | ent_cons     : QConstructor → Entity
  | ent_type     : QType_Name → List QVariable → List QConstructor → Entity
  | ent_typeall  : QType_Name → Entity
  | ent_class    : QClassName → List QVariable → Entity
  | ent_classall : QClassName → Entity
  | ent_module   : Module_Name → Entity -- use QModule_Name

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


mutual
  /--
  ```text
    p ∈ Pattern → x
                 | K p₁ … pₙ      k ≥ 0
                 | K {fp₁ … fpₙ}  k ≥ 0
                 | v@p
                 | ~p
                 | _
                 | literal
                 | v+integer
  ```
  --/
  inductive Pattern : Type where
    | pat_var : QVariable → Pattern
    | pat_constr_pat : QConstructor → List Pattern → Pattern
    | pat_constr_fieldPat : QConstructor → List FieldPattern → Pattern
    | pat_at : Variable → Pattern → Pattern
    | pat_lazy : Pattern → Pattern
    | pat_wildcard : Pattern
    | pat_lit : Literal → Pattern
    | pat_plus : Variable → Int → Pattern

  /--
  ```text
    fp ∈ FieldPattern → x = p
  ```
  --/
  inductive FieldPattern : Type where
    | fp_pat: Variable → Pattern → FieldPattern
end

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
    | expr_var : QVariable → Expression
    | expr_lit : Literal → Expression
    | expr_constr : QConstructor → Expression
    | expr_abs : NonEmptyList Pattern → Expression → Expression
    | expr_app : Expression → Expression → Expression
    | expr_let : Binds → Expression → Expression
    | expr_case : Expression → NonEmptyList Match → Expression
    | expr_do : Statements → Expression
    | expr_listComp : Expression → Qualifiers → Expression
    | expr_listRange : Expression → Option Expression → Option Expression → Expression
    | expr_recUpd : Expression → List FieldBinding → Expression
    | expr_recConstr : QConstructor → List FieldBinding → Expression

  /--
  ```text
  helper type used in the internals of Statements, Qualifiers
  ```
  -/
  inductive Statement : Type where
    | stmt_arr : Pattern → Expression → Statement
    | stmt_let : Binds → Statement
    | stmt_expr : Expression → Statement

  /--
  ```text
  stmts ∈ Statements → p <- e; stmts
                     | let binds; stmts
                     | e; stmts
                     | e
  ```
  -/
  inductive Statements : Type where
    | stmt_list : List Statement → Expression → Statements

  /--
  ```text
  quals ∈ Qualifiers → p <- e, quals
                     | let binds, quals
                     | e, quals
                     | ε
  ```
  -/
  inductive Qualifiers : Type where
    | qal_list : List Statement → Qualifiers

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
    | type_var  : Type_Variable → TypeExpression
    | type_name : Type_Name → TypeExpression
    | type_cons : TypeExpression → TypeExpression → TypeExpression

  /--
  ```text
  class ∈ ClassAssertion → C (u t₁ … tₖ)   k ≥ 0
  ```
  -/
  inductive ClassAssertion : Type where
    | classAssert : Class_Name → Type_Variable → List TypeExpression → ClassAssertion

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

/--
```text
typeDecl ∈ TypeDeclaration → data χ α₁ … αₖ = conDecl₁ | … | conDeclsₙ    k ≥ 0
                                                                          n ≥ 1
```
--/
inductive TypeDeclaration : Type where
  | typeDecl :
      -- Chi ->
      -- List Alphas ->
      NonEmptyList ConstructorDecl ->
      TypeDeclaration

/--
```text
typeDecls ∈ TypeDeclarations → typeDecl₁; …; typeDeclₙ    n ≥ 0
```
--/
inductive TypeDeclarations : Type where
  | typeDecls : List TypeDeclaration -> TypeDeclarations

/--
```text
mod ∈ Module → module M where typeDecls; binds
```
--/
inductive Module : Type where
  | module :
      Module_Name -- Use QModule:Name in the future?
    → List TypeDeclarations
    → Module
