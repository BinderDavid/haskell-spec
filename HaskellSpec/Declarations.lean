-- Figure 1
import HaskellSpec.Names
import HaskellSpec.NonEmptyList

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

mutual
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
  implist ∈ Import list → [[hiding] (ent₁,...,entₙ)]
                          n ≥ 0
  ```
  --/
  inductive ImportList : Type where
    | imp_hiding  : List Entity → ImportList
    | imp_showing : List Entity → ImportList
    | imp_empty
end

mutual
  /--
  ```text
  body ∈ Module body → ctDecls; instDecls; binds
  ```
  --/
  inductive ModuleBody : Type where
    | body :
        ClassesAndTypes
      → InstanceDecls
      -- → Binds
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
      -- → List Binding
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
    | classAssert : Class_Name -> Type_Variable -> List TypeExpression -> ClassAssertion

  /--
  ```text
  conDecl ∈ ConstructorDecl → J t₁ … tₙ                 k ≥ 0
                            | J { v₁ ∷ t₁ … vₙ ∷ tₙ }   k ≥ 0
  ```
  -/
  inductive ConstructorDecl : Type where
    | conDecl_simple: Constructor -> List TypeExpression -> ConstructorDecl
    | conDecl_record: Constructor -> List (QVariable × TypeExpression) -> ConstructorDecl

  /--
  ```text
  conDecls ∈ ConstructorDecls → conDecl₁ | … | conDeclₙ   n ≥ 1
  ```
  -/
  inductive ConstructorDecls : Type where
    | conDecls : NonEmptyList ConstructorDecl -> ConstructorDecls

  /--
  ```text
  instDecl ∈ InstanceDecl → instance cx => C t where bind₁; …; bindₙ    n ≥ 0
  ```
  -/
  inductive InstanceDecl : Type where
    -- | instDecl : Context -> Class_Name -> Type_Expression -> List Binding -> InstanceDecl

  /--
  ```text
  instDecls ∈ InstanceDecls → instDecl₁; …; instDeclₙ   n ≥ 0
  ```
  -/
  inductive InstanceDecls : Type where
    | instDecls : List InstanceDecl -> InstanceDecls

  /--
  ```text
  sig ∈ Signature → v :: cx => t
  ```
  -/
  inductive Signature : Type where
    | sig : QVariable -> Context -> TypeExpression -> Signature

  /--
  ```text
  sigs ∈ Signatures → sig₁; …; sigₙ   n ≥ 0
  ```
  -/
  inductive Signatures : Type where
    | sigs : List Signature -> Signatures

  /--
  ```text
  cx ∈ Context → (class₁,...,classₖ)
                  k ≥ 0
  ```
  --/
  inductive Context : Type where
    | cx : List ClassAssertion -> Context
end
