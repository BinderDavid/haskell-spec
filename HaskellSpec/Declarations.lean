-- Figure 1
import HaskellSpec.Names
import HaskellSpec.Expressions
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
      → Import_List
      → Import

  /--
  ```text
  implist ∈ Import list → [[hiding] (ent₁,...,entₙ)]
                          n ≥ 0
  ```
  --/
  inductive Import_List : Type where
    | imp_hiding  : List Entity → Import_List
    | imp_showing : List Entity → Import_List
    | imp_empty

  /--
  ```text
  body ∈ Module body → ctDecls; instDecls; binds
  ```
  --/
  inductive Module_Body : Type where
    | body :
        Classes_and_Types
      → Instance_Decls
      -- → Binds
      → Module_Body

  /--
  ```text
  ctDecls ∈ Classes and types → [ctDecl₁;...;ctDeclₙ then ctDecls]
                                n ≥ 1
  ```
  --/
  inductive Classes_and_Types : Type where
    | ct_empty
    | ct_Decls : NonEmptyList Class_or_Type → Classes_and_Types → Classes_and_Types

  /--
  ```text
  ctDecl ∈ Class or type → type S u₁ ... uₖ = t                              k ≥ 0
                         | data cx => S u₁ ... uₖ = conDecls                 k ≥ 0
                         | class cx => B u where sigs; bind₁; ...; bindₙ     k ≥ 0
  ```
  --/
  inductive Class_or_Type : Type where
    | ct_type :
        Type_Name
      → List Type_Variable
      → Type_Expression
      → Class_or_Type
    | ct_data :
        Context
      → Type_Name
      → List Type_Variable
      → Constructor_Decls
      → Class_or_Type
    | ct_class :
        Context
      → Class_Name
      → Type_Variable
      → Signatures
      -- → List Binding
      → Class_or_Type

  /--
  ```text
  t ∈ Type expression → u
                      | T
                      | t₁ t₂
  ```
  --/
  inductive Type_Expression : Type where
    | type_var  : Type_Variable → Type_Expression
    | type_name : Type_Name → Type_Expression
    | type_cons : Type_Expression → Type_Expression → Type_Expression

  /--
  ```text
  cx ∈ Context → (class₁,...,classₖ)
                 k ≥ 0
  ```
  --/
  def Context : Type := List Class_Assertion

  inductive Class_Assertion : Type where

  inductive Constructor_Decls : Type where

  inductive Constructor_Decl : Type where

  inductive Instance_Decls : Type where

  inductive Instance_Decl : Type where

  inductive Signatures : Type where

  inductive Signature : Type where
end

-- inductive Plus : Type where
--   | add : Nat → Nat → Plus
-- open Plus

-- notation a "⊕" b => add a b

-- def test2 : Plus := 2 ⊕ 2
