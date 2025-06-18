import HaskellSpec.Source.Lang
import HaskellSpec.Names

namespace Source

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
  | var        : QVariable → Entity
  | cons       : QConstructor → Entity
  | type_some  : QType_Name → List QVariable → List QConstructor → Entity
  | type_all   : QType_Name → Entity
  | class_some : QClassName → List QVariable → Entity
  | class_all  : QClassName → Entity
  | module     : Module_Name → Entity -- use QModule_Name
/--
```text
implist ∈ Import list → [[hiding] (ent₁,...,entₙ)]
                        n ≥ 0
```
--/
inductive ImportList : Type where
  | hide_some  : List Entity → ImportList
  | list_some : List Entity → ImportList
  | empty

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
  | conDecls : NonEmptyList ConstructorDecl → ConstructorDecls

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
  | empty
  | decls : NonEmptyList ClassOrType → ClassesAndTypes → ClassesAndTypes



def classAssertionName : ClassAssertion → QClassName
  | ClassAssertion.classAssert C _ _ => C

/-
Reconstruct the type of the class assertion. e.g.
   classAssertionType (C u (t₁ … tₖ)) = u t₁ … tₖ
-/
def classAssertionType : ClassAssertion → TypeExpression
   | ClassAssertion.classAssert _ TV TS => List.foldl TypeExpression.app (TypeExpression.var TV) TS

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
mod ∈ Module → module M (ent₁,..., entₖ) where imp₁;...;impₙ;body
               k, n ≥ 0
```
--/
inductive Module : Type where
  | module :
      Module_Name
    → List Entity
    → List Import
    → Module

end Source
