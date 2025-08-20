import HaskellSpec.Names
namespace Source


/--
Distinguishes between qualified and unqualified imports.
```text
qualifer ∈ Qualifier → qualified
                     | ε
```
-/
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
-/
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
-/
inductive ImportList : Type where
  | hide_some  : List Entity → ImportList
  | list_some : List Entity → ImportList
  | empty

/--
```text
imp ∈ Import → import qualifier M as M' implist
```
-/
structure Import : Type where
  qualified : Qualifier
  from_mod : Module_Name
  as_mod : Module_Name
  entities : ImportList

end Source
