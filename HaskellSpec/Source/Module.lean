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
