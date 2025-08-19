import HaskellSpec.Names
import HaskellSpec.SemanticTypes

namespace Env

@[reducible]
def Env (name : Type) (info : Type) : Type :=
  List (name × info)

/--
Operations on Environments
Section 2.7
-/
def dom (env : Env name info) : List name :=
  List.map Prod.fst env

-- This one is not explicitly defined in the paper, but needed later on.
def range : (Env.Env k v) → List v := List.map Prod.snd

def remove [BEq name] (env : Env name info) (names : List name) : (Env name info) :=
  List.filter pred env
  where pred := (Bool.not ∘ (List.contains names) ∘ Prod.fst)

def restrict [BEq name] (env : Env name info) (names : List name) : (Env name info) :=
  List.filter pred env
  where pred := (List.contains names ∘ Prod.fst)

def intersect [BEq t] (l l' : List t) : (List t) :=
  List.filter (List.contains l') l

def oplus [BEq name] [BEq info] (env env' : Env name info) (_ : (intersect (dom env) (dom env') = empty)) : (Env name info) :=
  List.append env env'

-- ⊕ from Section 2.7 as a ternary relation
-- asserts that the environments have no overlapping domains
inductive safe_union : Env k v -> Env k v -> Env k v -> Prop where

  | SU_nil :
      safe_union [] E E

  | SU_cons :
        k ∉ dom E'
      → safe_union E E' E''
      → safe_union ((k, v) :: E) E' ((k, v) :: E'')

export safe_union (SU_nil SU_cons)

def oplusbar [BEq name] (env env' : Env name info) (_ : (restrict env (dom env') = restrict env' (dom env))) : (Env name info) :=
  List.append env env'

def oplusarrow [BEq name] [BEq info] (env env' : Env name info) : (Env name info) :=
  List.append (remove env (dom env')) env'

def oplustilde (env env' : Env name info) : (Env name info) :=
  List.append env env'

def unQual [Unqual name] : Env name info -> Env name info :=
  List.map (λ(n, i) => (Unqual.unQual n, i))

def justQs [IsQual name] : Env name info -> Env name info :=
  List.filter (IsQual.isQual ∘ Prod.fst)

def justSingle [BEq name] [BEq info] : Env name info -> Env name info :=
  sorry -- TODO My first attempt at defining this did not satisfy the
        -- termination checker.

inductive TE_Item : Type where
  | DataType : SemTy.Type_Constructor →  TE_Item
  | TypeSynonym : SemTy.Type_Constructor → Int → List SemTy.Type_Variable → SemTy.TypeS → TE_Item

/--
### Type environment

The type environment contains information about type constructors and type variables.
The type constructor information is derived from type declarations in the program and
the type variable information records in-scope type variables.

Cp. section 2.7.2
-/
def TE : Type := Env QType_Name TE_Item × Env Type_Variable SemTy.Type_Variable

def TE_init : TE :=
  ([(QType_Name.Special Special_Type_Constructor.List, TE_Item.DataType (SemTy.Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.List) (SemTy.Kind.Fun SemTy.Kind.Star SemTy.Kind.Star)))],
   [])

/--
### Label Environment

Cp. section 2.7.3
-/
inductive LE : Type where

/--
Section 2.7.4

The definition of IE_Entry is implicit in the definition of IE.

Note: We are using the type Variable from the source language for x,
instead of a separate type for the semantic syntax. For v, we use
QVariable instead of a type for the semantic syntax.
-/
inductive IE_Entry : Type where
/--
v is bound in a dictionary abstraction
```text
v : Γ (α τ_1 … τ_k)
```
-/
  | BoundInDictionaryAbstraction : Variable
    -> SemTy.SClass_Name -> SemTy.Type_Variable
    -> List SemTy.TypeS -> IE_Entry
/--
x represents a superclass in classinfo
```text
x : Γ α
```
-/
  | SuperclassInClassinfo : QVariable -> SemTy.SClass_Name
    -> SemTy.Type_Variable-> IE_Entry
/--
x is a dictionary from an instance declaration
```text
x : ∀α_1 … α_k . θ ⇒ Γ (χ α_1 … α_k)
```
-/
  | DictioanryFromInstanceDeclaration : QVariable
    -> List SemTy.Type_Variable
    -> SemTy.Context
    -> SemTy.SClass_Name
    -> SemTy.Type_Constructor
    -> SemTy.Type_Variable-> IE_Entry
/--
x extracts a dictionary for the superclass Γ
```text
x : ∀α . Γ'α ⇒ Γα
```
-/
  | ExtractsADictionaryForTheSuperclass : QVariable
    -> SemTy.Type_Variable
    -> SemTy.SClass_Name
    -> SemTy.SClass_Name
    -> IE_Entry

/--
### Instance Environment

Cp. section 2.7.4
-/
def IE := List IE_Entry

/--
### Class environment

Cp. section 2.7.1
-/
inductive CEEntry : Type where
  | ceEntry :
      SemTy.SClass_Name ->
      Int ->
      -- this is probably wrong, it should be some "dictionary variable"
      Variable ->
      QClassName ->
      IE ->
      CEEntry

@[reducible]
def CE := List CEEntry

/--
Cp. Fig 16
-/
def CE_init : CE := []

/--
### Data constructor environment

Cp. section 2.7.3
-/
def DE : Type := Env QConstructor (QConstructor × SemTy.Type_Constructor × SemTy.TypeScheme) × Env QVariable (QVariable × SemTy.Type_Constructor × LE)

/--
### Overloading Environment

Cp. section 2.7.4
-/
inductive OE : Type where


inductive VE_Item : Type where
  | Ordinary : QVariable → SemTy.TypeScheme → VE_Item
  | Class : QVariable → SemTy.ClassTypeScheme → VE_Item

/--
### Variable Environment

Cp. section 2.7.5
-/
@[reducible]
def VE : Type :=
  Env QVariable VE_Item


inductive KE_Name : Type where
  | T : QType_Name -> KE_Name
  | u : Type_Variable -> KE_Name
  | C : QClassName -> KE_Name

/--
### Kind Environment

The kind environment contains information about the kinds of class and type names and type variables.

Cp. section 2.7.6
-/
@[reducible]
def KE : Type :=
  Env KE_Name SemTy.Kind

/--
### Source Environment

Cp. section 2.7.7
-/
inductive SE : Type where

/--
### Global Environment

Cp. section 2.7.8
-/
structure GE : Type where
  ce : CE
  te : TE
  de : DE

/--
### Full Environment

Cp. section 2.7.8
-/
structure FE : Type where
  ce : CE
  te : TE
  de : DE
  ie : IE
  ve : VE

/--
### Entity Environment

Cp. section 2.7.8
-/
structure EE : Type where
  ce : CE
  te : TE
  de : DE
  ve : VE

/--
### Module Environment

Cp. section 2.7.9
-/
def ME : Type := Env Module_Name FE

end Env
