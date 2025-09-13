import HaskellSpec.Names
import HaskellSpec.SemanticTypes

namespace Env

@[reducible]
def Env (name info : Type) : Type :=
  List (name × info)

/-
Operations on Environments from section 2.7
-/

/-- Domain of an environment -/
def dom (env : Env name info) : List name :=
  List.map Prod.fst env

/-- Range of an environment -/
def rng (env : Env name info) : List info :=
  List.map Prod.snd env

/-- This is written `E / names` in the paper. -/
def remove [BEq name] (env : Env name info) (names : List name) : Env name info :=
  List.filter (Bool.not ∘ (List.contains names) ∘ Prod.fst) env

/-- This is written `E | names` in the paper. -/
def restrict [BEq name] (env : Env name info) (names : List name) : Env name info :=
  List.filter (List.contains names ∘ Prod.fst) env


def intersect [BEq t] (l l' : List t) : (List t) :=
  List.filter (List.contains l') l

def oplus [BEq name] [BEq info] (env₁ env₂ : Env name info)
          : intersect (dom env₁) (dom env₂) = empty → Env name info :=
  λ _ => List.append env₁ env₂

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

/-- This operation is written `⊕^~` in the paper.-/
class OplusTilde (env : Type) where
  oplustilde (env₁ env₂ : env) : env

instance instOplusTildeEnv : OplusTilde (Env name info) where
  oplustilde env₁ env₂ := List.append env₁ env₂

class Qualify (env : Type) where
  qualify  (m : Module_Name) (e : env) : env

instance instQualifyEnv : Qualify (Env name info) where
  qualify := sorry

def unQual [Unqual name] : Env name info -> Env name info :=
  List.map (λ(n, i) => (Unqual.unQual n, i))

class JustQs (env : Type) where
  justQs : env -> env

instance instJustQsEnv [IsQual name] : JustQs (Env name info) where
  justQs := List.filter (IsQual.isQual ∘ Prod.fst)


def justSingle [BEq name] [BEq info] : Env name info -> Env name info :=
  sorry -- TODO My first attempt at defining this did not satisfy the
        -- termination checker.

inductive TE_Item : Type where
  | DataType : SemTy.Type_Constructor →  TE_Item
  | TypeSynonym : SemTy.Type_Constructor → Int → List SemTy.Type_Variable → SemTy.TypeS → TE_Item


/-
### Type environment

The type environment contains information about type constructors and type variables.
The type constructor information is derived from type declarations in the program and
the type variable information records in-scope type variables.

Cp. section 2.7.2
-/
abbrev TE₁ : Type := Env QType_Name TE_Item
abbrev TE₂ : Type := Env Type_Variable SemTy.Type_Variable
structure TE : Type where
  te₁ : TE₁
  te₂ : TE₂

instance instJustQsTE : JustQs TE where
  justQs te :=
    { te₁ := JustQs.justQs te.te₁
      te₂ := te.te₂
    }

def TE_init : TE :=
  { te₁ := [(QType_Name.Special Special_Type_Constructor.List, TE_Item.DataType (SemTy.Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.List) (SemTy.Kind.Fun SemTy.Kind.Star SemTy.Kind.Star)))]
    te₂ := []
  }

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
structure CEEntry : Type where
  name : SemTy.SClass_Name
  h : Int
  var : Variable
  class_name : QClassName
  ie : IE

@[reducible]
def CE := List (QClassName × CEEntry)


/--
Cp. Fig 16
-/
def CE_init : CE := []


abbrev DE₁ : Type := Env QConstructor (QConstructor × SemTy.Type_Constructor × SemTy.TypeScheme)
abbrev DE₂ : Type := Env QVariable (QVariable × SemTy.Type_Constructor × LE)

def constrs (de : DE₁)(χ : SemTy.Type_Constructor) : List QConstructor :=
  List.map Prod.fst (List.filter (λ ⟨_,info⟩ => info.snd.fst == χ) de)

def fields (de : DE₂)(χ : SemTy.Type_Constructor) : List QVariable :=
  List.map Prod.fst (List.filter (λ ⟨_,info⟩ => info.snd.fst == χ) de)

/--
### Data constructor environment

Cp. section 2.7.3
-/
structure DE : Type where
  de₁ : DE₁
  de₂ : DE₂

instance instJustQsDE : JustQs DE where
  justQs de :=
    { de₁ := JustQs.justQs de.de₁
      de₂ := JustQs.justQs de.de₂
    }

/--
### Overloading Environment

Cp. section 2.7.4
-/
inductive OE : Type where


inductive VE_Item : Type where
  | Ordinary : QVariable → SemTy.TypeScheme → VE_Item
  | Class : QVariable → SemTy.ClassTypeScheme → VE_Item
  deriving BEq


/--
### Variable Environment

Cp. section 2.7.5
-/
@[reducible]
def VE : Type :=
  Env QVariable VE_Item

def ops (ve : VE)(Γ : SemTy.SClass_Name) : List QVariable :=
  sorry

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
structure SE : Type where
  cs : Unit
  ts : Unit
  ds : Unit
  vs : Unit

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

/-- An empty entity environment -/
def ee_empty : EE :=
  EE.mk [] ⟨[],[]⟩ ⟨[],[]⟩ []

instance instJustQsEE : JustQs EE where
  justQs ee :=
   { ce := JustQs.justQs ee.ce
     te := JustQs.justQs ee.te
     de := JustQs.justQs ee.de
     ve := JustQs.justQs ee.ve
   }

def ee_union (ee₁ ee₂ : EE) : EE :=
  { ce := ee₁.ce ++ ee₂.ce
    te := ⟨ee₁.te.te₁ ++ ee₂.te.te₁,ee₁.te.te₂ ++ ee₂.te.te₂⟩
    de := ⟨ee₁.de.de₁ ++ ee₂.de.de₁,ee₁.de.de₂ ++ ee₂.de.de₂⟩
    ve := ee₁.ve ++ ee₂.ve
  }


def ee_unions (ees : List EE) : EE :=
  ees.foldl ee_union ee_empty


instance instOplusTildeEE : OplusTilde EE where
  oplustilde ee₁ ee₂ :=
    { ce := OplusTilde.oplustilde ee₁.ce ee₂.ce
      te := ⟨ OplusTilde.oplustilde ee₁.te.te₁ ee₂.te.te₁
            , OplusTilde.oplustilde ee₁.te.te₂ ee₂.te.te₂ ⟩
      de := ⟨ OplusTilde.oplustilde ee₁.de.de₁ ee₂.de.de₁
            , OplusTilde.oplustilde ee₁.de.de₂ ee₂.de.de₂ ⟩
      ve := OplusTilde.oplustilde ee₁.ve ee₂.ve
    }

instance instQualifyEE : Qualify EE where
  qualify := sorry

/--
### Module Environment

Cp. section 2.7.9
-/
@[reducible]
def ME : Type := Env Module_Name FE

end Env
