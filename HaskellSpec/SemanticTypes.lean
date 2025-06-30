import HaskellSpec.Names

/-!
# Semantic Types

Semantic types are defined in Fig. 4 of the paper.
-/
namespace SemTy

/--
```text
κ ∈ Kind → *
         | κ → κ
```
-/
inductive Kind : Type where
  | Star : Kind
  | Fun : Kind → Kind → Kind

export Kind (Star Fun)



/--
```text
Γ ∈ Class name → Cᵏ
```
-/
inductive Class_Name : Type where
  | Mk : OClass_Name → Kind → Class_Name


def hs_prelude : Module_Name :=
  Module_Name.Mk "Prelude"

def hs_ratio : Module_Name :=
  Module_Name.Mk "Ratio"

def prelude_eq : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Eq")) Kind.Star


def prelude_ord : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Ord")) Kind.Star

def prelude_num : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Num")) Kind.Star

def prelude_integral : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Integral")) Kind.Star

def prelude_fractional : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Fractional")) Kind.Star

def prelude_enum : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Enum")) Kind.Star

def prelude_monad : Class_Name :=
  Class_Name.Mk (OClass_Name.Qualified hs_prelude (_root_.Class_Name.Mk
  "Monad")) (Kind.Fun Kind.Star Kind.Star)

def prelude_enum_from : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFrom")

def prelude_enum_from_then : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFromThen")

def prelude_enum_from_to : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFromTo")

def prelude_enum_from_then_to : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFromThenTo")

def prelude_frominteger : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "fromInteger")

def prelude_fromrational : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "fromRational")

def ratio_percent : QVariable :=
  QVariable.Qualified hs_ratio (Variable.Mk "(%)")

/--
```text
χ ∈ Type constructor → Tᵏ
```
-/
inductive Type_Constructor : Type where
  | Mk : OType_Name → Kind → Type_Constructor

/--
```text
α ∈ Type variable → uᵏ
```
-/
inductive Type_Variable : Type where
  | Mk : Type_Variable → Kind → Type_Variable

/--
```text
τ ∈ Type → α
         | χ
         | τ₁ τ₂
```
-/
inductive TypeS : Type where
  | Variable : Type_Variable → TypeS
  | TypeConstructor : Type_Constructor → TypeS
  | App : TypeS → TypeS → TypeS

/--
The type `Prelude!Char`
-/
def prelude_char : TypeS :=
  TypeS.TypeConstructor (Type_Constructor.Mk (OType_Name.Qualified (Module_Name.Mk "Prelude") (Type_Name.Mk "Char")) Kind.Star)

/--
The type `Prelude!Bool`
-/
def prelude_bool : TypeS :=
  TypeS.TypeConstructor (Type_Constructor.Mk (OType_Name.Qualified (Module_Name.Mk "Prelude") (Type_Name.Mk "Bool")) Kind.Star)

/--
The type `[] : * → *`
-/
def prelude_list : TypeS :=
TypeS.TypeConstructor (Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.List) (Kind.Fun Kind.Star Kind.Star))

/--
The type `-> : * → * → *`
-/
def prelude_fun : TypeS :=
TypeS.TypeConstructor (Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.Fun) (Kind.Fun Kind.Star (Kind.Fun Kind.Star Kind.Star)))

/--
```text
θ ∈ Context → (Γ₁ τ₁, … , Γₙ τₙ)
```
-/
inductive Context : Type where
  | Mk : List (Class_Name × TypeS) → Context

/--
```text
σ ∈ Type Scheme →  ∀ α₁ … αₖ.θ ⇒ τ
```
-/
inductive TypeScheme : Type where
  | Forall : List Type_Variable → Context → TypeS → TypeScheme

/--
This is written as follows in the paper:
```
∀ α. Γ α ⇒c σ
```
It is used to assign types to typeclass methods. The example given in the paper is:
```
ceiling : ∀ α. RealFrac α ⇒c ∀ β. Integral β ⇒ α → β
```
-/
inductive ClassTypeScheme : Type where
  | Forall : Type_Variable → Class_Name → TypeScheme → ClassTypeScheme
end SemTy
