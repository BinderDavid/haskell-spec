import HaskellSpec.Names
import HaskellSpec.SemanticTypes

/-!
# Prelude
-/
namespace Prelude

/-
## Module Names
-/

def hs_prelude : Module_Name := Module_Name.Mk "Prelude"

def hs_ratio : Module_Name := Module_Name.Mk "Ratio"

/-
## Haskell Types in the Prelude
-/


/--
The type `Prelude!Char`
-/
def char : SemTy.TypeS :=
  SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Qualified hs_prelude (Type_Name.Mk "Char")) SemTy.Kind.Star)

/--
The type `Prelude!Bool`
-/
def bool : SemTy.TypeS :=
  SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Qualified hs_prelude (Type_Name.Mk "Bool")) SemTy.Kind.Star)

/--
The type `[] : * → *`
-/
def list : SemTy.TypeS :=
  SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.List) (SemTy.Kind.Fun SemTy.Kind.Star SemTy.Kind.Star))

def mk_list (α: SemTy.TypeS): SemTy.TypeS :=
 SemTy.TypeS.App list α

/--
The type `-> : * → * → *`
-/
def funt : SemTy.TypeS :=
  SemTy.TypeS.TypeConstructor (SemTy.Type_Constructor.Mk (OType_Name.Special Special_Type_Constructor.Fun) (SemTy.Kind.Fun SemTy.Kind.Star (SemTy.Kind.Fun SemTy.Kind.Star SemTy.Kind.Star)))

def mk_funt (α β : SemTy.TypeS) : SemTy.TypeS :=
  SemTy.TypeS.App (SemTy.TypeS.App funt α) β

/-
## Names of Type Classes
-/

def eq : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Eq")) SemTy.Kind.Star

def ord : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Ord")) SemTy.Kind.Star

def num : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Num")) SemTy.Kind.Star

def integral : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Integral")) SemTy.Kind.Star

def fractional : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Fractional")) SemTy.Kind.Star

def enum : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Enum")) SemTy.Kind.Star

def monad : SemTy.SClass_Name :=
  SemTy.SClass_Name.mk (OClass_Name.Qualified hs_prelude (Class_Name.mk
  "Monad")) (SemTy.Kind.Fun SemTy.Kind.Star SemTy.Kind.Star)

/-
## Names of Type Class Methods
-/

def enum_from : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFrom")

def enum_from_then : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFromThen")

def enum_from_to : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFromTo")

def enum_from_then_to : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "enumFromThenTo")

def frominteger : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "fromInteger")

def fromrational : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "fromRational")

def equals : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "(==)")

def bind : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "(>>=)")

def sequence : QVariable :=
  QVariable.Qualified hs_prelude (Variable.Mk "(>>)")

/-
## Names of Constructors
-/

def ratio_percent : QVariable :=
  QVariable.Qualified hs_ratio (Variable.Mk "(%)")

end Prelude
