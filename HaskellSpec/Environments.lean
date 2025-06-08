import HaskellSpec.Names
import HaskellSpec.SemanticTypes

namespace Environment

@[reducible]
def Env (name : Type) (info : Type) : Type :=
  List (name × info)


/--
Class environment
-/
inductive CE : Type where

/--
Type environment
-/
inductive TE : Type where

/--
Data constructor environment
-/
inductive DE : Type where

/--
Label Environment
-/
inductive LE : Type where

/--
Instance Environment
-/
inductive IE : Type where

/--
Overloading Environment
-/
inductive OE : Type where

/--
Variable Environment
-/
inductive VE : Type where

/--
Kind Environment
-/
inductive KE_Name : Type where
  | T : QType_Name -> KE_Name
  | u : Type_Variable -> KE_Name
  | C : QClassName -> KE_Name


@[reducible]
def KE : Type :=
  Env KE_Name SemanticTypes.Kind

/--
Source Environment
-/
inductive SE : Type where

/--
### Global Environment

Cp. section 2.7.8
-/
def GE : Type := CE × TE × DE

/--
### Full Environment

Cp. section 2.7.8
-/
def FE : Type := CE × TE × DE × IE × VE

/--
### Entity Environment

Cp. section 2.7.8
-/
def EE : Type := CE × TE × DE × VE

/--
### Module Environment

Cp. section 2.7.9
-/
def ME : Type := Env Module_Name FE

end Environment
