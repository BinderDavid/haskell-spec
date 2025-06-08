import HaskellSpec.Names
import HaskellSpec.SemanticTypes

namespace Environment

@[reducible]
def Env (name : Type) (info : Type) : Type :=
  List (name × info)

/--
Operations on Environments
Section 2.7
-/
def dom (env : Env name info) : List name :=
  List.map Prod.fst env

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


inductive VE_Item : Type where

/--
Variable Environment
-/
def VE : Type := Env QVariable VE_Item

/--
Kind Environment
-/
inductive KindEnv_Name : Type where
  | T : QType_Name -> KindEnv_Name
  | u : Type_Variable -> KindEnv_Name
  | C : QClassName -> KindEnv_Name


@[reducible]
def KindEnv : Type :=
  Env KindEnv_Name SemanticTypes.Kind

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
