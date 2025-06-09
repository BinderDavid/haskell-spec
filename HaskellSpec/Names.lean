/-!
# Names

There is a qualified and an unqualified variant for all names.
We call them X and QX, respectively.

Names are defined in Table 1 and Fig. 3 in the paper
-/

/--
`M`
-/
inductive Module_Name : Type where
  | Mk : String -> Module_Name

/--
`v`
-/
inductive Variable : Type where
  | Mk : String → Variable

/--
```text
x ∈ Original variable → v
                      | M!v
```
-/
inductive QVariable : Type where
  | Unqualified : Variable → QVariable
  | Qualified : Module_Name → Variable → QVariable

/--
```
Δ ∈ Special data constructor → ()
                             | (k)
                             | []
                             | (:)
```
-/
inductive Special_Data_Constructor where
  | Unit : Special_Data_Constructor
  | Tuple : Nat → Special_Data_Constructor
  | Nil : Special_Data_Constructor
  | Cons : Special_Data_Constructor


/--
`J`
-/
inductive Constructor : Type where
  | Mk : String → Constructor

/--
```text
K ∈ Qualified data constructor → J
                               | M.J
                               | Δ
```
-/
inductive QConstructor : Type where
  | Unqualified : Constructor → QConstructor
  | Qualified : Module_Name → Constructor → QConstructor
  | Special : Special_Data_Constructor → QConstructor

/--
```text
Σ ∈ Special type constructor → ()
                             | (k)
                             | []
                             | (->)
```
-/
inductive Special_Type_Constructor : Type where
  | Unit : Special_Type_Constructor
  | Tuple : Nat → Special_Type_Constructor
  | List : Special_Type_Constructor
  | Fun : Special_Type_Constructor

/--
```text
S ∈ Type constructor
```
-/
inductive Type_Name : Type where
  | Mk : String -> Type_Name

/--
```text
T ∈ Qualified type constructor → S
                               | M.S
                               | Σ
```
-/
inductive QType_Name : Type where
  | Unqualified : Type_Name → QType_Name
  | Qualified : Module_Name → Type_Name → QType_Name
  | Special : Special_Type_Constructor → QType_Name

/--
`u`
-/
inductive Type_Variable : Type where

/--
`M`
-/
inductive QType_Variable : Type where

/--
`B`
-/
inductive Class_Name : Type where
  | Mk : String → Class_Name

/--
# Qualified class name
```text
C ∈ Qualified class name → B
                         | M.B
```
-/
inductive QClassName : Type where
  | Unqualified : Class_Name → QClassName
  | Qualified : Module_Name → Class_Name → QClassName

/--
This class is informally specified at the end of section 2.3.
-/
class Unqual (name : Type u) where
  unQual : name -> name

instance instUnqualQClassName : Unqual QClassName where
  unQual
    | QClassName.Unqualified c => QClassName.Unqualified c
    | QClassName.Qualified _ c => QClassName.Unqualified c

instance instUnqualQConstructor : Unqual QConstructor where
  unQual
    | QConstructor.Unqualified c => QConstructor.Unqualified c
    | QConstructor.Qualified _ c => QConstructor.Unqualified c
    | QConstructor.Special s => QConstructor.Special s

/--
This class needed for a polymorphic implementation of
the `justQs` function, defined in section 2.7.
-/
class IsQual (name : Type u) where
  isQual : name -> Bool

instance instIsqualQClassName : IsQual QClassName where
  isQual
    | QClassName.Unqualified _ => False
    | QClassName.Qualified _ _ => True

instance instIsqualQConstructor : IsQual QConstructor where
  isQual
    | QConstructor.Unqualified _ => False
    | QConstructor.Qualified _ _ => True
    | QConstructor.Special _ => False
