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

/--
`M`
-/
inductive QModule_Name : Type where


/--
`v`
-/
inductive Variable : Type where
  | Mk : String → Variable

/--
`x`
-/
inductive QVariable : Type where

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

/--
`C`
-/
inductive QClassName : Type where
