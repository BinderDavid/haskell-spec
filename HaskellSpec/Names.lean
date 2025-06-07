/-!
# Names

There is a qualified and an unqualified variant for all names.
We call them X and QX, respectively.

Names are defined in Table 1 and Fig. 3 in the paper

-/

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
`J`
-/
inductive Constructor : Type where

/--
`K`
-/
inductive QConstructor : Type where

/--
`S`
-/
inductive Type_Name : Type where

/--
`T`
-/
inductive QType_Name : Type where

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

/--
`M`
-/
inductive Module_Name : Type where

/--
`M`
-/
inductive QModule_Name : Type where
