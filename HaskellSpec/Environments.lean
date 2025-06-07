namespace Environment

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
inductive KE : Type where

/--
Source Environment
-/
inductive SE : Type where

/--
Global Environment
-/
inductive GE : Type where

/--
Full Environment
-/
inductive FE : Type where

/--
Module Environment
-/
inductive ME : Type where

end Environment
