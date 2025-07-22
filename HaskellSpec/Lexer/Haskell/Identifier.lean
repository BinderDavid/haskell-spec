import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Haskell.Literals

/-
Identifiers
-/



def Tick : RE := RE.Symbol '\''

def ConId : RE := RE.App Large (RE.Star  (unions [Small, Large, Digit, Tick]))

def VarId : RE := RE.App Small (RE.Star  (unions [Small, Large, Digit, Tick]))
