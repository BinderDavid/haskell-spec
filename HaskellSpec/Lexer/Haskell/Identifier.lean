import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Haskell.Literals

/-
Identifiers
-/



def Tick : RE := RE.Symbol '\''

def ConId : RE := RE.App Large (RE.Star  (unions [Small, Large, Digit, Tick]))

def ModId : RE := ConId

def QConId : RE :=
  RE.App (RE.Star (RE.App ConId (RE.Symbol '.'))) ConId

def QConIdR : Rule :=
  Rule.mk QConId (λ _ => Token.QConId [] "")

def VarId : RE := RE.App Small (RE.Star  (unions [Small, Large, Digit, Tick]))


def all_identifiers : List Rule :=
  [QConIdR]
