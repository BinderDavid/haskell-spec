import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Haskell.Literals

/-
Identifiers
-/

def Large : RE :=
  unions [ RE.Symbol 'A',
           RE.Symbol 'B',
           RE.Symbol 'C',
           RE.Symbol 'D',
           RE.Symbol 'E',
           RE.Symbol 'F',
           RE.Symbol 'G',
           RE.Symbol 'H',
           RE.Symbol 'I',
           RE.Symbol 'J',
           RE.Symbol 'K',
           RE.Symbol 'L',
           RE.Symbol 'M',
           RE.Symbol 'N',
           RE.Symbol 'O',
           RE.Symbol 'P',
           RE.Symbol 'Q',
           RE.Symbol 'R',
           RE.Symbol 'S',
           RE.Symbol 'T',
           RE.Symbol 'U',
           RE.Symbol 'V',
           RE.Symbol 'W',
           RE.Symbol 'X',
           RE.Symbol 'Y',
           RE.Symbol 'Z']

def Small : RE :=
  unions [ RE.Symbol 'a',
           RE.Symbol 'b',
           RE.Symbol 'c',
           RE.Symbol 'd',
           RE.Symbol 'e',
           RE.Symbol 'f',
           RE.Symbol 'g',
           RE.Symbol 'h',
           RE.Symbol 'i',
           RE.Symbol 'j',
           RE.Symbol 'k',
           RE.Symbol 'l',
           RE.Symbol 'm',
           RE.Symbol 'n',
           RE.Symbol 'o',
           RE.Symbol 'p',
           RE.Symbol 'q',
           RE.Symbol 'r',
           RE.Symbol 's',
           RE.Symbol 't',
           RE.Symbol 'u',
           RE.Symbol 'v',
           RE.Symbol 'w',
           RE.Symbol 'x',
           RE.Symbol 'y',
           RE.Symbol 'z',
           RE.Symbol '_']

def Tick : RE := RE.Symbol '\''

def ConId : RE := RE.App Large (RE.Star  (unions [Small, Large, Literals.Digit, Tick]))

def VarId : RE := RE.App Small (RE.Star  (unions [Small, Large, Literals.Digit, Tick]))

/-
Symbols
-/

def Symbol : RE :=
  unions [ RE.Symbol '!',
           RE.Symbol '#',
           RE.Symbol '$',
           RE.Symbol '%',
           RE.Symbol '&',
           RE.Symbol '*',
           RE.Symbol '+',
           RE.Symbol '.',
           RE.Symbol '/',
           RE.Symbol '<',
           RE.Symbol '=',
           RE.Symbol '>',
           RE.Symbol '?',
           RE.Symbol '@',
           RE.Symbol '\\',
           RE.Symbol '^',
           RE.Symbol '|',
           RE.Symbol '-',
           RE.Symbol '~',
           RE.Symbol ':'
           ]
