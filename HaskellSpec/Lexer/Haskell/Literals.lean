import HaskellSpec.Lexer.RegExp

namespace Literals

/-
Decimal, Hexadecimals, Octals
-/

def Digit : RE :=
  unions [ RE.Symbol '0',
           RE.Symbol '1',
           RE.Symbol '2',
           RE.Symbol '3',
           RE.Symbol '4',
           RE.Symbol '5',
           RE.Symbol '6',
           RE.Symbol '7',
           RE.Symbol '8',
           RE.Symbol '9']

def Octit : RE :=
  unions [ RE.Symbol '0',
           RE.Symbol '1',
           RE.Symbol '2',
           RE.Symbol '3',
           RE.Symbol '4',
           RE.Symbol '5',
           RE.Symbol '6',
           RE.Symbol '7']

def Hexit : RE :=
  unions [ RE.Symbol '0',
           RE.Symbol '1',
           RE.Symbol '2',
           RE.Symbol '3',
           RE.Symbol '4',
           RE.Symbol '5',
           RE.Symbol '6',
           RE.Symbol '7',
           RE.Symbol '8',
           RE.Symbol '9',
           RE.Symbol 'a',
           RE.Symbol 'A',
           RE.Symbol 'b',
           RE.Symbol 'B',
           RE.Symbol 'c',
           RE.Symbol 'C',
           RE.Symbol 'd',
           RE.Symbol 'D',
           RE.Symbol 'e',
           RE.Symbol 'E',
           RE.Symbol 'f',
           RE.Symbol 'F']

def Decimal : RE := RE.Plus Digit

def Octal : RE := RE.Plus Octit

def Hexadecimal : RE := RE.Plus Hexit

/-
Integer Literals
-/

def OctalPrefix : RE :=
  RE.Union (RE.App (RE.Symbol '0') (RE.Symbol 'o')) (RE.App (RE.Symbol '0') (RE.Symbol 'O'))

def HexPrefix : RE :=
  RE.Union (RE.App (RE.Symbol '0') (RE.Symbol 'x')) (RE.App (RE.Symbol '0') (RE.Symbol 'X'))

def Integer : RE :=
  unions [ Decimal,
           apps [OctalPrefix, Decimal],
           apps [HexPrefix, Decimal]]

/-
Float Literals
-/

def Exponent : RE :=
  unions [ apps [RE.Symbol 'e', Decimal],
           apps [RE.Symbol 'e', RE.Symbol '+', Decimal],
           apps [RE.Symbol 'e', RE.Symbol '-', Decimal],
           apps [RE.Symbol 'E', Decimal],
           apps [RE.Symbol 'E', RE.Symbol '+', Decimal],
           apps [RE.Symbol 'E', RE.Symbol '-', Decimal]]

def Float1 : RE :=
  apps [ Decimal, RE.Symbol '.', Decimal]

def Float2 : RE :=
  apps [ Decimal, RE.Symbol '.', Decimal, Exponent]

def Float3 : RE :=
  apps [ Decimal, Exponent]

def Float : RE :=
  unions [Float1, Float2, Float3]

end Literals
