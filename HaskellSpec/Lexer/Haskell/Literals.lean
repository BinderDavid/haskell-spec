import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Rules
import HaskellSpec.Lexer.Haskell.Tokens
import HaskellSpec.Lexer.Haskell.CharClasses

namespace Literals

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

def IntegerR : Rule :=
  Rule.mk Integer (λ _ => Token.LitInteger 0) -- TODO: Parse string and convert to obtain literal value

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

def FloatR : Rule :=
  Rule.mk Float (λ _ => Token.LitFloat 0 0) -- TODO: Parse string and convert to obtain literal value

/-
Character Literals
-/

def CharEscape : RE :=
  unions [ RE.Symbol 'a', -- alert
           RE.Symbol 'b', -- backspace
           RE.Symbol 'f', -- form feed
           RE.Symbol 'n', -- new line
           RE.Symbol 'r', -- carriage return
           RE.Symbol 't', -- horizontal tab
           RE.Symbol 'v', -- vertical tab
           RE.Symbol '\\',
           RE.Symbol '"',
           RE.Symbol '\'',
           RE.Symbol '&']

def Ascii : RE := sorry

def Escape : RE :=
  RE.App (RE.Symbol '\\')
         (unions [CharEscape, Ascii, Decimal, RE.App (RE.Symbol 'o') Octal, RE.App (RE.Symbol 'x') Hexadecimal])

def Char : RE := sorry

def CharR : Rule :=
  Rule.mk Char (λ_ => Token.LitChar 'a') -- TODO: Parse string and convert to obtain literal value

/-
String Literals
-/

def String : RE := sorry

def StringR : Rule :=
  Rule.mk String (λ _ => Token.LitString "") -- TODO: Parse string and convert to obtain literal value

/-
All rules declared in this module for literals
-/

def all_literals : List Rule :=
  [IntegerR, FloatR, CharR, StringR]

end Literals
