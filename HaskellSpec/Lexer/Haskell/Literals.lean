import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Rules
import HaskellSpec.Lexer.Haskell.Tokens
import HaskellSpec.Lexer.Haskell.CharClasses

namespace Literals

/-
Integer Literals
-/

def parse_octit (c : Char) : Nat :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | _ => 0

def parse_digit (c : Char) : Nat :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | _ => 0

def parse_hexit (c : Char) : Nat :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | 'a' => 10
  | 'A' => 10
  | 'b' => 11
  | 'B' => 11
  | 'c' => 12
  | 'C' => 12
  | 'd' => 13
  | 'D' => 13
  | 'e' => 14
  | 'E' => 14
  | 'f' => 15
  | 'F' => 15
  | _ => 0

def parse_octal (ls : List Char) : Nat :=
  let ls_inv : List Char        := ls.reverse
  let octits : List Nat         := ls_inv.map parse_octit
  let zipped : List (Nat × Nat) := octits.zipIdx 0
  let comped : List Nat         := zipped.map (λ ⟨o,pos⟩ => o * (8^pos))
  comped.sum

def parse_decimal (ls : List Char) : Nat :=
  let ls_inv : List Char        := ls.reverse
  let digits : List Nat         := ls_inv.map parse_digit
  let zipped : List (Nat × Nat) := digits.zipIdx 0
  let comped : List Nat         := zipped.map (λ ⟨o,pos⟩ => o * (10^pos))
  comped.sum

def parse_hexadecimal (ls : List Char) : Nat :=
  let ls_inv : List Char        := ls.reverse
  let hexits : List Nat         := ls_inv.map parse_hexit
  let zipped : List (Nat × Nat) := hexits.zipIdx 0
  let comped : List Nat         := zipped.map (λ ⟨o,pos⟩ => o * (16^pos))
  comped.sum


def parse_integer (s : String) : Nat :=
  match s.toList with
  | '0' :: 'o' :: ls => parse_octal ls
  | '0' :: 'O' :: ls => parse_octal ls
  | '0' :: 'x' :: ls => parse_hexadecimal ls
  | '0' :: 'X' :: ls => parse_hexadecimal ls
  | ls => parse_decimal ls

def OctalPrefix : RE :=
  RE.Union (RE.App (RE.Symbol '0') (RE.Symbol 'o')) (RE.App (RE.Symbol '0') (RE.Symbol 'O'))

def HexPrefix : RE :=
  RE.Union (RE.App (RE.Symbol '0') (RE.Symbol 'x')) (RE.App (RE.Symbol '0') (RE.Symbol 'X'))

def Integer : RE :=
  unions [ Decimal,
           apps [OctalPrefix, Decimal],
           apps [HexPrefix, Decimal]]

def IntegerR : Rule :=
  Rule.mk Integer (λ s => Token.LitInteger (parse_integer s))

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

def CharEscapeWithoutAmpersand : RE :=
  unions [ RE.Symbol 'a', -- alert
           RE.Symbol 'b', -- backspace
           RE.Symbol 'f', -- form feed
           RE.Symbol 'n', -- new line
           RE.Symbol 'r', -- carriage return
           RE.Symbol 't', -- horizontal tab
           RE.Symbol 'v', -- vertical tab
           RE.Symbol '\\',
           RE.Symbol '"',
           RE.Symbol '\'']

def CharEscape : RE :=
  RE.Union CharEscapeWithoutAmpersand
           (RE.Symbol '&')

def Cntrl : RE :=
  unions [ AscLarge,
           RE.Symbol '@',
           RE.Symbol '[',
           RE.Symbol '\\',
           RE.Symbol ']',
           RE.Symbol '^',
           RE.Symbol '_']

def Ascii : RE :=
  unions [ apps [RE.Symbol '^', Cntrl],
           apps [RE.Symbol 'N', RE.Symbol 'U', RE.Symbol 'L'],
           apps [RE.Symbol 'S', RE.Symbol 'O', RE.Symbol 'H'],
           apps [RE.Symbol 'S', RE.Symbol 'T', RE.Symbol 'X'],
           apps [RE.Symbol 'E', RE.Symbol 'T', RE.Symbol 'X'],
           apps [RE.Symbol 'E', RE.Symbol 'O', RE.Symbol 'T'],
           apps [RE.Symbol 'E', RE.Symbol 'N', RE.Symbol 'Q'],
           apps [RE.Symbol 'A', RE.Symbol 'C', RE.Symbol 'K'],
           apps [RE.Symbol 'B', RE.Symbol 'E', RE.Symbol 'L'],
           apps [RE.Symbol 'B', RE.Symbol 'S'],
           apps [RE.Symbol 'H', RE.Symbol 'T'],
           apps [RE.Symbol 'L', RE.Symbol 'F'],
           apps [RE.Symbol 'V', RE.Symbol 'T'],
           apps [RE.Symbol 'F', RE.Symbol 'F'],
           apps [RE.Symbol 'C', RE.Symbol 'R'],
           apps [RE.Symbol 'S', RE.Symbol 'O'],
           apps [RE.Symbol 'S', RE.Symbol 'I'],
           apps [RE.Symbol 'D', RE.Symbol 'L', RE.Symbol 'E'],
           apps [RE.Symbol 'D', RE.Symbol 'C', RE.Symbol '1'],
           apps [RE.Symbol 'D', RE.Symbol 'C', RE.Symbol '2'],
           apps [RE.Symbol 'D', RE.Symbol 'C', RE.Symbol '3'],
           apps [RE.Symbol 'D', RE.Symbol 'C', RE.Symbol '4'],
           apps [RE.Symbol 'N', RE.Symbol 'A', RE.Symbol 'K'],
           apps [RE.Symbol 'S', RE.Symbol 'Y', RE.Symbol 'N'],
           apps [RE.Symbol 'E', RE.Symbol 'T', RE.Symbol 'B'],
           apps [RE.Symbol 'C', RE.Symbol 'A', RE.Symbol 'N'],
           apps [RE.Symbol 'E', RE.Symbol 'M'],
           apps [RE.Symbol 'S', RE.Symbol 'U', RE.Symbol 'B'],
           apps [RE.Symbol 'E', RE.Symbol 'S', RE.Symbol 'C'],
           apps [RE.Symbol 'F', RE.Symbol 'S'],
           apps [RE.Symbol 'G', RE.Symbol 'S'],
           apps [RE.Symbol 'U', RE.Symbol 'S'],
           apps [RE.Symbol 'S', RE.Symbol 'P'],
           apps [RE.Symbol 'D', RE.Symbol 'E', RE.Symbol 'L'],
           ]

def Escape : RE :=
  RE.App (RE.Symbol '\\')
         (unions [CharEscape, Ascii, Decimal, RE.App (RE.Symbol 'o') Octal, RE.App (RE.Symbol 'x') Hexadecimal])

def EscapeWithoutAmpersand : RE :=
  RE.App (RE.Symbol '\\')
         (unions [CharEscapeWithoutAmpersand, Ascii, Decimal, RE.App (RE.Symbol 'o') Octal, RE.App (RE.Symbol 'x') Hexadecimal])

/-- This function defines the canonical meaning of all escape sequences allowed in character literals. -/
def parse_char (s : String) : Char :=
  match s.toList with
  -- Character escapes
  | '\\' :: 'a' :: [] => '\x07' -- alert
  | '\\' :: 'b' :: [] => '\x08' -- backspace
  | '\\' :: 'f' :: [] => '\x0c' -- form feed
  | '\\' :: 'n' :: [] => '\x0a' -- new line / line feed
  | '\\' :: 'r' :: [] => '\x0d' -- carriage return
  | '\\' :: 't' :: [] => '\x09' -- horizontal tab
  | '\\' :: 'v' :: [] => '\x0b' -- vertical tab
  | '\\' :: '\\' :: [] => '\\'
  | '\\' :: '"' :: [] => '"'
  | '\\' :: '\'' :: [] => '\''
  -- Control Escapes
  | '\\' :: '^' :: 'A' :: [] => 'X'
  | '\\' :: '^' :: 'B' :: [] => 'X'
  | '\\' :: '^' :: 'C' :: [] => 'X'
  | '\\' :: '^' :: 'D' :: [] => 'X'
  | '\\' :: '^' :: 'E' :: [] => 'X'
  | '\\' :: '^' :: 'F' :: [] => 'X'
  | '\\' :: '^' :: 'G' :: [] => 'X'
  | '\\' :: '^' :: 'H' :: [] => 'X'
  | '\\' :: '^' :: 'I' :: [] => 'X'
  | '\\' :: '^' :: 'J' :: [] => 'X'
  | '\\' :: '^' :: 'K' :: [] => 'X'
  | '\\' :: '^' :: 'L' :: [] => 'X'
  | '\\' :: '^' :: 'M' :: [] => 'X'
  | '\\' :: '^' :: 'N' :: [] => 'X'
  | '\\' :: '^' :: 'O' :: [] => 'X'
  | '\\' :: '^' :: 'P' :: [] => 'X'
  | '\\' :: '^' :: 'Q' :: [] => 'X'
  | '\\' :: '^' :: 'R' :: [] => 'X'
  | '\\' :: '^' :: 'S' :: [] => 'X'
  | '\\' :: '^' :: 'T' :: [] => 'X'
  | '\\' :: '^' :: 'U' :: [] => 'X'
  | '\\' :: '^' :: 'V' :: [] => 'X'
  | '\\' :: '^' :: 'W' :: [] => 'X'
  | '\\' :: '^' :: 'X' :: [] => 'X'
  | '\\' :: '^' :: 'Y' :: [] => 'X'
  | '\\' :: '^' :: 'Z' :: [] => 'X'
  | '\\' :: '^' :: '@' :: [] => 'X'
  | '\\' :: '^' :: '[' :: [] => 'X'
  | '\\' :: '^' :: '\\' :: [] => 'X'
  | '\\' :: '^' :: ']' :: [] => 'X'
  | '\\' :: '^' :: '^' :: [] => 'X'
  | '\\' :: '^' :: '_' :: [] => 'X'
  -- ASCII Escapes
  | '\\' :: 'N' :: 'U' :: 'L' :: [] => 'X'
  | '\\' :: 'S' :: 'O' :: 'H' :: [] => 'X'
  | '\\' :: 'S' :: 'T' :: 'X' :: [] => 'X'
  | '\\' :: 'E' :: 'T' :: 'X' :: [] => 'X'
  | '\\' :: 'E' :: 'O' :: 'T' :: [] => 'X'
  | '\\' :: 'E' :: 'N' :: 'Q' :: [] => 'X'
  | '\\' :: 'A' :: 'C' :: 'K' :: [] => 'X'
  | '\\' :: 'B' :: 'E' :: 'L' :: [] => 'X'
  | '\\' :: 'B' :: 'S' :: [] => 'X'
  | '\\' :: 'H' :: 'T' :: [] => 'X'
  | '\\' :: 'L' :: 'F' :: [] => 'X'
  | '\\' :: 'V' :: 'T' :: [] => 'X'
  | '\\' :: 'F' :: 'F' :: [] => 'X'
  | '\\' :: 'C' :: 'R' :: [] => 'X'
  | '\\' :: 'S' :: 'O' :: [] => 'X'
  | '\\' :: 'S' :: 'I' :: [] => 'X'
  | '\\' :: 'D' :: 'L' :: 'E' :: [] => 'X'
  | '\\' :: 'D' :: 'C' :: '1' :: [] => 'X'
  | '\\' :: 'D' :: 'C' :: '2' :: [] => 'X'
  | '\\' :: 'D' :: 'C' :: '3' :: [] => 'X'
  | '\\' :: 'D' :: 'C' :: '4' :: [] => 'X'
  | '\\' :: 'N' :: 'A' :: 'K' :: [] => 'X'
  | '\\' :: 'S' :: 'Y' :: 'N' :: [] => 'X'
  | '\\' :: 'E' :: 'T' :: 'B' :: [] => 'X'
  | '\\' :: 'C' :: 'A' :: 'N' :: [] => 'X'
  | '\\' :: 'E' :: 'M' :: [] => 'X'
  | '\\' :: 'S' :: 'U' :: 'B' :: [] => 'X'
  | '\\' :: 'E' :: 'S' :: 'C' :: [] => 'X'
  | '\\' :: 'F' :: 'S' :: [] => 'X'
  | '\\' :: 'G' :: 'S' :: [] => 'X'
  | '\\' :: 'U' :: 'S' :: [] => 'X'
  | '\\' :: 'S' :: 'P' :: [] => 'X'
  | '\\' :: 'D' :: 'E' :: 'L' :: [] => 'X'
  -- Numeric Escapes
  | '\\' :: 'o' :: _s => 'X'
  | '\\' :: 'x' :: _s => 'X'
  | '\\' :: _s => 'X'
  -- Single characters
  | s :: [] => s
  -- Anything else should not occur
  | _ => 'X'

def Char : RE :=
  apps [ RE.Symbol '\'',
         unions [RE.Symbol ' ', EscapeWithoutAmpersand, GraphicChar],
         RE.Symbol '\'']

def CharR : Rule :=
  Rule.mk Char (λ s => Token.LitChar (parse_char s))

/-
String Literals
-/

def Gap : RE :=
  apps [RE.Symbol '\\', RE.Plus WhiteChar, RE.Symbol '\\']

def String : RE :=
  apps [ RE.Symbol '"',
         RE.Star (unions [GraphicString, RE.Symbol ' ', Escape, Gap]),
         RE.Symbol '"']

def StringR : Rule :=
  Rule.mk String (λ s => Token.LitString s) -- TODO: Have to escape control characters.

/-
All rules declared in this module for literals
-/

def all_literals : List Rule :=
  [IntegerR, FloatR, CharR, StringR]

end Literals
