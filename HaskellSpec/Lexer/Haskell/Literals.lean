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
           apps [RE.Symbol 'R', RE.Symbol 'S'],
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
  | '\'' :: '\\' :: 'a' :: '\'' :: [] => '\x07' -- alert
  | '\'' :: '\\' :: 'b' :: '\'' :: [] => '\x08' -- backspace
  | '\'' :: '\\' :: 'f' :: '\'' :: [] => '\x0c' -- form feed
  | '\'' :: '\\' :: 'n' :: '\'' :: [] => '\x0a' -- new line / line feed
  | '\'' :: '\\' :: 'r' :: '\'' :: [] => '\x0d' -- carriage return
  | '\'' :: '\\' :: 't' :: '\'' :: [] => '\x09' -- horizontal tab
  | '\'' :: '\\' :: 'v' :: '\'' :: [] => '\x0b' -- vertical tab
  | '\'' :: '\\' :: '\\' :: '\'' :: [] => '\x5c'
  | '\'' :: '\\' :: '"' :: '\'' :: [] => '\x22'
  | '\'' :: '\\' :: '\'' :: '\'' :: [] => '\x27'
  -- Control Escapes
  | '\'' :: '\\' :: '^' :: '@' :: '\'' :: [] => '\x00'
  | '\'' :: '\\' :: '^' :: 'A' :: '\'' :: [] => '\x01'
  | '\'' :: '\\' :: '^' :: 'B' :: '\'' :: [] => '\x02'
  | '\'' :: '\\' :: '^' :: 'C' :: '\'' :: [] => '\x03'
  | '\'' :: '\\' :: '^' :: 'D' :: '\'' :: [] => '\x04'
  | '\'' :: '\\' :: '^' :: 'E' :: '\'' :: [] => '\x05'
  | '\'' :: '\\' :: '^' :: 'F' :: '\'' :: [] => '\x06'
  | '\'' :: '\\' :: '^' :: 'G' :: '\'' :: [] => '\x07'
  | '\'' :: '\\' :: '^' :: 'H' :: '\'' :: [] => '\x08'
  | '\'' :: '\\' :: '^' :: 'I' :: '\'' :: [] => '\x09'
  | '\'' :: '\\' :: '^' :: 'J' :: '\'' :: [] => '\x0a'
  | '\'' :: '\\' :: '^' :: 'K' :: '\'' :: [] => '\x0b'
  | '\'' :: '\\' :: '^' :: 'L' :: '\'' :: [] => '\x0c'
  | '\'' :: '\\' :: '^' :: 'M' :: '\'' :: [] => '\x0d'
  | '\'' :: '\\' :: '^' :: 'N' :: '\'' :: [] => '\x0e'
  | '\'' :: '\\' :: '^' :: 'O' :: '\'' :: [] => '\x0f'
  | '\'' :: '\\' :: '^' :: 'P' :: '\'' :: [] => '\x10'
  | '\'' :: '\\' :: '^' :: 'Q' :: '\'' :: [] => '\x11'
  | '\'' :: '\\' :: '^' :: 'R' :: '\'' :: [] => '\x12'
  | '\'' :: '\\' :: '^' :: 'S' :: '\'' :: [] => '\x13'
  | '\'' :: '\\' :: '^' :: 'T' :: '\'' :: [] => '\x14'
  | '\'' :: '\\' :: '^' :: 'U' :: '\'' :: [] => '\x15'
  | '\'' :: '\\' :: '^' :: 'V' :: '\'' :: [] => '\x16'
  | '\'' :: '\\' :: '^' :: 'W' :: '\'' :: [] => '\x17'
  | '\'' :: '\\' :: '^' :: 'X' :: '\'' :: [] => '\x18'
  | '\'' :: '\\' :: '^' :: 'Y' :: '\'' :: [] => '\x19'
  | '\'' :: '\\' :: '^' :: 'Z' :: '\'' :: [] => '\x1a'
  | '\'' :: '\\' :: '^' :: '[' :: '\'' :: [] => '\x1b'
  | '\'' :: '\\' :: '^' :: '\\' :: '\'' :: [] => '\x1c'
  | '\'' :: '\\' :: '^' :: ']' :: '\'' :: [] => '\x1d'
  | '\'' :: '\\' :: '^' :: '^' :: '\'' :: [] => '\x1e'
  | '\'' :: '\\' :: '^' :: '_' :: '\'' :: [] => '\x1f'
  -- ASCII Escapes
  | '\'' :: '\\' :: 'N' :: 'U' :: 'L' :: '\'' :: [] => '\x00'
  | '\'' :: '\\' :: 'S' :: 'O' :: 'H' :: '\'' :: [] => '\x01'
  | '\'' :: '\\' :: 'S' :: 'T' :: 'X' :: '\'' :: [] => '\x02'
  | '\'' :: '\\' :: 'E' :: 'T' :: 'X' :: '\'' :: [] => '\x03'
  | '\'' :: '\\' :: 'E' :: 'O' :: 'T' :: '\'' :: [] => '\x04'
  | '\'' :: '\\' :: 'E' :: 'N' :: 'Q' :: '\'' :: [] => '\x05'
  | '\'' :: '\\' :: 'A' :: 'C' :: 'K' :: '\'' :: [] => '\x06'
  | '\'' :: '\\' :: 'B' :: 'E' :: 'L' :: '\'' :: [] => '\x07'
  | '\'' :: '\\' :: 'B' :: 'S' :: '\'' :: [] => '\x08'
  | '\'' :: '\\' :: 'H' :: 'T' :: '\'' :: [] => '\x09'
  | '\'' :: '\\' :: 'L' :: 'F' :: '\'' :: [] => '\x0a'
  | '\'' :: '\\' :: 'V' :: 'T' :: '\'' :: [] => '\x0b'
  | '\'' :: '\\' :: 'F' :: 'F' :: '\'' :: [] => '\x0c'
  | '\'' :: '\\' :: 'C' :: 'R' :: '\'' :: [] => '\x0d'
  | '\'' :: '\\' :: 'S' :: 'O' :: '\'' :: [] => '\x0e'
  | '\'' :: '\\' :: 'S' :: 'I' :: '\'' :: [] => '\x0f'
  | '\'' :: '\\' :: 'D' :: 'L' :: 'E' :: '\'' :: [] => '\x10'
  | '\'' :: '\\' :: 'D' :: 'C' :: '1' :: '\'' :: [] => '\x11'
  | '\'' :: '\\' :: 'D' :: 'C' :: '2' :: '\'' :: [] => '\x12'
  | '\'' :: '\\' :: 'D' :: 'C' :: '3' :: '\'' :: [] => '\x13'
  | '\'' :: '\\' :: 'D' :: 'C' :: '4' :: '\'' :: [] => '\x14'
  | '\'' :: '\\' :: 'N' :: 'A' :: 'K' :: '\'' :: [] => '\x15'
  | '\'' :: '\\' :: 'S' :: 'Y' :: 'N' :: '\'' :: [] => '\x16'
  | '\'' :: '\\' :: 'E' :: 'T' :: 'B' :: '\'' :: [] => '\x17'
  | '\'' :: '\\' :: 'C' :: 'A' :: 'N' :: '\'' :: [] => '\x18'
  | '\'' :: '\\' :: 'E' :: 'M' :: '\'' :: [] => '\x19'
  | '\'' :: '\\' :: 'S' :: 'U' :: 'B' :: '\'' :: [] => '\x1a'
  | '\'' :: '\\' :: 'E' :: 'S' :: 'C' :: '\'' :: [] => '\x1b'
  | '\'' :: '\\' :: 'F' :: 'S' :: '\'' :: [] => '\x1c'
  | '\'' :: '\\' :: 'G' :: 'S' :: '\'' :: [] => '\x1d'
  | '\'' :: '\\' :: 'R' :: 'S' :: '\'' :: [] => '\x1e'
  | '\'' :: '\\' :: 'U' :: 'S' :: '\'' :: [] => '\x1f'
  | '\'' :: '\\' :: 'S' :: 'P' :: '\'' :: [] => '\x20'
  | '\'' :: '\\' :: 'D' :: 'E' :: 'L' :: '\'' :: [] => '\x7f'
  -- Numeric Escapes
  | '\'' :: '\\' :: 'o' :: _s => 'X'
  | '\'' :: '\\' :: 'x' :: _s => 'X'
  | '\'' :: '\\' :: _s => 'X'
  -- Single characters
  | '\'' :: s :: '\'' :: [] => s
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
