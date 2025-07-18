import HaskellSpec.Lexer.RegExp

/-
ASCII Character Classes
-/

def AscLarge : RE :=
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

def Large : RE := AscLarge -- TODO: Missing "uniLarge" unicode

def AscSmall : RE :=
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
           RE.Symbol 'z']


def Small : RE :=-- TODO: Missing "uniSmall"
  RE.Union AscSmall (RE.Symbol '_')

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
Graphic / Any etc
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

def Special : RE :=
  unions [ RE.Symbol '(',
           RE.Symbol ')',
           RE.Symbol ',',
           RE.Symbol ';',
           RE.Symbol '[',
           RE.Symbol ']',
           RE.Symbol '`',
           RE.Symbol '{',
           RE.Symbol '}']

def Graphic : RE :=
  unions [Small, Large, Symbol, Digit, Special, RE.Symbol '"', RE.Symbol '\'']

def Any : RE :=
  unions [ Graphic, RE.Symbol ' ', RE.Symbol '\t']

/-
Comments
-/

def Dashes : RE :=
  RE.Union (RE.App (RE.Symbol '-') (RE.Symbol '-'))
           (apps [RE.Symbol '-', RE.Symbol '-', RE.Symbol '-'])

def Opencom : RE :=
  RE.App (RE.Symbol '{') (RE.Symbol '-')

def Closecom : RE :=
  RE.App (RE.Symbol '-') (RE.Symbol '}')

def Comment : RE := sorry

def NComment : RE := sorry

/-
Whitespace
-/

def Newline : RE :=
  unions [RE.App (RE.Symbol '\r') (RE.Symbol '\n'),
          RE.Symbol '\n',
          RE.Symbol '\r',
          RE.Symbol '\u000C'] -- Form feed

def WhiteChar : RE := -- TODO: Missing "uniWhite"
  unions [ Newline,
           RE.Symbol '\u2B7F', -- Vertical Tab
           RE.Symbol ' ',
           RE.Symbol '\t']

def Whitestuff : RE :=
  unions [ WhiteChar,
           Comment,
           NComment]

def Whitespace : RE :=
  RE.Plus Whitestuff
