import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Tokens

namespace Reserved
/-
Symbols and reserved operators
-/

def OpenParen : RE := RE.Symbol '('

def CloseParen : RE := RE.Symbol ')'

def OpenBracket : RE := RE.Symbol '['

def CloseBracket : RE := RE.Symbol ']'

def OpenBrace : RE := RE.Symbol '{'

def CloseBrace : RE := RE.Symbol '}'

def Comma : RE := RE.Symbol ','

def Colon : RE := RE.Symbol ':'

def DoubleColon : RE := RE.App (RE.Symbol ':') (RE.Symbol ':')

def Semicolon : RE := RE.Symbol ';'

def Backtick : RE := RE.Symbol '`'

def Backslash : RE := RE.Symbol '\\'

def DotDot : RE := RE.App (RE.Symbol '.') (RE.Symbol '.')

def Equal : RE := RE.Symbol '='

def ArrowLeft : RE := RE.App (RE.Symbol '<') (RE.Symbol '-')

def ArrowRight : RE := RE.App (RE.Symbol '-') (RE.Symbol '>')

def DoubleArrowRight : RE := RE.App (RE.Symbol '=') (RE.Symbol '>')

def Tilde : RE := RE.Symbol '~'

def Ampersand : RE := RE.Symbol '@'

def Solidus : RE := RE.Symbol '|'

def Underscore : RE := RE.Symbol '_'

def ReservedOp : RE :=
  unions [ DotDot,
           Colon,
           DoubleColon,
           Equal,
           Backslash,
           Solidus,
           ArrowLeft,
           ArrowRight,
           DoubleArrowRight,
           Ampersand,
           Tilde]

/-
Reserved Keywords
-/

def Case : RE := from_string ['c','a','s','e']

def Class : RE := from_string ['c','l','a','s','s']

def Data : RE := from_string ['d','a','t','a']

def Default : RE := from_string ['d','e','f','a','u','l','t']

def Deriving : RE := from_string ['d','e','r','i','v','i','n','g']

def Do : RE := from_string ['d','o']

def Else : RE := from_string ['e','l','s','e']

def Foreign : RE := from_string ['f','o','r','e','i','g','n']

def If : RE := from_string ['i','f']

def Import : RE := from_string ['i','m','p','o','r','t']

def In : RE := from_string ['i','n']

def Infix : RE := from_string ['i','n','f','i','x']

def Infixl : RE := from_string ['i','n','f','i','x','l']

def Infixr : RE := from_string ['i','n','f','i','x','r']

def Instance : RE := from_string ['i','n','s','t','a','n','c','e']

def Let : RE := from_string ['l','e','t']

def Module : RE := from_string ['m','o','d','u','l','e']

def Newtype : RE := from_string ['n','e','w','t','y','p','e']

def Of : RE := from_string ['o','f']

def Then : RE := from_string ['t','h','e','n']

def TypeT : RE := from_string ['t','y','p','e']

def Where : RE := from_string ['w','h','e','r','e']

def ReservedId : RE :=
  unions [ Case,
           Class,
           Data,
           Default,
           Deriving,
           Do,
           Else,
           Foreign,
           If,
           Import,
           In,
           Infix,
           Infixl,
           Infixr,
           Instance,
           Let,
           Module,
           Newtype,
           Of,
           Then,
           TypeT,
           Where,
           Underscore]

end Reserved
