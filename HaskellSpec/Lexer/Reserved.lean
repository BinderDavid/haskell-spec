import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Tokens
import HaskellSpec.Lexer.Rules

namespace Reserved
/-
Symbols and reserved operators
-/

def OpenParen : RE := RE.Symbol '('
def OpenParenR : Rule := Rule.mk OpenParen (λ _ => Token.OpenParen)

def CloseParen : RE := RE.Symbol ')'
def CloseParenR : Rule := Rule.mk CloseParen (λ _ => Token.CloseParen)

def OpenBracket : RE := RE.Symbol '['
def OpenBracketR : Rule := Rule.mk OpenBracket (λ _ => Token.OpenBracket)

def CloseBracket : RE := RE.Symbol ']'
def CloseBracketR : Rule := Rule.mk CloseBracket (λ _ => Token.CloseBracket)

def OpenBrace : RE := RE.Symbol '{'
def OpenBraceR : Rule := Rule.mk OpenBrace (λ _ => Token.OpenBrace)

def CloseBrace : RE := RE.Symbol '}'
def CloseBraceR : Rule := Rule.mk CloseBrace (λ _ => Token.CloseBrace)

def Comma : RE := RE.Symbol ','
def CommaR : Rule := Rule.mk Comma (λ _ => Token.Comma)

def Colon : RE := RE.Symbol ':'
def ColonR : Rule := Rule.mk Colon (λ _ => Token.Colon)

def DoubleColon : RE := RE.App (RE.Symbol ':') (RE.Symbol ':')
def DoubleColonR : Rule := Rule.mk DoubleColon (λ _ => Token.DoubleColon)

def Semicolon : RE := RE.Symbol ';'
def SemicolonR : Rule := Rule.mk Semicolon (λ _ => Token.Semicolon)

def Backtick : RE := RE.Symbol '`'
def BacktickR : Rule := Rule.mk Backtick (λ _ => Token.Backtick)

def Backslash : RE := RE.Symbol '\\'
def BackslashR : Rule := Rule.mk Backslash (λ _ => Token.Backslash)

def DotDot : RE := RE.App (RE.Symbol '.') (RE.Symbol '.')
def DotDotR : Rule := Rule.mk DotDot (λ _ => Token.DotDot)

def Equal : RE := RE.Symbol '='
def EqualR : Rule := Rule.mk Equal (λ _ => Token.Equal)

def ArrowLeft : RE := RE.App (RE.Symbol '<') (RE.Symbol '-')
def ArrowLeftR : Rule := Rule.mk ArrowLeft (λ _ => Token.ArrowLeft)

def ArrowRight : RE := RE.App (RE.Symbol '-') (RE.Symbol '>')
def ArrowRightR : Rule := Rule.mk ArrowRight (λ _ => Token.ArrowRight)

def DoubleArrowRight : RE := RE.App (RE.Symbol '=') (RE.Symbol '>')
def DoubleArrowRightR : Rule := Rule.mk DoubleArrowRight (λ _ => Token.DoubleArrowRight)

def Tilde : RE := RE.Symbol '~'
def TildeR : Rule := Rule.mk Tilde (λ _ => Token.Tilde)

def Ampersand : RE := RE.Symbol '@'
def AmpersandR : Rule := Rule.mk Ampersand (λ _ => Token.Ampersand)

def Solidus : RE := RE.Symbol '|'
def SolidusR : Rule := Rule.mk Solidus (λ _ => Token.Solidus)

def Underscore : RE := RE.Symbol '_'
def UnderscoreR : Rule := Rule.mk Underscore (λ _ => Token.Solidus)

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
def CaseR : Rule := Rule.mk Case (λ _ => Token.Case)

def Class : RE := from_string ['c','l','a','s','s']
def ClassR : Rule := Rule.mk Class (λ _ => Token.Class)

def Data : RE := from_string ['d','a','t','a']
def DataR : Rule := Rule.mk Data (λ _ => Token.Data)

def Default : RE := from_string ['d','e','f','a','u','l','t']
def DefaultR : Rule := Rule.mk Default  (λ _ => Token.Default)

def Deriving : RE := from_string ['d','e','r','i','v','i','n','g']
def DerivingR : Rule := Rule.mk Deriving (λ _ => Token.Deriving)

def Do : RE := from_string ['d','o']
def DoR : Rule := Rule.mk Do (λ _ => Token.Do)

def Else : RE := from_string ['e','l','s','e']
def ElseR : Rule := Rule.mk Else (λ _ => Token.Else)

def Foreign : RE := from_string ['f','o','r','e','i','g','n']
def ForeignR : Rule := Rule.mk Foreign (λ _ => Token.Foreign)

def If : RE := from_string ['i','f']
def IfR : Rule := Rule.mk If (λ _ => Token.If)

def Import : RE := from_string ['i','m','p','o','r','t']
def ImportR : Rule := Rule.mk Import (λ _ => Token.If)

def In : RE := from_string ['i','n']
def InR : Rule := Rule.mk In (λ _ => Token.In)

def Infix : RE := from_string ['i','n','f','i','x']
def InfixR : Rule := Rule.mk Infix (λ _ => Token.Infix)

def Infixl : RE := from_string ['i','n','f','i','x','l']
def InfixlR : Rule := Rule.mk Infixl (λ _ => Token.Infixl)

def Infixr : RE := from_string ['i','n','f','i','x','r']
def InfixrR : Rule := Rule.mk Infixr (λ _ => Token.Infixr)

def Instance : RE := from_string ['i','n','s','t','a','n','c','e']
def InstanceR : Rule := Rule.mk Instance (λ _ => Token.Instance)

def Let : RE := from_string ['l','e','t']
def LetR : Rule := Rule.mk Let (λ _ => Token.Let)

def Module : RE := from_string ['m','o','d','u','l','e']
def ModuleR : Rule := Rule.mk Module (λ _ => Token.Module)

def Newtype : RE := from_string ['n','e','w','t','y','p','e']
def NewtypeR : Rule := Rule.mk Newtype (λ _ => Token.Newtype)

def Of : RE := from_string ['o','f']
def OfR : Rule := Rule.mk Of (λ _ => Token.Of)

def Then : RE := from_string ['t','h','e','n']
def ThenR : Rule := Rule.mk Then (λ _ => Token.Then)

def TypeT : RE := from_string ['t','y','p','e']
def TypeTR : Rule := Rule.mk TypeT (λ _ => Token.TypeT)

def Where : RE := from_string ['w','h','e','r','e']
def WhereR : Rule := Rule.mk Where (λ _ => Token.Where)

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

/-
All rules declared in this module for reserved keywords and operators
-/

def all_reserved : List Rule :=
  [ OpenParenR,
    CloseParenR,
    OpenBracketR,
    CloseBracketR,
    OpenBraceR,
    CloseBraceR,
    CommaR,
    ColonR,
    DoubleColonR,
    SemicolonR,
    BacktickR,
    BackslashR,
    DotDotR,
    EqualR,
    ArrowLeftR,
    ArrowRightR,
    DoubleArrowRightR,
    TildeR,
    AmpersandR,
    SolidusR,
    UnderscoreR,
    /- Keywords-/
    CaseR,
    ClassR,
    DataR,
    DefaultR,
    DerivingR,
    DoR,
    ElseR,
    ForeignR,
    IfR,
    ImportR,
    InR,
    InfixR,
    InfixlR,
    InfixrR,
    InstanceR,
    LetR,
    ModuleR,
    NewtypeR,
    OfR,
    ThenR,
    TypeTR,
    WhereR,
  ]

end Reserved
