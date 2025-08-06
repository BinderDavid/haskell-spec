import Veriflex.RegExp
import Veriflex.Grammar
import HaskellSpec.Lexer.Haskell.Tokens

open Veriflex

namespace Reserved
/-
Symbols and reserved operators
-/

def OpenParen : RE := RE.Symbol '('
def OpenParenR : Rule Token := Rule.mk OpenParen (λ _ => Token.OpenParen)

def CloseParen : RE := RE.Symbol ')'
def CloseParenR : Rule Token := Rule.mk CloseParen (λ _ => Token.CloseParen)

def OpenBracket : RE := RE.Symbol '['
def OpenBracketR : Rule Token := Rule.mk OpenBracket (λ _ => Token.OpenBracket)

def CloseBracket : RE := RE.Symbol ']'
def CloseBracketR : Rule Token := Rule.mk CloseBracket (λ _ => Token.CloseBracket)

def OpenBrace : RE := RE.Symbol '{'
def OpenBraceR : Rule Token := Rule.mk OpenBrace (λ _ => Token.OpenBrace)

def CloseBrace : RE := RE.Symbol '}'
def CloseBraceR : Rule Token := Rule.mk CloseBrace (λ _ => Token.CloseBrace)

def Comma : RE := RE.Symbol ','
def CommaR : Rule Token := Rule.mk Comma (λ _ => Token.Comma)

def Colon : RE := RE.Symbol ':'
def ColonR : Rule Token := Rule.mk Colon (λ _ => Token.Colon)

def DoubleColon : RE := RE.App (RE.Symbol ':') (RE.Symbol ':')
def DoubleColonR : Rule Token := Rule.mk DoubleColon (λ _ => Token.DoubleColon)

def Semicolon : RE := RE.Symbol ';'
def SemicolonR : Rule Token := Rule.mk Semicolon (λ _ => Token.Semicolon)

def Backtick : RE := RE.Symbol '`'
def BacktickR : Rule Token := Rule.mk Backtick (λ _ => Token.Backtick)

def Backslash : RE := RE.Symbol '\\'
def BackslashR : Rule Token := Rule.mk Backslash (λ _ => Token.Backslash)

def DotDot : RE := RE.App (RE.Symbol '.') (RE.Symbol '.')
def DotDotR : Rule Token := Rule.mk DotDot (λ _ => Token.DotDot)

def Equal : RE := RE.Symbol '='
def EqualR : Rule Token := Rule.mk Equal (λ _ => Token.Equal)

def ArrowLeft : RE := RE.App (RE.Symbol '<') (RE.Symbol '-')
def ArrowLeftR : Rule Token := Rule.mk ArrowLeft (λ _ => Token.ArrowLeft)

def ArrowRight : RE := RE.App (RE.Symbol '-') (RE.Symbol '>')
def ArrowRightR : Rule Token := Rule.mk ArrowRight (λ _ => Token.ArrowRight)

def DoubleArrowRight : RE := RE.App (RE.Symbol '=') (RE.Symbol '>')
def DoubleArrowRightR : Rule Token := Rule.mk DoubleArrowRight (λ _ => Token.DoubleArrowRight)

def Tilde : RE := RE.Symbol '~'
def TildeR : Rule Token := Rule.mk Tilde (λ _ => Token.Tilde)

def Ampersand : RE := RE.Symbol '@'
def AmpersandR : Rule Token := Rule.mk Ampersand (λ _ => Token.Ampersand)

def Solidus : RE := RE.Symbol '|'
def SolidusR : Rule Token := Rule.mk Solidus (λ _ => Token.Solidus)

def Underscore : RE := RE.Symbol '_'
def UnderscoreR : Rule Token := Rule.mk Underscore (λ _ => Token.Underscore)


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
def As : RE := from_string ['a','s']
def AsR : Rule Token := Rule.mk As (λ _ => Token.As)

def Case : RE := from_string ['c','a','s','e']
def CaseR : Rule Token := Rule.mk Case (λ _ => Token.Case)

def Class : RE := from_string ['c','l','a','s','s']
def ClassR : Rule Token := Rule.mk Class (λ _ => Token.Class)

def Data : RE := from_string ['d','a','t','a']
def DataR : Rule Token := Rule.mk Data (λ _ => Token.Data)

def Default : RE := from_string ['d','e','f','a','u','l','t']
def DefaultR : Rule Token := Rule.mk Default  (λ _ => Token.Default)

def Deriving : RE := from_string ['d','e','r','i','v','i','n','g']
def DerivingR : Rule Token := Rule.mk Deriving (λ _ => Token.Deriving)

def Do : RE := from_string ['d','o']
def DoR : Rule Token := Rule.mk Do (λ _ => Token.Do)

def Else : RE := from_string ['e','l','s','e']
def ElseR : Rule Token := Rule.mk Else (λ _ => Token.Else)

def Foreign : RE := from_string ['f','o','r','e','i','g','n']
def ForeignR : Rule Token := Rule.mk Foreign (λ _ => Token.Foreign)

def If : RE := from_string ['i','f']
def IfR : Rule Token := Rule.mk If (λ _ => Token.If)

def Import : RE := from_string ['i','m','p','o','r','t']
def ImportR : Rule Token := Rule.mk Import (λ _ => Token.Import)

def In : RE := from_string ['i','n']
def InR : Rule Token := Rule.mk In (λ _ => Token.In)

def Infix : RE := from_string ['i','n','f','i','x']
def InfixR : Rule Token := Rule.mk Infix (λ _ => Token.Infix)

def Infixl : RE := from_string ['i','n','f','i','x','l']
def InfixlR : Rule Token := Rule.mk Infixl (λ _ => Token.Infixl)

def Infixr : RE := from_string ['i','n','f','i','x','r']
def InfixrR : Rule Token := Rule.mk Infixr (λ _ => Token.Infixr)

def Instance : RE := from_string ['i','n','s','t','a','n','c','e']
def InstanceR : Rule Token := Rule.mk Instance (λ _ => Token.Instance)

def Let : RE := from_string ['l','e','t']
def LetR : Rule Token := Rule.mk Let (λ _ => Token.Let)

def Module : RE := from_string ['m','o','d','u','l','e']
def ModuleR : Rule Token := Rule.mk Module (λ _ => Token.Module)

def Newtype : RE := from_string ['n','e','w','t','y','p','e']
def NewtypeR : Rule Token := Rule.mk Newtype (λ _ => Token.Newtype)

def Of : RE := from_string ['o','f']
def OfR : Rule Token := Rule.mk Of (λ _ => Token.Of)

-- Not reserved, can be used as ident.
def Qualified : RE := from_string ['q','u','a','l','i','f','i','e','d']
def QualifiedR : Rule Token := Rule.mk Qualified (λ _ => Token.Qualified)

def Then : RE := from_string ['t','h','e','n']
def ThenR : Rule Token := Rule.mk Then (λ _ => Token.Then)

def TypeT : RE := from_string ['t','y','p','e']
def TypeTR : Rule Token := Rule.mk TypeT (λ _ => Token.TypeT)

def Where : RE := from_string ['w','h','e','r','e']
def WhereR : Rule Token := Rule.mk Where (λ _ => Token.Where)

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

def all_reserved : List (Rule Token) :=
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
    /- Semi Keywords: Can be used as identifiers -/
    AsR,
    QualifiedR,
    UnderscoreR,
  ]

end Reserved
