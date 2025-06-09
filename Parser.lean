import Parser
import HaskellSpec.Source.SourceLang
import HaskellSpec.Names

open Parser Char

def parseLiteral : SimpleParser Substring Char Source.Literal :=
 Source.Literal.char <$> (char '\'' *> ASCII.alphanum <* char '\'')
 <|> Source.Literal.string <$>
 (char '"' *> ((String.mk ∘ Array.toList ∘ Prod.fst) <$> takeUntil (char '"') ASCII.alphanum ))
 <|> Source.Literal.float <$> (lookAhead (dropUntil (char '.' <|> char 'e') ASCII.digit) *> ASCII.parseFloat)
 <|> Source.Literal.integer <$> ASCII.parseInt


def testParser  {α : Type} [inst : BEq α] (p : SimpleParser Substring Char α) (str : String) (exp : α) : Bool :=
  match Parser.run p str with
    | .ok s r => s == "" && r == exp
    | _ => false

def testLiteralChar : Bool :=
  match Parser.run parseLiteral "\'a\'" with
  | .ok s (.char c) => s == "" && c == 'a'
  | _ => false

#guard testLiteralChar


def testLiteralString : Bool :=
  match Parser.run parseLiteral "\"foo\"" with
  | .ok sRest (.string s) => sRest == "" && s == "foo"
  | _ => false

#guard testLiteralString

def testLiteralInteger : Bool :=
  match Parser.run parseLiteral "42" with
  | .ok s (.integer i) => s == "" && i == 42
  | _ => false

#guard testLiteralInteger


def testLiteralFloat : Bool :=
  match Parser.run parseLiteral "42.42" with
  | .ok s (.float f) => s == "" && f == 42.42
  | _ => false

#guard testLiteralFloat

def testLiteralFloatExponential : Bool :=
  match Parser.run parseLiteral "42e42" with
  | .ok s (.float f) => s == "" && f == 42e42
  | _ => false

#guard testLiteralFloatExponential

def parseQualifier : SimpleParser Substring Char Source.Qualifier :=
  string "qualified" *> pure Source.Qualifier.qualified
  <|> pure Source.Qualifier.unqualified

def testQualifier : Bool :=
  match Parser.run parseQualifier "qualified" with
  | .ok s .qualified => s == ""
  | _ => false

#guard testQualifier

def parseModuleName : SimpleParser Substring Char Module_Name :=
  (λ c cs => Module_Name.Mk ∘ String.mk ∘ List.cons c $ Array.toList cs ) <$> ASCII.uppercase <*> takeMany ASCII.alphanum

def testParseModuleName : Bool :=
  match Parser.run parseModuleName "Mod" with
    | .ok s (.Mk v) => s == "" && v == "Mod"
    | _ => false

#guard testParseModuleName

def testParseModuleNameLowerFails : Bool :=
  match Parser.run parseModuleName "mod" with
    | .ok _ _ => false
    | _ => true

#guard testParseModuleNameLowerFails

def parseVariable : SimpleParser Substring Char Variable :=
  (λ c cs => Variable.Mk ∘ String.mk ∘ List.cons c $ Array.toList cs ) <$> ASCII.lowercase <*> takeMany ASCII.alphanum

def testParseVariable : Bool :=
  match Parser.run parseVariable "var" with
    | .ok s (.Mk v) => s == "" && v == "var"
    | _ => false

#guard testParseVariable

def testParseVariableUpperFails : Bool :=
  match Parser.run parseVariable "Var" with
    | .ok _ _ => false
    | _ => true

#guard testParseVariableUpperFails


def parseQVarible : SimpleParser Substring Char QVariable :=
  QVariable.Unqualified <$> parseVariable
 -- Qualified original only exist in target language

def testParseQVariableUnqualfied : Bool :=
  match Parser.run parseQVarible "var" with
    | .ok s (.Unqualified (.Mk v)) => s == "" && v == "var"
    | _ => false

#guard testParseQVariableUnqualfied


def parseSpecialDataConstructor : SimpleParser Substring Char Special_Data_Constructor :=
   char '(' *> dropMany (char ' ') *> char ')' *> pure Special_Data_Constructor.Unit
   <|> Special_Data_Constructor.Tuple <$> (char '(' *> dropMany (char ' ') *> ASCII.parseNat <* dropMany (char ' ') <* char ')')
   <|> char '[' *> dropMany (char ' ') *> char ']' *> pure Special_Data_Constructor.Nil
   <|> char ':' *> pure Special_Data_Constructor.Cons

