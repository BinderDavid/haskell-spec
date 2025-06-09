import Parser
import HaskellSpec.Source.SourceLang
import HaskellSpec.Names

open Parser Char

abbrev HaskellParser := SimpleParser Substring Char

def testParser  {α : Type} [inst : BEq α] (p : HaskellParser α) (str : String) (exp : α) : Bool :=
  match Parser.run p str with
    | .ok s r => s == "" && r == exp
    | _ => false

def testParserFails  {α : Type} (p : HaskellParser α) (str : String) : Bool :=
  match Parser.run p str with
    | .ok _ _ => false
    | _ => true

def parseLiteral : HaskellParser Source.Literal :=
 Source.Literal.char <$> (char '\'' *> ASCII.alphanum <* char '\'')
 <|> Source.Literal.string <$>
 (char '"' *> ((String.mk ∘ Array.toList ∘ Prod.fst) <$> takeUntil (char '"') ASCII.alphanum ))
 <|> Source.Literal.float <$> (lookAhead (dropUntil (char '.' <|> char 'e') ASCII.digit) *> ASCII.parseFloat)
 <|> Source.Literal.integer <$> ASCII.parseInt

#guard testParser parseLiteral "\'a\'" (.char 'a') 
#guard testParser parseLiteral "\"foo\"" (.string "foo")
#guard testParser parseLiteral "42" (.integer 42)
#guard testParser parseLiteral "42.42" (.float 42.42)
#guard testParser parseLiteral "42e42" (.float 42e42)

def parseQualifier : HaskellParser Source.Qualifier :=
  string "qualified" *> pure Source.Qualifier.qualified
  <|> pure Source.Qualifier.unqualified

#guard testParser parseQualifier "qualified"  .qualified 

def parseModuleName : HaskellParser Module_Name :=
  (λ c cs => Module_Name.Mk ∘ String.mk ∘ List.cons c $ Array.toList cs ) <$> ASCII.uppercase <*> takeMany ASCII.alphanum

#guard testParser parseModuleName "Mod" (.Mk "Mod")
#guard testParserFails parseModuleName "mod" 

def parseVariable : HaskellParser Variable :=
  (λ c cs => Variable.Mk ∘ String.mk ∘ List.cons c $ Array.toList cs ) <$> ASCII.lowercase <*> takeMany ASCII.alphanum

#guard testParser parseVariable "var" (.Mk "var")
#guard testParserFails parseVariable "Var"

def parseQVarible : HaskellParser QVariable :=
  QVariable.Unqualified <$> parseVariable
 -- Qualified original only exist in target language

#guard testParser parseQVarible "var" (.Unqualified (.Mk "var"))

def parseSpecialDataConstructor : HaskellParser Special_Data_Constructor :=
   char '(' *> dropMany (char ' ') *> char ')' *> pure Special_Data_Constructor.Unit
   <|> Special_Data_Constructor.Tuple <$> (char '(' *> dropMany (char ' ') *> ASCII.parseNat <* dropMany (char ' ') <* char ')')
   <|> char '[' *> dropMany (char ' ') *> char ']' *> pure Special_Data_Constructor.Nil
   <|> char ':' *> pure Special_Data_Constructor.Cons

#guard testParser parseSpecialDataConstructor "()" Special_Data_Constructor.Unit
#guard testParser parseSpecialDataConstructor "(  )" Special_Data_Constructor.Unit
#guard testParser parseSpecialDataConstructor "(2)" (Special_Data_Constructor.Tuple 2)
#guard testParser parseSpecialDataConstructor "(  3   )" (Special_Data_Constructor.Tuple 3)
#guard testParser parseSpecialDataConstructor "[]" Special_Data_Constructor.Nil
#guard testParser parseSpecialDataConstructor "[   ]" Special_Data_Constructor.Nil
#guard testParser parseSpecialDataConstructor ":" Special_Data_Constructor.Cons

def parseConstrutor : HaskellParser Constructor :=
  (λ c cs => Constructor.Mk ∘ String.mk ∘ List.cons c $ Array.toList cs) <$> ASCII.uppercase <*> takeMany ASCII.alphanum

#guard testParser parseConstrutor "Cons" (Constructor.Mk "Cons")
