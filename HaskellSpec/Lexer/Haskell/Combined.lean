import HaskellSpec.Lexer.Haskell.Literals
import HaskellSpec.Lexer.Haskell.Reserved
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Haskell.Identifier
import HaskellSpec.Lexer.Brzozowski

def all_rules : List Rule :=
  all_identifiers ++ Literals.all_literals ++ Reserved.all_reserved ++ [WhitespaceR]


def lex_haskell (s : String) : List Token :=
  (lex s.toList all_rules).fst
