import HaskellSpec.Lexer.Haskell.Literals
import HaskellSpec.Lexer.Haskell.Reserved
import HaskellSpec.Lexer.Brzozowski

def all_rules : List Rule :=
  Literals.all_literals ++ Reserved.all_reserved


def lex_haskell (s : String) : List Token :=
  (lex s.toList all_rules).fst
