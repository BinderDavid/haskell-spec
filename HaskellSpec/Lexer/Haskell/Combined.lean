import HaskellSpec.Lexer.Haskell.Literals
import HaskellSpec.Lexer.Haskell.Reserved
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Haskell.Identifier
import Veriflex.Lexer

open Veriflex

def all_rules : List (Rule Token) :=
  Literals.all_literals ++ Reserved.all_reserved ++ [WhitespaceR] ++ all_identifiers


def lex_haskell (s : String) : List Token :=
  (lex s.toList all_rules).fst
