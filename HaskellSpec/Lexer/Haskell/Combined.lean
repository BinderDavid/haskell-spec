import HaskellSpec.Lexer.Haskell.Literals
import HaskellSpec.Lexer.Haskell.Reserved
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Haskell.Identifier
import HaskellSpec.Lexer.Columnizer
import Veriflex.Lexer
import Veriflex.Located

open Veriflex
open Columnizer

def all_rules : Grammar Token :=
  Literals.all_literals ++ Reserved.all_reserved ++ [WhitespaceR] ++ all_identifiers


def lex_haskell_located (s : String) : List LToken :=
  (lex all_rules (columnizer s.toList)).fst

def lex_haskell (s : String) : List Token :=
  contents (lex_haskell_located s)
