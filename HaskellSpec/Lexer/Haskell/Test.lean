import HaskellSpec.Lexer.Haskell.Combined

#guard lex_haskell "infix" == [Token.Infix]
