import HaskellSpec.Lexer.Brzozowski

import HaskellSpec.Lexer.Haskell.Reserved

#eval maxpref_one "infixfoo".toList Reserved.Infix
