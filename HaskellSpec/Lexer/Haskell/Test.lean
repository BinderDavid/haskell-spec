import HaskellSpec.Lexer.Brzozowski

import HaskellSpec.Lexer.Haskell.Reserved

#eval lex "infixf".toList Reserved.all_reserved
