import HaskellSpec.Lexer.Brzozowski

import HaskellSpec.Lexer.Haskell.Reserved

#eval max_pref "infixf".toList Reserved.all_reserved
