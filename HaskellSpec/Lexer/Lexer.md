# Differences to the Haskell 2010 Language Report

- The Haskell language report uses notation `re_<re>` in order to specify set difference. This is justified by the fact that regular languages are closed under absolute and relative complement. By adjusting a few regular expressions we can remove all uses of complements, which simplifies the implementation of a verified lexer.
- The Haskell language report allows nested comments. This cannot be captured by regular expressions, since it requires to keep track of opening and closing pairs of `{-` and `-}`. This lexical grammar does therefore not support nested comments.
- The Haskell Language Report allows some Unicode characters in identifiers and numeric literals. This lexical grammar currently does not support that. It should be noted that GHC does also not allow unicode in numeric literals.

