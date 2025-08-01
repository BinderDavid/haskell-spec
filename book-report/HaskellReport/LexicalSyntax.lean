import VersoManual
import HaskellReport.Papers
import HaskellSpec.Lexer.Haskell.Combined

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

set_option pp.rawOnError true

#doc (Manual) "Lexical Syntax" =>

Description of lexical syntax...

# Keywords

# Symbols

# Literals

## Integer Literals

```lean
#eval lex_haskell "42"
#eval lex_haskell "0x42"
#eval lex_haskell "0X42"
#eval lex_haskell "0o42"
#eval lex_haskell "0O42"
```

## Float Literals

```lean
#eval lex_haskell "42.0"
```

## Character Literals

```lean
#eval lex_haskell "'a'"
```

## String Literals

```lean
#eval lex_haskell "\"hello world\""
```

# Comments

## Line Comments

```lean
#eval lex_haskell "-- some comment"
```

## Block Comments

```lean
#eval lex_haskell "{- some comment -}"
```
