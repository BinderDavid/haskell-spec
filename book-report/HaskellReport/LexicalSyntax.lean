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

Haskell supports two styles of comments, line comments and block comments.

## Line Comments

A line comment begins with a sequence of two or more consecutive dashes (e.g. "--") and extends to the following newline.
The sequence of dashes must not form part of a legal lexeme.
For example, "-->" or "|--" do not begin a comment, because both of these are legal lexemes; however "--foo" does start a comment.

```lean
#eval lex_haskell "-- some comment"
```

## Block Comments

```lean
#eval lex_haskell "{- some comment -}"
```
