import VersoManual
import HaskellReport.Papers
import HaskellSpec.Lexer.Haskell.Combined
import HaskellSpec.Lexer.Columnizer

open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

set_option pp.rawOnError true

#doc (Manual) "Lexical Syntax" =>

Description of lexical syntax...

# Column Indentation

The “columnizer“ assigns logical columns to characters in a string according
to the rules of section 10.3 of the Haskell2010 language report. That section specifies that:

The “indentation” of a lexeme is the column number of the first character of that lexeme; the indentation of a line is the indentation of its leftmost lexeme. To determine the column number, assume a fixed-width font with the following conventions:

- The characters newline, return, linefeed, and formfeed, all start a new line.
- The first column is designated column 1, not 0.
- Tab stops are 8 characters apart.
- A tab character causes the insertion of enough spaces to align the current position with the next tab stop.

```lean
#eval columnizer ['a', 'b', 'c']
#eval columnizer ['a', 'b', '\n', 'c', 'd']
#eval columnizer ['a', 'b', '\r', 'c', 'd']
#eval columnizer ['a', 'b', '\u000C', 'c', 'd']
#eval columnizer ['a', 'b', '\r', '\n', 'c', 'd']
#eval columnizer ['a', '\t', 'b']
```

# Keywords

The following list of keywords are reserved and cannot be used as identifiers

```lean
#eval lex_haskell "case"
#eval lex_haskell "class"
#eval lex_haskell "data"
#eval lex_haskell "default"
#eval lex_haskell "deriving"
#eval lex_haskell "do"
#eval lex_haskell "else"
#eval lex_haskell "foreign"
#eval lex_haskell "if"
#eval lex_haskell "import"
#eval lex_haskell "in"
#eval lex_haskell "infix"
#eval lex_haskell "infixl"
#eval lex_haskell "infixr"
#eval lex_haskell "instance"
#eval lex_haskell "let"
#eval lex_haskell "module"
#eval lex_haskell "newtype"
#eval lex_haskell "of"
#eval lex_haskell "then"
#eval lex_haskell "type"
#eval lex_haskell "where"
```

The following list of keywords are lexed as separate tokens, since they appear explicitly in some grammar productions, but they can also be used as identifiers.

```lean
#eval lex_haskell "_"
#eval lex_haskell "as"
#eval lex_haskell "qualified"
```

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
