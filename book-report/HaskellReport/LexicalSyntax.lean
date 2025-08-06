import VersoManual
import HaskellReport.Papers
import HaskellSpec.Lexer.Haskell.Combined
import HaskellSpec.Lexer.Haskell.Literals
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Columnizer
import Veriflex.RegExp

open Veriflex
open Verso.Genre Manual
open Verso.Genre.Manual.InlineLean

set_option pp.rawOnError true

#doc (Manual) "Lexical Syntax" =>

This chapter describes the lexical syntax of Haskell 2010 by specifying how a list of characters can be transformed into a list of tokens annotated with column information.
Tokens have to be annotated with their logical column so that the indentation sensitive context free grammar can be implemented correctly.
The following sections describe how the the logical columns of individual characters are computed according to the Haskell report, the semantics of regular expressions and the maximum munch principle, before then discussing the individual grammar productions for keywords, symbols, identifiers, literals, comments and whitespace.


# Column Indentation

The “columnizer“ assigns logical columns to characters in a string according
to the rules of section 10.3 of the Haskell2010 language report. That section specifies that:

The “indentation” of a lexeme is the column number of the first character of that lexeme; the indentation of a line is the indentation of its leftmost lexeme. To determine the column number, assume a fixed-width font with the following conventions:

- The characters newline, return, linefeed, and formfeed, all start a new line.
- The first column is designated column 1, not 0.
- Tab stops are 8 characters apart.
- A tab character causes the insertion of enough spaces to align the current position with the next tab stop.

{docstring Columnizer.LChar}

{docstring Columnizer.columnizer}

```lean
#eval Columnizer.columnizer ['a', 'b', 'c']
#eval Columnizer.columnizer ['a', 'b', '\n', 'c', 'd']
#eval Columnizer.columnizer ['a', 'b', '\r', 'c', 'd']
#eval Columnizer.columnizer ['a', 'b', '\u000C', 'c', 'd']
#eval Columnizer.columnizer ['a', 'b', '\r', '\n', 'c', 'd']
#eval Columnizer.columnizer ['a', '\t', 'b']
```

# Regular Expressions

{docstring RE}

{docstring Matching}

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

Haskell supports four kinds of literals: Integer literals, float literals, character literals and string literals.

## Integer Literals

Integer literals may be given in decimal (the default), octal (prefixed by 0o or 0O) or hexadecimal notation (prefixed by 0x or 0X).

{docstring Octit}

{docstring Octal}

```lean
#print Octit
#print Octal
```

{docstring Digit}

{docstring Decimal}

```lean
#print Digit
#print Decimal
```

{docstring Hexit}

{docstring Hexadecimal}

```lean
#print Hexit
#print Hexadecimal
```

Integer literals are parsed using the following regular expression:

{docstring Literals.Integer}

```lean
#eval lex_haskell "42"
#eval lex_haskell "0x42"
#eval lex_haskell "0X42"
#eval lex_haskell "0o42"
#eval lex_haskell "0O42"
```

## Float Literals

Float literals are parsed using the following regular expression:

{docstring Literals.Float}

```lean
#eval lex_haskell "42.0"
```

## Character Literals

Character literals are parsed using the following regular expression:

{docstring Literals.Char}

```lean
#eval lex_haskell "'a'"
```

## String Literals

String literals are parsed using the following regular expression:

{docstring Literals.String}

```lean
#print Literals.String
#eval lex_haskell "\"hello world\""
```

# Comments

Haskell supports two styles of comments, line comments and block comments.

## Line Comments

A line comment begins with a sequence of two or more consecutive dashes (e.g. "--") and extends to the following newline.
The sequence of dashes must not form part of a legal lexeme.
For example, "-->" or "|--" do not begin a comment, because both of these are legal lexemes; however "--foo" does start a comment.

```lean
#eval lex_haskell "-- some comment\n"
```

## Block Comments

```lean
#eval lex_haskell "{- some comment -}"
```
