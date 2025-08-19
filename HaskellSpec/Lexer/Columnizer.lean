import Veriflex.Located
/-
This module implements the `columnizer` which assigns logical columns to characters in a string according
to the rules of section 10.3 of the Haskell2010 language report. That section specifies that:

  The “indentation” of a lexeme is the column number of the first character of that lexeme; the indentation of a line is the indentation of its leftmost lexeme. To determine the column number, assume a fixed-width font with the following conventions:

  - The characters newline, return, linefeed, and formfeed, all start a new line.
  - The first column is designated column 1, not 0.
  - Tab stops are 8 characters apart.
  - A tab character causes the insertion of enough spaces to align the current position with the next tab stop.
-/

namespace Columnizer

open Veriflex
/--
The Haskell language report specifies that columns start with 1
-/
def START_COLUMN : Nat := 1

/--
The Haskell language report specifies that horizontal tabs align at 8 column boundaries
-/
def TAB_WIDTH : Nat := 8

def tab_insertion_nat (column : Nat) : Nat :=
  (TAB_WIDTH - Nat.mod column TAB_WIDTH) + 1

def tab_insertion_help (column countdown  : Nat) : List LChar :=
  match countdown with
  | 0 => []
  | Nat.succ x => Located.mk column ' ' :: tab_insertion_help (column + 1) x

def tab_insertion (column : Nat) : List LChar :=
  tab_insertion_help column (tab_insertion_nat column)

def columnizer_rec (s : List Char) (column : Nat) : List LChar :=
  match s with
  | [] => []
    -- Newline = return linefeed | return | linefeed | formfeed
  | '\r' :: '\n' :: xs => Located.mk column '\r' :: Located.mk (column + 1) '\n' :: columnizer_rec xs START_COLUMN
  | '\n' :: xs => Located.mk column '\n' :: columnizer_rec xs START_COLUMN
  | '\r' :: xs => Located.mk column '\r' :: columnizer_rec xs START_COLUMN
  | '\u000C' :: xs => Located.mk column '\u000C' :: columnizer_rec xs START_COLUMN
    -- Horizontal Tabs are replaced by an appropriate number of '_' so that the position of the next
    -- character is at a TAB_WIDTH aligned boundary.
  | '\t' :: xs => tab_insertion column ++ columnizer_rec xs (column + tab_insertion_nat column)
    -- Every other character
  | x :: xs => Located.mk column x :: columnizer_rec xs (column + 1)

/--
Annotates a list of characters with their logical column according to the rules of the Haskell report.
-/
def columnizer (s : List Char) : List LChar :=
  columnizer_rec s START_COLUMN

/-
Tests
-/

#guard columnizer ['a', 'b', 'c'] ==
       [Located.mk 1 'a', Located.mk 2 'b', Located.mk 3 'c']

#guard columnizer ['a', 'b', '\n', 'c', 'd'] ==
       [Located.mk 1 'a', Located.mk 2 'b', Located.mk 3 '\n', Located.mk 1 'c', Located.mk 2 'd']

#guard columnizer ['a', 'b', '\r', 'c', 'd'] ==
       [Located.mk 1 'a', Located.mk 2 'b', Located.mk 3 '\r', Located.mk 1 'c', Located.mk 2 'd']

#guard columnizer ['a', 'b', '\u000C', 'c', 'd'] ==
       [Located.mk 1 'a', Located.mk 2 'b', Located.mk 3 '\u000C', Located.mk 1 'c', Located.mk 2 'd']

#guard columnizer ['a', 'b', '\r', '\n', 'c', 'd'] ==
       [Located.mk 1 'a', Located.mk 2 'b', Located.mk 3 '\r', Located.mk 4 '\n', Located.mk 1 'c', Located.mk 2 'd']

#guard columnizer ['a', '\t', 'b'] ==
       [Located.mk 1 'a', Located.mk 2 ' ', Located.mk 3 ' ', Located.mk 4 ' ',
        Located.mk 5 ' ', Located.mk 6 ' ', Located.mk 7 ' ', Located.mk 8 ' ', Located.mk 9 'b']

end Columnizer
