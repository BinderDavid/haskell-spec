/-
This module implements the `columnizer` which assigns logical columns to characters in a string according
to the rules of section 10.3 of the Haskell2010 language report. That section specifies that:

  The “indentation” of a lexeme is the column number of the first character of that lexeme; the indentation of a line is the indentation of its leftmost lexeme. To determine the column number, assume a fixed-width font with the following conventions:

  - The characters newline, return, linefeed, and formfeed, all start a new line.
  - The first column is designated column 1, not 0.
  - Tab stops are 8 characters apart.
  - A tab character causes the insertion of enough spaces to align the current position with the next tab stop.
-/

/--
The Haskell language report specifies that columns start with 1
-/
def START_COLUMN : Nat := 1

/--
The Haskell language report specifies that horizontal tabs align at 8 column boundaries
-/
def TAB_WIDTH : Nat := 8


structure LocatedChar : Type where
  char : Char
  column : Nat
  deriving Repr, BEq

def tab_insertion_nat (column : Nat) : Nat :=
  (TAB_WIDTH - Nat.mod column TAB_WIDTH) + 1

def tab_insertion_help (column countdown  : Nat) : List LocatedChar :=
  match countdown with
  | 0 => []
  | Nat.succ x => { char := ' ', column := column } :: tab_insertion_help (column + 1) x

def tab_insertion (column : Nat) : List LocatedChar :=
  tab_insertion_help column (tab_insertion_nat column)

def columnizer_rec (s : List Char) (column : Nat) : List LocatedChar :=
  match s with
  | [] => []
    -- Newline = return linefeed | return | linefeed | formfeed
  | '\r' :: '\n' :: xs => { char := '\r', column := column } :: { char := '\n', column := column + 1 } :: columnizer_rec xs START_COLUMN
  | '\n' :: xs => { char := '\n', column := column } :: columnizer_rec xs START_COLUMN
  | '\r' :: xs => { char := '\r', column := column } :: columnizer_rec xs START_COLUMN
  | '\u000C' :: xs => { char := '\u000C', column := column } :: columnizer_rec xs START_COLUMN
    -- Horizontal Tabs are replaced by an appropriate number of '_' so that the position of the next
    -- character is at a TAB_WIDTH aligned boundary.
  | '\t' :: xs => tab_insertion column ++ columnizer_rec xs (column + tab_insertion_nat column)
    -- Every other character
  | x :: xs => { char := x, column := column } :: columnizer_rec xs (column + 1)

def columnizer (s : List Char) : List LocatedChar :=
  columnizer_rec s START_COLUMN

/-
Tests
-/

#guard columnizer ['a', 'b', 'c'] ==
       [LocatedChar.mk 'a' 1, LocatedChar.mk 'b' 2, LocatedChar.mk 'c' 3]

#guard columnizer ['a', 'b', '\n', 'c', 'd'] ==
       [LocatedChar.mk 'a' 1, LocatedChar.mk 'b' 2, LocatedChar.mk '\n' 3, LocatedChar.mk 'c' 1, LocatedChar.mk 'd' 2]

#guard columnizer ['a', 'b', '\r', 'c', 'd'] ==
       [LocatedChar.mk 'a' 1, LocatedChar.mk 'b' 2, LocatedChar.mk '\r' 3, LocatedChar.mk 'c' 1, LocatedChar.mk 'd' 2]

#guard columnizer ['a', 'b', '\u000C', 'c', 'd'] ==
       [LocatedChar.mk 'a' 1, LocatedChar.mk 'b' 2, LocatedChar.mk '\u000C' 3, LocatedChar.mk 'c' 1, LocatedChar.mk 'd' 2]

#guard columnizer ['a', 'b', '\r', '\n', 'c', 'd'] ==
       [LocatedChar.mk 'a' 1, LocatedChar.mk 'b' 2, LocatedChar.mk '\r' 3, LocatedChar.mk '\n' 4, LocatedChar.mk 'c' 1, LocatedChar.mk 'd' 2]

#guard columnizer ['a', '\t', 'b'] ==
       [LocatedChar.mk 'a' 1, LocatedChar.mk ' ' 2, LocatedChar.mk ' ' 3, LocatedChar.mk ' ' 4,
        LocatedChar.mk ' ' 5, LocatedChar.mk ' ' 6, LocatedChar.mk ' ' 7, LocatedChar.mk ' ' 8, LocatedChar.mk 'b' 9]
