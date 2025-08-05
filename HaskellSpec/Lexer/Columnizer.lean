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


/--
A located character
-/
structure LChar : Type where
  char : Char
  column : Nat
  deriving Repr, BEq

def tab_insertion_nat (column : Nat) : Nat :=
  (TAB_WIDTH - Nat.mod column TAB_WIDTH) + 1

def tab_insertion_help (column countdown  : Nat) : List LChar :=
  match countdown with
  | 0 => []
  | Nat.succ x => { char := ' ', column := column } :: tab_insertion_help (column + 1) x

def tab_insertion (column : Nat) : List LChar :=
  tab_insertion_help column (tab_insertion_nat column)

def columnizer_rec (s : List Char) (column : Nat) : List LChar :=
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

def columnizer (s : List Char) : List LChar :=
  columnizer_rec s START_COLUMN

/-
Tests
-/

#guard columnizer ['a', 'b', 'c'] ==
       [LChar.mk 'a' 1, LChar.mk 'b' 2, LChar.mk 'c' 3]

#guard columnizer ['a', 'b', '\n', 'c', 'd'] ==
       [LChar.mk 'a' 1, LChar.mk 'b' 2, LChar.mk '\n' 3, LChar.mk 'c' 1, LChar.mk 'd' 2]

#guard columnizer ['a', 'b', '\r', 'c', 'd'] ==
       [LChar.mk 'a' 1, LChar.mk 'b' 2, LChar.mk '\r' 3, LChar.mk 'c' 1, LChar.mk 'd' 2]

#guard columnizer ['a', 'b', '\u000C', 'c', 'd'] ==
       [LChar.mk 'a' 1, LChar.mk 'b' 2, LChar.mk '\u000C' 3, LChar.mk 'c' 1, LChar.mk 'd' 2]

#guard columnizer ['a', 'b', '\r', '\n', 'c', 'd'] ==
       [LChar.mk 'a' 1, LChar.mk 'b' 2, LChar.mk '\r' 3, LChar.mk '\n' 4, LChar.mk 'c' 1, LChar.mk 'd' 2]

#guard columnizer ['a', '\t', 'b'] ==
       [LChar.mk 'a' 1, LChar.mk ' ' 2, LChar.mk ' ' 3, LChar.mk ' ' 4,
        LChar.mk ' ' 5, LChar.mk ' ' 6, LChar.mk ' ' 7, LChar.mk ' ' 8, LChar.mk 'b' 9]
