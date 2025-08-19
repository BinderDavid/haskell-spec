import Veriflex.Located

open Veriflex

inductive Token : Type where
  | Whitespace : String → Token
  | QVarId : List String → String → Token
  | QConId : List String → String → Token
  | QVarSym : List String → String → Token
  | QVarCon : List String → String → Token
  | LitInteger : Nat → Token
    /-- The two numbers express the floating point number as a ratio -/
  | LitFloat : Nat → Nat → Token
  | LitChar : Char → Token
  | LitString : String → Token
    /-- `(` -/
  | OpenParen
    /-- `)` -/
  | CloseParen
    /-- `[` -/
  | OpenBracket
    /-- `]` -/
  | CloseBracket
    /-- `{` -/
  | OpenBrace
    /-- `}` -/
  | CloseBrace
    /-- `,` -/
  | Comma
    /-- `:` -/
  | Colon
    /-- `::` -/
  | DoubleColon
    /-- `;` -/
  | Semicolon
    /-- `\`` -/
  | Backtick
    /-- `\` -/
  | Backslash
    /-- `..` -/
  | DotDot
    /-- `=` -/
  | Equal
    /-- `<-` -/
  | ArrowLeft
    /-- `->` -/
  | ArrowRight
    /-- `=>` -/
  | DoubleArrowRight
    /-- `~` -/
  | Tilde
    /-- `@` -/
  | Ampersand
    /-- `|` -/
  | Solidus
    /-- `_` -/
  | Underscore
    /-- `as` -/
  | As
    /-- `case` -/
  | Case
    /-- `class` -/
  | Class
    /-- `data` -/
  | Data
    /-- `default` -/
  | Default
    /-- `deriving` -/
  | Deriving
    /-- `do` -/
  | Do
    /-- `else` -/
  | Else
    /-- `foreign` -/
  | Foreign
    /-- `if` -/
  | If
    /-- `import` -/
  | Import
    /-- `in` -/
  | In
    /-- `infix` -/
  | Infix
    /-- `infixl` -/
  | Infixl
    /-- `infixr` -/
  | Infixr
    /-- `instance` -/
  | Instance
    /-- `let` -/
  | Let
    /-- `module` -/
  | Module
    /-- `newtype` -/
  | Newtype
    /-- `of` -/
  | Of
    /-- `qualified` -/
  | Qualified
    /-- `then` -/
  | Then
    /-- `type` -/
  | TypeT
    /-- `where` -/
  | Where
  deriving Repr, BEq

/--
A located token with the start column
-/
abbrev LToken := Located Token
