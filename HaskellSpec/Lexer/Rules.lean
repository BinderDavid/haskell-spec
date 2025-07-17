import HaskellSpec.Lexer.Tokens
import HaskellSpec.Lexer.RegExp

/-
Rules
-/

inductive Rule : Type where
  | mk : RE → (String → Token) → Rule
