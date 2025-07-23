/-
Formalization of indentation sensitive context-free grammars.

Following the paper:
Michael D. Adams, Principled Parsing for Indentation-Sensitive Languages: Revisiting Landin's Offside Rule
-/
import HaskellSpec.Lexer.Haskell.Tokens

namespace ISCFG


inductive NonTerminal : Type where
  -- Modules
  | Module
  | Body
  -- Imports
  | ImpDecls
  | ImpDecl
  | ImpSpec
  | Import
  -- Exports
  | Exports
  | Export
  -- Toplevel Declarations
  | TopDecls
  | Modid
  deriving Repr

inductive XTerminal : Type where
  | Terminal : Token → XTerminal
  | NonTerminal : NonTerminal → XTerminal
  deriving Repr

structure Rule where
  lhs : NonTerminal
  rhss : List (List XTerminal)
  deriving Repr

structure Grammar where
  rules : List Rule
  start : NonTerminal

inductive Step : Grammar → List XTerminal → List XTerminal → Prop where
  | STEP :
    g ∈ G.rules →
    rhs ∈ g.rhss →
    Step G  (xs ++ [XTerminal.NonTerminal g.lhs] ++ zs) (xs ++ rhs ++ zs)

/--
Reflexive transitive closure of Step
-/
inductive Steps : Grammar → List XTerminal → List XTerminal → Prop where
  | REFL :
    Steps G xs xs
  | TRANS :
    Steps G xs ys →
    Step G ys zs →
    Steps G xs zs

inductive IsTerminal : XTerminal → Prop where
  | Yes : IsTerminal (XTerminal.Terminal t)

def AllTerminal (xs : List XTerminal) : Prop :=
  ∀ x ∈ xs, IsTerminal x

def Accepts (G : Grammar) (w : List XTerminal) : Prop :=
  Steps G [ XTerminal.NonTerminal G.start ] w ∧
  AllTerminal w

end ISCFG
