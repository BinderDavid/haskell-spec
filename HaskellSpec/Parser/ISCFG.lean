/-
Formalization of indentation sensitive context-free grammars.

Following the paper:
Michael D. Adams, Principled Parsing for Indentation-Sensitive Languages: Revisiting Landin's Offside Rule
-/
import HaskellSpec.Lexer.Haskell.Tokens

namespace ISCFG

/--
We just use Strings for NonTerminals.
Once the Grammar is finished, it might be better to replace it with an enum.
-/
def NonTerminal : Type := String

/--
Terminals are Tokens produced by the lexer.
-/
def Terminal : Type := Token

inductive XTerminal : Type where
  | Terminal : Terminal → XTerminal
  | NonTerminal : NonTerminal → XTerminal

structure Rule where
  lhs : NonTerminal
  rhs : List TerminalOrNonTerminal

structure Grammar where
  rules : List Rule
  start : NonTerminal

inductive Step : Grammar → List XTerminal → List XTerminal → Prop where
  | STEP :
    g ∈ G.rules →
    Step G  (xs ++ [XTerminal.NonTerminal g.lhs] ++ zs) (xs ++ g.rhs ++ zs)

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
