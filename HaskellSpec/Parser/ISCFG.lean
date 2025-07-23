/-
Formalization of indentation sensitive context-free grammars.

Following the paper:
Michael D. Adams, Principled Parsing for Indentation-Sensitive Languages: Revisiting Landin's Offside Rule
-/
import HaskellSpec.Lexer.Haskell.Tokens

namespace ISCFG


/--
Non Terminals
-/
inductive NT : Type where
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
  | TopDecl
  | Decls
  | Decl
  | Cdecls
  | Cdecl
  | Idecls
  | Idecl
  | GenDecl
  | Ops
  | Op
  | Qop
  | Vars
  | Fixity
  -- Identifiers
  | Modid
  | Cname
  | Var
  -- Types
  | SimpleType
  | TypeT
  -- Expressions
  | Exp
  | Infixexp
  | Lexp
  | Fexp
  | Aexp
  -- Other
  | Fdecl
  | Pat
  | Funlhs
  | Rhs
  | Context
  | Integer
  deriving Repr

/--
Either a Terminal or NonTerminal
-/
inductive XTerminal : Type where
  | T : Token → XTerminal
  | NT : NT → XTerminal
  deriving Repr

structure Rule where
  lhs : NT
  rhss : List (List XTerminal)
  deriving Repr

structure Grammar where
  rules : List Rule
  start : NT

inductive Step : Grammar → List XTerminal → List XTerminal → Prop where
  | STEP :
    g ∈ G.rules →
    rhs ∈ g.rhss →
    Step G  (xs ++ [XTerminal.NT g.lhs] ++ zs) (xs ++ rhs ++ zs)

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
  | Yes : IsTerminal (XTerminal.T t)

def AllTerminal (xs : List XTerminal) : Prop :=
  ∀ x ∈ xs, IsTerminal x

def Accepts (G : Grammar) (w : List XTerminal) : Prop :=
  Steps G [ XTerminal.NT G.start ] w ∧
  AllTerminal w

end ISCFG
