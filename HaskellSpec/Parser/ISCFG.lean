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
  | Qualified
  | ImpDecl
  | ImpSpec
  | ImpAs
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
Indentation Relations
-/
inductive IndentRelation : Type where
    /-- Equal -/
  | EQ : IndentRelation
    /-- Greater Than -/
  | GT : IndentRelation
    /-- Greater or Equal -/
  | GE : IndentRelation
    /-- Don't Care -/
  | DC : IndentRelation
  deriving Repr

inductive IndentR : IndentRelation → Int → Int → Prop where
  | EQ :
    n₁ = n₂ →
    IndentR IndentRelation.EQ n₁ n₂
  | GT :
    n₁ > n₂ →
    IndentR IndentRelation.GT n₁ n₂
  | GE :
    n₁ ≥ n₂ →
    IndentR IndentRelation.GE n₁ n₂
  | DC :
    IndentR IndentRelation.DC n₁ n₂

/--
Either a Terminal or NonTerminal
-/
inductive XTerminal : Type where
  | T : IndentRelation → Token → XTerminal
  | NT : IndentRelation → NT → XTerminal
  deriving Repr

structure Rule where
  lhs : NT
  rhss : List (List XTerminal)
  deriving Repr

structure Grammar where
  rules : List Rule
  start : NT
  start_indent : IndentRelation

inductive Step : Grammar → List XTerminal → List XTerminal → Prop where
  | STEP :
    g ∈ G.rules →
    rhs ∈ g.rhss →
    Step G  (xs ++ [XTerminal.NT _ g.lhs] ++ zs) (xs ++ rhs ++ zs)

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
  | Yes : IsTerminal (XTerminal.T _ t)

def AllTerminal (xs : List XTerminal) : Prop :=
  ∀ x ∈ xs, IsTerminal x

def Accepts (G : Grammar) (w : List XTerminal) : Prop :=
  Steps G [ XTerminal.NT G.start_indent G.start ] w ∧
  AllTerminal w

end ISCFG
