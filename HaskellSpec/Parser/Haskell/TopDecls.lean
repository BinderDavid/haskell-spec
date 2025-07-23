import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal

/--
⟨topdecl⟩ → type ⟨simpletype⟩ = ⟨type⟩
          | data [⟨context⟩ =>] ⟨simpletype⟩ [= ⟨constrs⟩] [⟨deriving⟩]
          | newtype [⟨context⟩ =>] ⟨simpletype⟩ = ⟨newconstr⟩ [⟨deriving⟩]
          | class [⟨scontext⟩ =>] ⟨tycls⟩ ⟨tyvar⟩ [where ⟨cdecls⟩]
          | instance [⟨scontext⟩ =>] ⟨qtycls⟩ ⟨inst⟩ [where ⟨idecls⟩]
          | default ( ⟨type₁⟩ , … , ⟨typeₙ⟩ )
          | foreign ⟨fdecl⟩
          | ⟨decl⟩
-/
def Topdecl : Rule :=
  { lhs := NT.TopDecl
    rhss := [] -- TODO
  }

/--
```
⟨decls⟩ → { ⟨decl₁⟩ , … , ⟨declₙ⟩ }                (n ≥ 0)
```
-/
def Decls : Rule :=
  { lhs := NT.Decls
    rhss := [] -- TODO
  }

/--
```
⟨decl⟩ → ⟨gendecl⟩
       | ⟨funlhs⟩ ⟨rhs⟩
       | ⟨pat⟩ ⟨rhs⟩
```
-/
def Decl : Rule :=
  { lhs := NT.Decl
    rhss := [] -- TODO
  }


/--
```
⟨cdecls⟩ → { ⟨cdecl₁⟩ , … , ⟨cdeclₙ⟩ }                (n ≥ 0)
```
-/
def Cdecls : Rule :=
  { lhs := NT.Cdecls
    rhss := [] -- TODO
  }

/--
```
⟨cdecl⟩ → ⟨gendecl⟩
        | ⟨funlhs⟩ ⟨rhs⟩
        | ⟨var⟩ ⟨rhs⟩
```
-/
def Cdecl : Rule :=
  { lhs := NT.Cdecl
    rhss := [] -- TODO
  }

/--
```
⟨idecls⟩ → { ⟨idecl₁⟩ , … , ⟨ideclₙ⟩ }                (n ≥ 0)
```
-/
def Idecls : Rule :=
  { lhs := NT.Idecls
    rhss := [] -- TODO
  }

/--
```
⟨idecl⟩ → ⟨funlhs⟩ ⟨rhs⟩
        | ⟨var⟩ ⟨rhs⟩
        |                            (empty)
```
-/
def Idecl : Rule :=
  { lhs := NT.Idecl
    rhss := [] -- TODO
  }

/--
```
⟨gendecl⟩ → ⟨vars⟩ :: [⟨context⟩ =>] ⟨type⟩
          | ⟨fixity⟩ [⟨integer⟩] ⟨ops⟩
          |                                 (empty declaration)
```
-/
def GenDecl : Rule :=
  { lhs := NT.GenDecl
    rhss := [] -- TODO
  }

/--
```
⟨ops⟩ → ⟨op₁⟩ , … , ⟨opₙ⟩          (n ≥ 1)
```
-/
def Ops : Rule :=
  { lhs := NT.Ops
    rhss := [] -- TODO
  }

/--
```
⟨vars⟩ → ⟨var₁⟩ , … , ⟨varₙ⟩          (n ≥ 1)
```
-/
def Vars : Rule :=
  { lhs := NT.Vars
    rhss := [] -- TODO
  }

/--
```
⟨fixity⟩ → infixl
         | infixr
         | infix
```
-/
def Fixity : Rule :=
  { lhs := NT.Fixity
    rhss := [ [T Token.Infixl],
              [T Token.Infixr],
              [T Token.Infix]
            ]
  }
