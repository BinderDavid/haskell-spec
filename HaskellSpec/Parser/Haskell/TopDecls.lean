import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal

/--
```txt
⟨topdecl⟩ → type ⟨simpletype⟩ = ⟨type⟩
          | data [⟨context⟩ =>] ⟨simpletype⟩ [= ⟨constrs⟩] [⟨deriving⟩]
          | newtype [⟨context⟩ =>] ⟨simpletype⟩ = ⟨newconstr⟩ [⟨deriving⟩]
          | class [⟨scontext⟩ =>] ⟨tycls⟩ ⟨tyvar⟩ [where ⟨cdecls⟩]
          | instance [⟨scontext⟩ =>] ⟨qtycls⟩ ⟨inst⟩ [where ⟨idecls⟩]
          | default ( ⟨type₁⟩ , … , ⟨typeₙ⟩ )
          | foreign ⟨fdecl⟩
          | ⟨decl⟩
```
-/
def Topdecl : Rule :=
  { lhs := NT.TopDecl
    rhss := [ [T Token.TypeT, NT NT.SimpleType, T Token.Equal, NT NT.TypeT],
              [T Token.Data], -- TODO
              [T Token.Newtype], -- TODO
              [T Token.Class], -- TODO
              [T Token.Instance], -- TODO
              [T Token.Default], -- TODO
              [T Token.Foreign, NT NT.Fdecl],
              [NT NT.Decl]
            ]
  }

/--
```txt
⟨decls⟩ → { ⟨decl₁⟩ , … , ⟨declₙ⟩ }                (n ≥ 0)
```
-/
def Decls : Rule :=
  { lhs := NT.Decls
    rhss := [] -- TODO
  }

/--
```txt
⟨decl⟩ → ⟨gendecl⟩
       | ⟨funlhs⟩ ⟨rhs⟩
       | ⟨pat⟩ ⟨rhs⟩
```
-/
def Decl : Rule :=
  { lhs := NT.Decl
    rhss := [ [NT NT.GenDecl],
              [NT NT.Funlhs, NT NT.Rhs],
              [NT NT.Pat, NT NT.Rhs]
            ]
  }


/--
```txt
⟨cdecls⟩ → { ⟨cdecl₁⟩ , … , ⟨cdeclₙ⟩ }                (n ≥ 0)
```
-/
def Cdecls : Rule :=
  { lhs := NT.Cdecls
    rhss := [] -- TODO
  }

/--
```txt
⟨cdecl⟩ → ⟨gendecl⟩
        | ⟨funlhs⟩ ⟨rhs⟩
        | ⟨var⟩ ⟨rhs⟩
```
-/
def Cdecl : Rule :=
  { lhs := NT.Cdecl
    rhss := [ [NT NT.GenDecl],
              [NT NT.Funlhs, NT NT.Rhs],
              [NT NT.Var, NT NT.Rhs]
            ]
  }

/--
```txt
⟨idecls⟩ → { ⟨idecl₁⟩ , … , ⟨ideclₙ⟩ }                (n ≥ 0)
```
-/
def Idecls : Rule :=
  { lhs := NT.Idecls
    rhss := [] -- TODO
  }

/--
```txt
⟨idecl⟩ → ⟨funlhs⟩ ⟨rhs⟩
        | ⟨var⟩ ⟨rhs⟩
        |                            (empty)
```
-/
def Idecl : Rule :=
  { lhs := NT.Idecl
    rhss := [ [NT NT.Funlhs, NT NT.Rhs],
              [NT NT.Var, NT NT.Rhs],
              []
            ]
  }

/--
```txt
⟨gendecl⟩ → ⟨vars⟩ :: ⟨context⟩ => ⟨type⟩
          | ⟨vars⟩ :: ⟨type⟩
          | ⟨fixity⟩ ⟨integer⟩ ⟨ops⟩
          | ⟨fixity⟩ ⟨ops⟩
          |                                 (empty declaration)
```
-/
def GenDecl : Rule :=
  { lhs := NT.GenDecl
    rhss := [ [NT NT.Vars, T Token.DoubleColon, NT NT.Context, T Token.DoubleArrowRight, NT NT.TypeT],
              [NT NT.Vars, T Token.DoubleColon, NT NT.TypeT],
              [NT NT.Fixity, NT NT.Integer, NT NT.Ops],
              [NT NT.Fixity, NT NT.Ops],
              []
            ]
  }

/--
```txt
⟨ops⟩ → ⟨op⟩
      | ⟨op⟩ , ⟨ops⟩
```
-/
def Ops : Rule :=
  { lhs := NT.Ops
    rhss := [ [NT NT.Op],
              [NT NT.Op, T Token.Comma, NT NT.Ops]
            ]
  }

/--
```txt
⟨vars⟩ → ⟨var⟩
       | ⟨var⟩ , ⟨vars⟩
```
-/
def Vars : Rule :=
  { lhs := NT.Vars
    rhss := [ [NT NT.Var],
              [NT NT.Var, T Token.Comma, NT NT.Vars]
            ]
  }

/--
```txt
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
