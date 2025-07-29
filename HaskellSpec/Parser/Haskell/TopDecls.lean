import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal
open IndentRelation

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
    rhss := [ [T DC Token.TypeT, NT DC NT.SimpleType, T DC Token.Equal, NT DC NT.TypeT],
              [T DC Token.Data], -- TODO
              [T DC Token.Newtype], -- TODO
              [T DC Token.Class], -- TODO
              [T DC Token.Instance], -- TODO
              [T DC Token.Default], -- TODO
              [T DC Token.Foreign, NT DC NT.Fdecl],
              [NT DC NT.Decl]
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
    rhss := [ [NT DC NT.GenDecl],
              [NT DC NT.Funlhs, NT DC NT.Rhs],
              [NT DC NT.Pat, NT DC NT.Rhs]
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
    rhss := [ [NT DC NT.GenDecl],
              [NT DC NT.Funlhs, NT DC NT.Rhs],
              [NT DC NT.Var, NT DC NT.Rhs]
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
    rhss := [ [NT DC NT.Funlhs, NT DC NT.Rhs],
              [NT DC NT.Var, NT DC NT.Rhs],
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
    rhss := [ [NT DC NT.Vars, T DC Token.DoubleColon, NT DC NT.Context, T DC Token.DoubleArrowRight, NT DC NT.TypeT],
              [NT DC NT.Vars, T DC Token.DoubleColon, NT DC NT.TypeT],
              [NT DC NT.Fixity, NT DC NT.Integer, NT DC NT.Ops],
              [NT DC NT.Fixity, NT DC NT.Ops],
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
    rhss := [ [NT DC NT.Op],
              [NT DC NT.Op, T DC Token.Comma, NT DC NT.Ops]
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
    rhss := [ [NT DC NT.Var],
              [NT DC NT.Var, T DC Token.Comma, NT DC NT.Vars]
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
    rhss := [ [T DC Token.Infixl],
              [T DC Token.Infixr],
              [T DC Token.Infix]
            ]
  }
