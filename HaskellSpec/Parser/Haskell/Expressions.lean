import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal
open IndentRelation

/--
```txt
⟨exp⟩ → ⟨infixexp⟩ :: ⟨context⟩ => ⟨type⟩
      | ⟨infixexp⟩ :: ⟨type⟩
      | ⟨infixexp⟩
```
-/
def Exp : Rule :=
  { lhs := NT.Exp
    rhss := [ [NT DC NT.Infixexp, T DC Token.DoubleColon, NT DC NT.Context, T DC Token.DoubleArrowRight, NT DC NT.TypeT],
              [NT DC NT.Infixexp, T DC Token.DoubleColon, NT DC NT.TypeT],
              [NT DC NT.Infixexp]
            ]
  }

/--
```txt
⟨infixexp⟩ → ⟨lexp⟩ ⟨qop⟩ ⟨infixexp⟩
           | - ⟨infixexp⟩
           | ⟨lexp⟩
```
-/
def Infixexp : Rule :=
  { lhs := NT.Infixexp
    rhss := [ [NT DC NT.Lexp, NT DC NT.Qop, NT DC NT.Infixexp],
              [], -- TODO: Add Minus to Tokens!
              [NT DC NT.Lexp]
            ]
  }

/--
```txt
⟨lexp⟩ → \ ⟨apat₁⟩ … ⟨apatₙ⟩ -> ⟨exp⟩         (n ≥ 1)
       | let ⟨decls⟩ in ⟨exp⟩
       | if ⟨exp⟩ [;] then ⟨exp⟩ [;] else ⟨exp⟩
       | case ⟨exp⟩ of { ⟨alts⟩ }
       | do { ⟨stmts⟩ }
       | ⟨fexp⟩
```
-/
def Lexp : Rule :=
  { lhs := NT.Lexp
    rhss := [] -- TODO
  }

/--
```txt
⟨fexp⟩ → ⟨fexp⟩ ⟨aexp⟩
       | ⟨aexp⟩
```
-/
def Fexp : Rule :=
  { lhs := NT.Fexp
    rhss := [ [NT DC NT.Fexp, NT DC NT.Aexp],
              [NT DC NT.Aexp]
            ]
  }

/--
```txt
⟨aexp⟩ → ⟨qvar⟩
       | ⟨gcon⟩
       | ⟨literal⟩
       | ( ⟨exp⟩ )
       | ( ⟨exp₁⟩ , … , ⟨expₖ⟩ )                   (k ≥ 2)
       | [ ⟨exp₁⟩ , … , ⟨expₖ⟩ ]                   (k ≥ 1)
       | [ ⟨exp₁⟩ [, ⟨exp₂⟩] .. [⟨exp₃⟩]]
       | [ ⟨exp⟩ | ⟨qual₁⟩ , … ,  ⟨qualₙ⟩ ]        (n ≥ 1)
       | ( ⟨infixexp⟩ ⟨qop⟩ )
       | ( ⟨qop⟩_⟨-⟩ ⟨infixexp⟩ )
       | ⟨qcon⟩ { ⟨fbind₁⟩ , … , ⟨fbindₙ⟩ }        (n ≥ 0)
       | ⟨aexp⟩_⟨qcon⟩ { ⟨fbind₁⟩ , … , ⟨fbindₙ⟩ } (n ≥ 1)
```
-/
def Aexp : Rule :=
  { lhs := NT.Aexp
    rhss := [] -- TODO
  }
