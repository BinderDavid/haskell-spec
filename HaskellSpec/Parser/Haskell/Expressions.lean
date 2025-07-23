import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal

/--
```txt
⟨exp⟩ → ⟨infixexp⟩ :: ⟨context⟩ => ⟨type⟩
      | ⟨infixexp⟩ :: ⟨type⟩
      | ⟨infixexp⟩
```
-/
def Exp : Rule :=
  { lhs := NT.Exp
    rhss := [ [NT NT.Infixexp, T Token.DoubleColon, NT NT.Context, T Token.DoubleArrowRight, NT NT.TypeT],
              [NT NT.Infixexp, T Token.DoubleColon, NT NT.TypeT],
              [NT NT.Infixexp]
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
    rhss := [ [NT NT.Lexp, NT NT.Qop, NT NT.Infixexp],
              [], -- TODO: Add Minus to Tokens!
              [NT NT.Lexp]
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
    rhss := [ [NT NT.Fexp, NT NT.Aexp],
              [NT NT.Aexp]
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
