import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal
open IndentRelation

namespace Module

/--
```txt
⟨module⟩ → module ⟨modid⟩ where ⟨body⟩
         | module ⟨modid⟩ ⟨exports⟩ where ⟨body⟩
         | ⟨body⟩
```
-/
def Module : Rule :=
  { lhs := NT.Module
    rhss := [[T DC Token.Module, NT DC NT.Modid, T DC Token.Where, NT DC NT.Body],
             [T DC Token.Module, NT DC NT.Modid, NT DC NT.Exports, T DC Token.Where, NT DC NT.Body],
             [NT DC NT.Body]
            ]
  }

/--
```txt
⟨body⟩ → { ⟨impdecls⟩ ; ⟨topdecls⟩ }
       | { ⟨impdecls⟩ }
       | { ⟨topdecls⟩ }
```
-/
def Body : Rule :=
  { lhs := NT.Body
    rhss := [[T DC Token.OpenBrace, NT DC NT.ImpDecls, T DC Token.Semicolon, NT DC NT.TopDecls, T DC Token.CloseBrace],
             [T DC Token.OpenBrace, NT DC NT.ImpDecls, T DC Token.CloseBrace],
             [T DC Token.OpenBrace, NT DC NT.TopDecls, T DC Token.CloseBrace]]
  }

/--
```txt
⟨impdecls⟩ → ⟨impdecl⟩
           | ⟨impdecl⟩ ; ⟨impdecls⟩
```
-/
def ImpDecls : Rule :=
  { lhs := NT.ImpDecls
    rhss := [[NT DC NT.ImpDecl],
             [NT DC NT.ImpDecl, T DC Token.Semicolon, NT DC NT.ImpDecls]]
  }

/--
```txt
⟨impdecl⟩ → import ⟨qualified⟩ ⟨modid⟩ ⟨impas⟩ ⟨impspec⟩
          |
```
-/
def ImpDecl : Rule :=
  { lhs := NT.ImpDecl
    rhss := [ [T DC Token.Import, NT DC NT.Qualified, NT DC NT.Modid, NT DC NT.ImpAs, NT DC NT.ImpSpec ],
              []
            ]
  }

/--
```txt
⟨impas⟩ → as ⟨modid⟩
        |
```
-/
def ImpAs : Rule :=
  { lhs := NT.ImpAs
    rhss := [ [T DC Token.As, NT DC NT.Modid],
              []
            ]
  }

/--
```txt
⟨qualified⟩ → qualified
            |
```
-/
def Qualified : Rule :=
  { lhs := NT.Qualified,
    rhss := [ [T DC Token.Qualified],
              []]
  }
/--
```txt
⟨impspec⟩ → ( ⟨import₁⟩, ..., ⟨importₙ⟩ [,])          (n ≥ 0)
          | hiding ( ⟨import₁⟩, ..., ⟨importₙ⟩ [,])   (n ≥ 0)
          |
```
-/
def ImpSpec : Rule :=
  { lhs := NT.ImpSpec
    rhss := [] -- TODO
  }

/--
```txt
⟨import⟩ → ⟨var⟩
         | ⟨tycon⟩ (..)
         | ⟨tycon⟩ ( ⟨cname₁⟩, … , ⟨cnameₙ⟩ )       (n ≥ 0)
         | ⟨tycls⟩ (..)
         | ⟨tycls⟩ ( ⟨var₁⟩, … , ⟨varₙ⟩ )           (n ≥ 0)
```
-/
def Import : Rule :=
  { lhs := NT.Import
    rhss := [] -- TODO
  }

/--
```txt
⟨exports⟩ → ( ⟨export₁⟩ , … , ⟨exportₙ⟩ )                   (n ≥ 0)
```
-/
def Exports : Rule :=
  { lhs := NT.Exports
    rhss := [] -- TODO
  }

/--
```txt
⟨export⟩ → ⟨qvar⟩
         | ⟨qtycon⟩ (..)
         | ⟨qtycon⟩ ( ⟨cname₁⟩ , … , ⟨cnameₙ⟩ )      (n ≥ 0)
         | ⟨qtycls⟩ (..)
         | ⟨qtycls⟩ ( ⟨var₁⟩ , … , ⟨varₙ⟩ )          (n ≥ 0)
         | module ⟨modid⟩
```
-/
def Export : Rule :=
  { lhs := NT.Export
    rhss := [] -- TODO
  }

/--
```txt
⟨topdecls⟩ → ⟨topdecl⟩
           | ⟨topdecl⟩ ; ⟨topdecls⟩
```
-/
def TopDecls : Rule :=
  { lhs := NT.TopDecls
    rhss := [[NT DC NT.TopDecl],
             [NT DC NT.TopDecl, T DC Token.Semicolon, NT DC NT.TopDecls]]
  }

/--
```txt
⟨cname⟩ → ⟨var⟩
        | ⟨con⟩
```
-/
def Cname : Rule :=
  { lhs := NT.Cname
    rhss := [] -- TODO
  }

end Module
