import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal

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
    rhss := [[T Token.Module, NT NT.Modid, T Token.Where, NT NT.Body],
             [T Token.Module, NT NT.Modid, NT NT.Exports, T Token.Where, NT NT.Body],
             [NT NT.Body]
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
    rhss := [[T Token.OpenBrace, NT NT.ImpDecls, T Token.Semicolon, NT NT.TopDecls, T Token.CloseBrace],
             [T Token.OpenBrace, NT NT.ImpDecls, T Token.CloseBrace],
             [T Token.OpenBrace, NT NT.TopDecls, T Token.CloseBrace]]
  }

/--
```txt
⟨impdecls⟩ → ⟨impdecl⟩
           | ⟨impdecl⟩ ; ⟨impdecls⟩
```
-/
def ImpDecls : Rule :=
  { lhs := NT.ImpDecls
    rhss := [[NT NT.ImpDecl],
             [NT NT.ImpDecl, T Token.Semicolon, NT NT.ImpDecls]]
  }

/--
```txt
⟨impdecl⟩ → import ⟨qualified⟩ ⟨modid⟩ ⟨impas⟩ ⟨impspec⟩
          |
```
-/
def ImpDecl : Rule :=
  { lhs := NT.ImpDecl
    rhss := [ [T Token.Import, NT NT.Qualified, NT NT.Modid, NT NT.ImpAs, NT NT.ImpSpec ],
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
    rhss := [ [T Token.As, NT NT.Modid],
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
    rhss := [ [T Token.Qualified],
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
    rhss := [[NT NT.TopDecl],
             [NT NT.TopDecl, T Token.Semicolon, NT NT.TopDecls]]
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
