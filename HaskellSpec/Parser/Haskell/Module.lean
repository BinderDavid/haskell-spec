import HaskellSpec.Parser.ISCFG

open ISCFG
open XTerminal

/--
```
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
```
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
```
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
```
⟨topdecls⟩ → ⟨topdecl⟩
           | ⟨topdecl⟩ ; ⟨topdecls⟩
```
-/
def TopDecls : Rule :=
  { lhs := NT.TopDecls
    rhss := [[NT NT.TopDecl],
             [NT NT.TopDecl, T Token.Semicolon, NT NT.TopDecls]]
  }
