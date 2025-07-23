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
  { lhs := NonTerminal.Module
    rhss := [[Terminal Token.Module, NonTerminal NonTerminal.Modid, Terminal Token.Where, NonTerminal NonTerminal.Body],
             [Terminal Token.Module, NonTerminal NonTerminal.Modid, NonTerminal NonTerminal.Exports, Terminal Token.Where, NonTerminal NonTerminal.Body],
             [NonTerminal NonTerminal.Body]
            ]
  }
