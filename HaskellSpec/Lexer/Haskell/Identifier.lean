import HaskellSpec.Lexer.RegExp
import HaskellSpec.Lexer.Haskell.CharClasses
import HaskellSpec.Lexer.Haskell.Literals

/-
Identifiers
-/

def Tick : RE := RE.Symbol '\''

def ConId : RE := RE.App Large (RE.Star  (unions [Small, Large, Digit, Tick]))

def ModId : RE := ConId


def group_firsts { α : Type} (xs : List α) : Option (List α × α) :=
  match xs with
  | [] => none
  | x :: xs =>
    match group_firsts xs with
    | none => some ⟨[], x⟩
    | some ⟨mods, tl⟩ => some ⟨[x] ++ mods ,tl ⟩

def parse_mod_prefix (s : String) : List String × String :=
  let split := s.split (λ c => c == '.')
  let grouped := group_firsts split
  match grouped with
  | none => ⟨[], "IMPOSSIBLE"⟩
  | some x => x

#guard parse_mod_prefix "bar" == ⟨ [], "bar" ⟩
#guard parse_mod_prefix "Foo.bar" == ⟨ ["Foo"], "bar" ⟩
#guard parse_mod_prefix "Foo.Baz.bar" == ⟨ ["Foo", "Baz"], "bar" ⟩

def QConId : RE :=
  RE.App (RE.Star (RE.App ModId (RE.Symbol '.'))) ConId

def QConIdR : Rule :=
  Rule.mk QConId (λ s => let ⟨mods, con⟩ :=  parse_mod_prefix s; Token.QConId mods con)

def VarId : RE := RE.App Small (RE.Star  (unions [Small, Large, Digit, Tick]))

def QVarId : RE :=
  RE.App (RE.Star (RE.App ModId (RE.Symbol '.'))) VarId

def QVarIdR : Rule :=
  Rule.mk QVarId (λ s => let ⟨mods, var⟩ := parse_mod_prefix s; Token.QVarId mods var)

def all_identifiers : List Rule :=
  [QConIdR, QVarIdR]
