import HaskellSpec.Names
import HaskellSpec.Source.Literals

namespace Source

/--
### Patterns
**Note:** `n+k`-patterns are part of Haskell 98 but have been removed from the formalization.
-/
inductive Pattern : Type where
    /--
    A variable pattern.
    -/
  | var : QVariable → Pattern
    /--
    A constructor pattern.
    Example: `Student name id`
    -/
  | constructor : QConstructor → List Pattern → Pattern
    /--
    A labelled constructor pattern.
    Example: `Student { name = name, id = id }`
    -/
  | constructor_labelled : QConstructor → List (Variable × Pattern) → Pattern
    /--
    An at-pattern.
    Example: `s @ Student name id`
    -/
  | at : Variable → Pattern → Pattern
    /--
    A lazy pattern.
    Example: `~p`
    -/
  | lazy : Pattern → Pattern
    /--
    A wildcard pattern.
    Example: `_`
    -/
  | wildcard : Pattern
    /--
    A literal pattern.
    Examples: `5`, `"hello world"`, `'c'`
    -/
  | literal : Literal → Pattern

end Source
