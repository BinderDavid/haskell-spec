namespace Source

/--
From Fig. 3:
```
literal ∈ Literal → char
                  | string
                  | integer
                  | float
```
-/
inductive Literal : Type where
    /--
    A character literal.
    Example: `'a'`
    Character literals are not overloaded and have type `Prelude.Char`.
    -/
  | char : Char → Literal
    /--
    A string literal.
    Example: `"hello"`
    String literals are not overloaded and have type `[Prelude.Char]`.
    -/
  | string : String → Literal
    /--
    An integer literal.
    Example: `5`
    Integer literals are overloaded and have type `Num a => a`.
    -/
  | integer : Int → Literal
    /--
    A floating literal.
    Example: `2.3`
    Floating literals are overloaded and have type `Fractional a => a`.
    -/
  | float : Int → Int → Literal

end Source
