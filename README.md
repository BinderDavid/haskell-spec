# Mechanization of the Haskell Language Report

An experimental mechanization of the Haskell 98 and Haskell 2010 language reports.
The goal of this project is to obtain a formal and mechanized *specification* of the language described in the language report.
The main objective is to remove any ambiguity from the wording of the report rather than to prove any metatheoretic properties about the language.

For information about contributing to the project see the [Contributing.md](./CONTRIBUTING.md).

## FAQ

### Why start with Haskell 98 instead of Haskell 2010

Our starting point is the paper "A static semantics for Haskell" by Karl-Filip Faxen, since that paper is the most complete description of the static semantics of the complete Haskell language report.
That paper was published in 2002 and therefore only covers the Haskell 98 report.
No other paper which is similarly comprehensive has been written for Haskell 2010.
It should be relatively easy to adapt the formalization to the Haskell 2010 report as a second step.

### What about the dynamic semantics of Haskell

The paper by Faxen does not specify an operational or denotational semantics.
It partially specifies the semantics of type classes by elaborating them to dictionary-passing style.

### What are the limitations of the formalization

The biggest omission, already mentioned in the paper, is that ambiguous functions such as `show . read :: String -> String` are not properly handled.

### Why don't we use a type inference algorithm for the specification

The problem with specifying the static semantics of a language using a deterministic algorithm is that we cannot talk about coherence in this way:
Talking formally about coherence requires that we can talk about multiple different typing derivations for the same typing judgement, and this is not possible with an algorithmic specification.
For a discussion of this issue, see for example [Cochis: Stable and coherent implicits](https://homepages.inf.ed.ac.uk/wadler/papers/cochis/cochis.pdf)

## Resources

- [Haskell 98 Language Report](https://www.haskell.org/onlinereport/)
- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
- [Faxen, A Static Semantics for Haskell](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/9D90E0C7DE8DA7D6BAEAC5143E658E1D/S0956796802004380a.pdf/a-static-semantics-for-haskell.pdf)