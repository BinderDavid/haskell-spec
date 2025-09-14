# Differences to the Faxen Paper

This document describes the differences between the Lean formalization and the paper by Faxen.

## Splitting of TE

The type environment `TE` defined in section 2.7.2 of the paper allows to look up both type names (of algebraic data types and synonyms)
and type variables. The environment `TE` has therefore been split into `TE1` and `TE2`.

## Removal of n+k Patterns

`n+k` patterns are no longer a part of Haskell 2010, and have therefore been removed from the formalization.
