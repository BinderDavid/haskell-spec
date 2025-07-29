/-
Copyright (c) 2024-2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import VersoManual
import HaskellReport.Meta.Lean
import HaskellReport.Papers

-- This is a chapter that's included
import HaskellReport.Nat

-- This gets access to most of the manual genre (which is also useful for textbooks)
open Verso.Genre Manual

-- This gets access to Lean code that's in code blocks, elaborated in the same process and
-- environment as Verso
open Verso.Genre.Manual.InlineLean


open HaskellReport

set_option pp.rawOnError true


#doc (Manual) "Haskell Report" =>

%%%
authors := ["David Binder and contributors"]
%%%

This document presents the mechanization of the Haskell 2010 language report.

{include 1 HaskellReport.Nat}
