import VersoManual
import HaskellReport.Papers
import HaskellReport.LexicalSyntax
import HaskellReport.ContextFreeSyntax
import HaskellReport.Introduction

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

{include 1 HaskellReport.Introduction}

{include 1 HaskellReport.LexicalSyntax}

{include 1 HaskellReport.ContextFreeSyntax}
