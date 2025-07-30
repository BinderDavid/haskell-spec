import VersoManual
import HaskellReport

open Verso Doc
open Verso.Genre Manual

open HaskellReport

def config : Config where
  emitTeX := false
  emitHtmlSingle := false
  emitHtmlMulti := true
  htmlDepth := 2

def main := manualMain (%doc HaskellReport) (extraSteps := []) (config := config)
