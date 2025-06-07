import HaskellSpec.Source.SourceLang
import HaskellSpec.Target.TargetLang
import HaskellSpec.Environments


/-!
# Modules

The rules are defined in fig. 11 of the paper.
-/


inductive module : Environment.ME → Source.Module → Target.Module → Environment.ME → Prop where
