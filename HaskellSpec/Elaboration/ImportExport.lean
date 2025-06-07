import HaskellSpec.Environments
import HaskellSpec.Source.SourceLang
import HaskellSpec.Names


/-!
# Import + Export Declarations

The rules are defined in fig. 12, 13, 14 of the paper.
-/

inductive Export : Environment.FE → Environment.SE → Source.Entity → Environment.FE → Prop where

inductive Import : Environment.ME → Source.Import → Environment.FE → Environment.SE → Prop where

inductive Implist : Module_Name → Environment.EE → Source.ImportList → Environment.EE → Prop where

inductive Qualifier : Environment.EE → Source.Qualifier → Environment.EE → Prop where

inductive Entity : Environment.EE → Source.Entity → Environment.EE → Prop where
