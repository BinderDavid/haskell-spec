import HaskellSpec.SemanticTypes
import HaskellSpec.Source.SourceLang
import HaskellSpec.Environments

/-!
# Kind System

The rules are defined in fig. 8, 9, 10 of the paper.
-/

namespace Kinding

inductive KindOrdering : SemanticTypes.Kind → SemanticTypes.Kind → Prop where

inductive kctDecls : Environment.KE → Source.ClassesAndTypes → Environment.KE → Prop where

inductive kgroup : Environment.KE → List Source.ClassOrType → Environment.KE → Prop where

inductive kctDecl : Environment.KE → Source.ClassOrType → Environment.KE → Prop where

inductive kconDecl : Environment.KE → Source.ConstructorDecl → Prop where

inductive ksigs : Environment.KE → Source.Signatures → Prop where

inductive ksig : Environment.KE → Source.Signature → Prop where

inductive kctx : Environment.KE → Source.Context → Prop where

inductive ktype : Environment.KE → Source.TypeExpression → SemanticTypes.Kind → Prop where

end Kinding
