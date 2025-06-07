import HaskellSpec.Names
import HaskellSpec.Declarations
-- Figure 2

inductive Literal : Type where
  | lit_dummy : Int -> Literal

mutual
/--
```text
  p ∈ Pattern → x
               | K p₁ … pₙ      k ≥ 0
               | K {fp₁ … fpₙ}  k ≥ 0
               | v@p
               | ~p
               | _
               | literal
               | v+integer
```
--/
  inductive Pattern : Type where
    | pat_var : QVariable -> Pattern
    | pat_constr_pat : QConstructor -> List Pattern -> Pattern
    | pat_constr_fieldPat : QConstructor -> List FieldPattern -> Pattern
    | pat_at : Variable -> Pattern -> Pattern
    | pat_lazy : Pattern -> Pattern
    | pat_wildcard : Pattern
    | pat_lit : Literal -> Pattern
    | pat_plus : Variable -> Int -> Pattern

/--
```text
  fp ∈ FieldPattern → x = p
```
--/
  inductive FieldPattern : Type where
    | fp_pat: Variable -> Pattern -> FieldPattern
end

mutual
/--
```text
  binds ∈ Binds → [ sigs; bindG then binds]
```
--/
  inductive Binds : Type where
    | binds_binds : Signatures -> BindGroup -> Binds -> Binds

/--
```text
  bindG ∈ BindGroup → bind₁; …; bindₙ   n ≥ 1
```
--/
  inductive BindGroup : Type where
    | bind_group : Binding -> List Binding -> BindGroup

/--
```text
  bind ∈ Binding → x match₁ [] … [] matchₙ    n ≥ 1
                 | p gdes
```
--/
  inductive Binding : Type where
    | bind_match : QVariable -> Match -> List Match -> Binding
    | bind_pat : Pattern -> GuardedExprs -> Binding

/--
```text
  match ∈ Match → p₁ … pₖ gdes    k ≥ 1
```
--/
  inductive Match : Type where
    | match_match : Pattern -> List Pattern -> GuardedExprs -> Match

/--
```text
  gdes ∈ GuardedExprs → gde₁ … gdeₙ where binds   n ≥ 1
```
--/
  inductive GuardedExprs : Type where
    | gExp_where : GuardedExp -> List GuardedExp -> Binds -> GuardedExprs

/--
```text
  gde ∈ GuardedExpr → | e₁ = e₂
```
--/
  inductive GuardedExp : Type where
    | gExp_eq : Expression -> Expression -> GuardedExp

/--
```text
  e ∈ Expression → x
                  | literal
                  | K
                  | \p₁ … pₖ -> e                     k ≥ 1
                  | e₁ e₂
                  | let binds in e
                  | case e of match₁ [] … [] matchₙ   n ≥ 1
                  | do stmts
                  | [e | quals]
                  | [e₁ [,e₂] .. [e₃]]
                  | e ⇐ {fbind₁, …, fbindₖ}           k ≥ 0
                  | K {fbind₁, …, fbindₖ}             k ≥ 0
```
--/
  inductive Expression : Type where
    | expr_var : QVariable -> Expression
    | expr_lit : Literal -> Expression
    | expr_constr : QConstructor -> Expression
    | expr_abs : Pattern -> List Pattern -> Expression -> Expression
    | expr_app : Expression -> Expression -> Expression
    | expr_let : Binds -> Expression -> Expression
    | expr_case : Expression -> Match -> List Match -> Expression
    | expr_do : Statements -> Expression
    | expr_listComp : Expression -> Qualifiers -> Expression
    | expr_listRange : Expression -> Option Expression -> Option Expression -> Expression
    | expr_recUpd : Expression -> List FieldBinding -> Expression
    | expr_recConstr : QConstructor -> List FieldBinding -> Expression

  inductive Statement : Type where
    | stmt_arr : Pattern -> Expression -> Statement
    | stmt_let : Binds -> Statement
    | stmt_expr : Expression -> Statement

  inductive Statements : Type where
    | stmt_list : List Statement -> Expression -> Statements

  inductive Qualifiers : Type where
    | qal_list : List Statement -> Qualifiers

  inductive FieldBinding : Type where
    | fb_bind : QVariable -> Expression -> FieldBinding
end
