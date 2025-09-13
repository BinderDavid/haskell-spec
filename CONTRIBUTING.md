# How to contribute

The formalization is done using the [Lean](https://lean-lang.org) proof assistant.
From the root directory you should be able to run the following command to see if the formalization typechecks:

```console
> lake build
Build completed successfully.
```

## Documentation

You can generate html documentation using the [doc-gen4](https://github.com/leanprover/doc-gen4) tool for Lean.
In order to build the documentation and serve it use the following target in the Makefile.

```console
> make serve-docs
```
and open the website `localhost:8000` in the browser.

## Coding Standards

### Naming

- Names of inference rules are in all-caps with underscores. E.g.: `T_APP`
- Names of judgement forms start with lowercase.
- Constructors of inductive data types should not have a prefix, since constructors are namespaced w.r.t. the type.

### Type Inference Rule

The following layouting style should be used to typeset and layout typing rules:

````lean
/--
Fig. 10 (Kind inference, type expressions)
```text
KE ⊢ t : κ
```
-/
inductive ktype : Env.KE
                → TypeExpression
                → Kind
                → Prop where
  | KIND_TVAR :
    (Env.KE_Name.u u, κ) ∈ ke →
    ----------------------------------------
    ktype ke (Source.TypeExpression.var u) κ

  | KIND_TCON :
    (Env.KE_Name.T T, κ) ∈ ke →
    ----------------------------------------------
    ktype ke (Source.TypeExpression.typename T) κ

  | KIND_APP :
    ktype ke t₁ (SemTy.Fun κ₁ κ₂) →
    ktype ke t₂ κ₁ →
    ---------------------------------------------
    ktype ke (Source.TypeExpression.app t₁ t₂) κ₂
````

To do this, observe the following points:

- The name of each inference rule is written in all-caps.
- All the premisses and the conclusion are aligned with the name of the inference rule.
- The implication arrow in inference rules comes at the end of the line.
- Different inference rules are separated by one empty line
- One line of dashes is used to separate the conclusion from the premisses of the rule.

