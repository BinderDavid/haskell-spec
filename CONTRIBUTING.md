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
At some point we might want to switch to using [verso](https://github.com/leanprover/verso), though.

## Coding Standards

### Naming

- Names of inference are in all-caps with underscores `T_APP`
- Names of judgement forms start with lowercase
- Constructors of inductive data types should not have a prefix, since constructors are namespaced w.r.t. the type.

### Import/Export Discipline

TODO