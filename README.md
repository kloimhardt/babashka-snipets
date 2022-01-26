# Some code snippets

## Sicmutils browser repl
Code for the following [sicmutils issue](https://github.com/sicmutils/sicmutils/issues/271)

Start the cljs-repl:
```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile scmtest.minimaldemo --repl
```

Call the following functions:
```
cljs.user=> (scmtest.minimaldemo/la1)
435
cljs.user=> (scmtest.minimaldemo/la2)
Execution error (TypeError) at (<cljs repl>:1).
Cannot mix BigInt and other types, use explicit conversions
```
