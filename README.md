# Some code snippets

## Sicmutils browser repl
Code for the following [sicmutils issue](https://github.com/sicmutils/sicmutils/issues/271)

Start the cljs-repl:
```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile scmtest.core --repl
```

Call the following functions:
```
cljs.user=> (scmtest.core/la1)
435
cljs.user=> (scmtest.core/la2)
```
