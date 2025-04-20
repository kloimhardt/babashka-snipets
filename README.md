# Some code snippets for exploring sicmutils features

## state advancer

Start the cljs-repl:
```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile scmtest.advancerdemo --repl
```

and open `http://localhost:9000` in your favourite web browser.

In the termianl, call the following functions:

```
cljs.user=> (scmtest.advancerdemo/test-sa2)
```

It gives a `Execution error (DivisionByZero)` error

## sicmutils and sci
 [scitest.cljs](https://github.com/kloimhardt/babashka-snipets/blob/master/src/scmtest/scitest.cljs) contains a minimal example of how to run sicmutils within sci.

To run, clone this repo and start the cljs-repl:

```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile scmtest.scitest --repl
```

open `http://localhost:9000` in your favourite web browser.

## sicmutils and the compile option
Code for the following [sicmutils issue](https://github.com/sicmutils/sicmutils/issues/271)

Start the cljs-repl:
```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile scmtest.minimaldemo --repl
```

and open `http://localhost:9000` in your favourite web browser.

In the termianl, call the following functions:
```
cljs.user=> (scmtest.minimaldemo/la1)
435

cljs.user=> (scmtest.minimaldemo/la2)
```
with sicmutils version 0.21.0 the following error occured
```
Execution error (TypeError) at (<cljs repl>:1).
Cannot mix BigInt and other types, use explicit conversions
```

Here we need the `:compile` true option
```
cljs.user=> (scmtest.minimaldemo/la3)
210
```

with sicmutils 0.21.1, there is no error even without the `:compile` 
```
cljs.user=> (scmtest.minimaldemo/la2)
210
```

## The Photon project
Essays on the history of radiation form early Thermodynamics (Lambert) to Quantum Theory (deBroglie)

```
clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta22\"}}}"
```

Then, on the appearing `user=>` prompt:

```
(do
    (require '[scicloj.clay.v2.api :as clay])
    (clay/make! {:source-path "lambert.clj" :live-reload true})
    )
```

```
clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta22\"} org.mentat/emmy {:mvn/version \"0.32.0\"}}}" -M mariastefan.clj
```

## scheme-sicm-ch01.clj: a "define"-Macro for Scheme emulation
run the file containing scheme code:
```
java -jar noj-2-beta11.1-uber.jar scheme-sicm-ch01.clj
```
or its sibling (containing pure Clojure code)

```
java -jar noj-2-beta11.1-uber.jar emmy-sicm-ch01.clj
```

Start nRepl with bells and whistles for Emacs editing:
```
clj -Sdeps '{:deps {org.mentat/emmy {:mvn/version "0.32.0"} cider/cider-nrepl {:mvn/version "0.50.2"} }}' -M -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"
```
Or just a bare nRepl
```
clj -Sdeps '{:deps {org.mentat/emmy {:mvn/version "0.32.0"} nrepl/nrepl {:mvn/version "1.3.0"}}}' -m nrepl.cmdline
```
## Run Jupyter
```
source python_venv/bin/activate
jupyter lab
```
(works only Java@21 not 23)

## Run Blockly Rendering with Babashka (or Cljs)

```
babashka-snipets/clj-twotiles$ bb genblocks.clj code.clj
```

```
bb --nrepl-server
```
or

```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile tilestest.genblocks --repl
```

and open `http://localhost:9000` in your favourite web browser.

In the termianl, call the following functions:

```
(tilestest.genblocks/rpg [[0 0]] '(a [1]))
```
