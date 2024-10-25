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
$ clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta15\"} com.nextjournal/beholder {:mvn/version \"1.0.2\"}}}"

(do
    (require '[scicloj.clay.v2.api :as clay])
    (require '[nextjournal.beholder :as beholder])
    (defn make! [_] (clay/make! {:source-path "babashka-snipets/lambert.clj"}))
    (def watcher (beholder/watch make! "babashka-snipets"))
)

also look at photon/photon.org -> Resources -> clj Files -> Command lines

```
## sicm_ch1.clj: a "define"-Macro for Scheme emulation

```
$ clj -Sdeps '{:deps {org.mentat/emmy {:mvn/version "0.32.0"}   nrepl/nrepl {:mvn/version "1.3.0"}}}'  -m nrepl.cmdline
```
