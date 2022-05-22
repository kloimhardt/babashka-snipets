# Some code snippets for exploring sicmutils features

## sicmutils and sci
When runnig the following code in sci, the error 'Could not resolve symbol: d:dt' occurs:

```
(def gamma (literal-manifold-map 'q R1-rect R2-rect))

(def the-metric (literal-metric 'g R2-rect))

(def Cartan
  (Christoffel->Cartan
   (metric->Christoffel-2
    the-metric
    (coordinate-system->basis R2-rect))))

(def geodesic-equation-residuals
  (((((covariant-derivative Cartan gamma) d:dt)
     ((differential gamma) d:dt))
    (chart R2-rect))
   ((point R1-rect) 't)))

(def simple-expression (* 3 'x))

[\"from within sci\"
 [Cartan (->TeX simple-expression)]]
```

To reproduce, clone this repo and start the cljs-repl:

```
clj -M --main cljs.main --repl-opts "{:launch-browser false}" --compile scmtest.scitest --repl
```

open `http://localhost:9000` in your favourite web browser.

the error mentioned above is printed in both terminal and browser console.

## obsolete: sicmutils and the compile option
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
