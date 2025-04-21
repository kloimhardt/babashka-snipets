(ns cljthreetiles
  (:require [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

(kind/html "<h1>8</h1>")

(kind/html "
<script src=\"https://unpkg.com/blockly/blockly_compressed.js\"></script>
")

(kind/html "
<script>
var toolbox = {\"kind\":\"categoryToolbox\",\"contents\":[{\"kind\":\"category\",\"name\":\">\",\"contents\":[]}]};
</script>
")

(kind/html "
<div id=\"blocklyDiv17\" style=\"width: 100%; height: 400px;\"></div>
")

(kind/html "
<script>
var workspace17 = Blockly.inject(document.getElementById('blocklyDiv17'), {'toolbox': toolbox, 'sounds': false});
</script>
")
