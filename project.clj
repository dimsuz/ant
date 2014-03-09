(defproject ant "1.2.3"
  :plugins [[lein-cljsbuild "1.0.2"]]
  :dependencies [[org.clojure/clojurescript "0.0-2173"]
                 [rm-hull/monet "0.1.10"]]
  :cljsbuild {
    :builds [{
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :output-to "target/main.js"  ; default: target/cljsbuild-main.js
          :optimizations :simple
          :pretty-print true}}]})
