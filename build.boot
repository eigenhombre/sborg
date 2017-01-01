(set-env! :dependencies '[[clj-time "0.12.2"]
                          [incanter "1.5.7"]
                          [org.clojure/data.csv "0.1.3"]]
          :source-paths #{"src"})


(require '[sborg.core :refer [make-sketchbook-plot]])


(deftask run []
  (make-sketchbook-plot "/Users/jacobsen/Dropbox/org/sketchbooks.csv"))


(comment
  (run)

  )
