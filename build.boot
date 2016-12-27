(set-env! :dependencies '[[clj-time "0.12.2"]
                          [incanter "1.5.7"]
                          [org.clojure/data.csv "0.1.3"]])


(require '[clj-time.coerce :as c]
         '[clj-time.core :as t]
         '[clj-time.format :as fmt]
         '[clojure.data.csv :as csv]
         '[sborg.times :refer [parse-time-with-?s]]
         '[clojure.java.io :as io]
         '[incanter.charts :refer [time-series-plot]]
         '[incanter.core :refer [view]])


(defn convert-csv-to-maps
  "
  Use first row in CSV and rest of input to make maps whose keys are
  set by the first row.
  "
  [csv]
  (let [[header & rows] csv
        header (->> header
                    (map (comp keyword clojure.string/lower-case
                               #(clojure.string/replace % #" " "-")
                               #(clojure.string/replace % #"\#" "num"))))]
    (->> rows
         (interleave (repeat header))
         (partition 2)
         (map (partial apply interleave))
         (map (partial apply hash-map)))))




(defn parse-time-field [k k' end? m]
  (assoc m k' (parse-time-with-?s (get m k) end?)))


(defn parse-integer-field [k m]
  (let [raw (get m k)]
    (assoc m k
           (when (seq raw)
             (Integer. raw)))))


(defn add-duration [m]
  (let [t0 (:t0 m)
        t1 (:t1 m)]
    (assoc m :duration-days (inc (t/in-days (t/interval t0 t1))))))


(defn add-page-rate [m]
  (assoc m :sheets-per-day (/ (:sheets m) (:duration-days m))))


(defn parse-fields [m]
  (->> m
       (parse-time-field :start-date :t0 false)
       (parse-time-field :end-date :t1 true)
       (parse-integer-field :sheets)))


(defn min-max-date [sketchbooks]
  (let [dates (->> sketchbooks
                   (mapcat (juxt :t0 :t1))
                   (sort-by c/to-long))]
    [(first dates) (last dates)]))


(defn all-applicable-days [first-day last-day]
  (->> first-day
             (iterate (fn [t] (t/plus t (t/days 1))))
             (take-while (fn [t] (t/before? t (t/plus last-day (t/days 1)))))))


(defn make-sketchbook-plot [csv-file]
  (let [sketchbooks
        (->> csv-file
             slurp                   ;; raw data
             csv/read-csv            ;; into CSV format
             convert-csv-to-maps     ;; convert to maps
             (map parse-fields)      ;; parse date and number fields
                                     ;; remove "Total" row:
             (remove (comp (partial = "Total pages") :size))
                                     ;; require contents and dates
             (remove (comp empty? :contents))
             (remove (comp nil? :end-date))
             (remove (comp nil? :start-date))
             (map add-duration)      ;; calculate actual duration...
             (map add-page-rate))    ;; and page rate.
        [first-day last-day] (min-max-date sketchbooks)
        page-counts-by-month
        (->> (for [d (all-applicable-days first-day last-day)]
               [(.getYear d)
                (.getMonthOfYear d)
                ;; Sum sheets for sketchbooks for this day:
                (->> sketchbooks
                     (filter #(t/within? (t/interval (:t0 %) (:t1 %)) d))
                     (map :sheets-per-day)
                     (apply +)
                     double)])
             ;; Group by year/month...
             (group-by (partial take 2))
             ;; and sum the entries for that sketchbook + year/month combo:
             (map (juxt first (comp (partial apply +)
                                    (partial map last)
                                    second)))
             ;; Sort by [yyyy MM]:
             (sort-by (comp (juxt first second) first)))
        ;; X-axis: Convert yyyy-MM to dates in Epoch milliseconds:
        times-ms (map (comp c/to-long (fn [[y m]] (t/date-time y m 1)) first)
                      page-counts-by-month)
        ;; Y-axis: page-counts:
        page-counts (map second page-counts-by-month)]
    (view (time-series-plot times-ms page-counts
                            :x-label "Year"
                            :y-label "Sheets drawn per month"
                            :size [1200 1200]))))


(deftask run []
  (make-sketchbook-plot "/Users/jacobsen/Dropbox/org/sketchbooks.csv"))

