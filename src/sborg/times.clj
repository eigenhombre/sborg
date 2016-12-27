(ns sborg.times
  (:require [clj-time.core :as t]))


(defn replace-year-? [yyyy end?]
  (-> yyyy
      (clojure.string/replace #"\?" (if end? "9" "0"))
      Integer.))


(defn replace-month-? [mm end?]
  (-> mm
      (clojure.string/replace #"\?\?"
                              (if end? "12" "01"))
      (clojure.string/replace #"0\?"
                              (if end? "09" "01"))
      (clojure.string/replace #"\?"
                              (if end? "2" "0"))
      Integer.))


(defn replace-day-? [[d0 d1 :as dd] yr mo end?]
  (let [d0 (cond
             (not= d0 \?) d0   ;; We already know d0.
             (not end?) \0     ;; "start" case: just use 0
             (= mo 2) \2       ;; February end case: 28 days, use '2'
             :else \3)         ;; all other months have >= 30 days.
        d1 (cond
             (not= d1 \?) d1   ;; We already know d1
                               ;; Smallest we can go is 01:
             (not end?) (if (= d0 \0) \1 \0)
             (= d0 \0) 9       ;; Tens portion is zero, max is 9
             (= mo 2) \8       ;; Feb.  FIXME: leap years based on yr
                               ;; 30 days has September, April, June...
             (#{9 4 6 11} mo) \0
             :else \1)]        ;; Everything else has 31.
    (Integer. (str d0 d1))))


(defn parse-time-with-?s [s end?]
  (when (seq s)
    (let [yr (replace-year-? (subs s 0 4) end?)
          mo (replace-month-? (subs s 4 6) end?)
          da (replace-day-? (subs s 6 8) yr mo end?)]
      (t/date-time yr mo da))))


(defn t [d end? expect]
  (let [ans
        (-> d
            (parse-time-with-?s end?)
            str
            (subs 0 10))]
    (assert (= ans expect)
            (format "ans %s != expect %s" ans expect))))


;; Fast, cheap-o unit tests to catch edge cases:
(t "20100101" false "2010-01-01")
(t "2010010?" false "2010-01-01")
(t "201001??" false "2010-01-01")
(t "20100???" false "2010-01-01")
(t "20101???" false "2010-10-01")
(t "2010????" false "2010-01-01")
(t "2010????" false "2010-01-01")
(t "201002??" false "2010-02-01")
(t "2000????" false "2000-01-01")
(t "200?????" false "2000-01-01")
(t "20??????" false "2000-01-01")

(t "20100101" true "2010-01-01")
(t "2010010?" true "2010-01-09")
(t "20100???" true "2010-09-30")
(t "20101???" true "2010-12-31")
(t "201001??" true "2010-01-31")
(t "201002??" true "2010-02-28")
(t "201009??" true "2010-09-30")
(t "201010??" true "2010-10-31")
(t "2010????" true "2010-12-31")
(t "2000????" true "2000-12-31")
(t "2000????" true "2000-12-31")
(t "200?????" true "2009-12-31")
(t "20??????" true "2099-12-31")
