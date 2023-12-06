(ns advent-of-code.2023.day1 
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn csv-to-vec [filename]
  (->> filename
       slurp
       str/split-lines))

(defn first-digit [s]
  (re-find #"\d" s))

(defn last-digit [s]
  (->> s
       reverse
       str
       (re-find #"\d")))

(defn calibrate-row [s]
  (let [first-digit (first-digit s)
        last-digit (last-digit s)]
    (Integer/parseInt (str first-digit last-digit))))


(defn calibrate-file [filename]
  (let [parsed-rows (csv-to-vec filename)
        calibrated-numbers (map calibrate-row parsed-rows)
        final-calibrated-number (reduce + calibrated-numbers)]
    final-calibrated-number))

(calibrate-file "resources/2023/day_1_input.csv")

;;;; Part 2
(def digits-map {"1" 1
                 "2" 2
                 "3" 3
                 "4" 4
                 "5" 5
                 "6" 6
                 "7" 7
                 "8" 8
                 "9" 9
                 "one" 1
                 "two" 2
                 "three" 3
                 "four" 4
                 "five" 5
                 "six" 6
                 "seven" 7 
                 "eight" 8
                 "nine" 9})

(def digits (keys digits-map))

(defn first-digit-v2 [row]
  (let [sorted-digits-indicies (->> digits
                                    (map (fn [digit] [digit (str/index-of row digit)]))
                                    (remove (fn [[_ idx]] (nil? idx)))
                                    (into (priority-map)))
        first-digit (->> sorted-digits-indicies
                         first
                         key
                         digits-map)]
    first-digit))

(defn last-digit-v2 [row]
  (let [sorted-digits-indicies (->> digits
                                    (map (fn [digit] [digit (str/last-index-of row digit)]))
                                    (remove (fn [[_ idx]] (nil? idx)))
                                    (into (priority-map)))
        last-digit (->> sorted-digits-indicies
                         last
                         key
                         digits-map)]
    last-digit))

(defn calibrate-row-v2 [s]
  (let [first-digit (first-digit-v2 s)
        last-digit (last-digit-v2 s)]
    (Integer/parseInt (str first-digit last-digit))))


(defn calibrate-file-v2 [filename]
  (let [parsed-rows (csv-to-vec filename)
        calibrated-numbers (map calibrate-row-v2 parsed-rows)
        final-calibrated-number (reduce + calibrated-numbers)]
    final-calibrated-number))

(calibrate-file-v2 "resources/2023/day_1_input.csv")

