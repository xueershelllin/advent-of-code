(ns advent-of-code.2023.day2
  (:require [clojure.string :as str]))

"" "
   https://adventofcode.com/2023/day/2

   Determine which games would have been possible if the bag had been loaded with only 
   12 red cubes, 13 green cubes, and 14 blue cubes. 
   What is the sum of the IDs of those games?
" ""

(def max-red 12)
(def max-green 13)
(def max-blue 14)

(defn single-colour-count-to-map
  "e.g. '4 green' => {'green' 4} "
  [s]
  (let [[count color] (str/split s #" \s*")]
    {color (Integer/parseInt count)}))

(defn multiple-colour-count-to-map
  "e.g. '4 green, 2 blue' => {'green' 4, 'blue' 2}"
  [s]
  (->> (str/split s #",\s*")
       (map single-colour-count-to-map)
       (into (sorted-map))))

(defn draw-results-to-color-maps
  "e.g. 
    '4 green, 2 blue; 
     1 red, 1 blue, 4 green; 
     3 green, 4 blue, 1 red'
    =>
    {'green' 4, 'blue' 2}
    {'red' 1, 'blue' 1, 'green' 4}
    {'green' 3, 'blue' 4, 'red' 1}"
  [s]
  (->> (str/split s #";\s*")
       (map multiple-colour-count-to-map)
       (set)))

(defn parse-row-into-map [row]
  (let [game-number (Integer/parseInt (re-find #"\d+(?=:)" row)) ;; this can be the first number before ":"
        draw-data-str (second (re-find #"Game \d+: (.*)" row)) ;; this can be the string after ": "
        draw-data-map (draw-results-to-color-maps draw-data-str)] ;; turn the string of draw results into a map
    {game-number draw-data-map}))

(defn parse-csv [filename]
  (->> filename
       slurp
       str/split-lines
       (map parse-row-into-map)
       (into (sorted-map))))

(defn red-over-max? [all-draws]
  (->> all-draws
       (map #(get % "red"))
       (remove nil?)
       (apply max)
       (< max-red)))
(defn blue-over-max? [all-draws]
  (->> all-draws
       (map #(get % "blue"))
       (remove nil?)
       (apply max)
       (< max-blue)))
(defn green-over-max? [all-draws]
  (->> all-draws
       (map #(get % "green"))
       (remove nil?)
       (apply max)
       (< max-green)))

(defn is-this-game-possible? [game]
  (let [game-number (first game)
        all-draws (second game)]
    (when-not (or (red-over-max? all-draws)
            (blue-over-max? all-draws)
            (green-over-max? all-draws))
      game-number)))

(defn get-final-result [filename]
  (->> filename
       parse-csv
       (map is-this-game-possible?)
       (remove nil?)
       (reduce +)))

(get-final-result "resources/2023/day_2_input.csv")

;;;;; Part 2
(def example-games (take 10 (parse-csv "resources/2023/day_2_input.csv")))
(def example-game [3 #{{"blue" 1, "red" 2}
                       {"blue" 9, "red" 4}
                       {"red" 3}
                       {"blue" 4, "green" 2, "red" 1}
                       {"blue" 2, "red" 1}
                       {"blue" 4, "green" 3, "red" 3}}])

(defn get-max-colour [colour all-draws]
  (->> all-draws
       (map #(get % colour))
       (map #(if (nil? %) 0 %))
       (apply max)))

(defn power-of-cubes [game]
  (let [all-draws (second game)
        red-cubes (get-max-colour "red" all-draws)
        green-cubes (get-max-colour "green" all-draws)
        blue-cubes (get-max-colour "blue" all-draws)]
    ;; (prn "blue" blue-cubes)
    ;; (prn "green" green-cubes)
    ;; (prn "red" red-cubes)
    (* red-cubes green-cubes blue-cubes)))

(defn get-final-result-v2 [filename]
  (->> filename
       parse-csv
       (map power-of-cubes)
       (remove nil?)
       (reduce +)))
(get-final-result-v2 "resources/2023/day_2_input.csv")


