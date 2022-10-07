(ns clj-ticktacktoe-tui.tui
  (:gen-class)
  (:require [clansi :refer :all]
            [clojure.string :as str]))

(def start-scene "W A S D to move, SPC to place. Press SPC to continue")

(def restart-win-scene-template ["* wins!"
                                 "restart [*] yes [*] no"])

(def restart-draw-scene-template ["Draw!"
                                  "restart [*] yes [*] no"])

(def game-scene-template ["[*] [*] [*]"
                          "[*] [*] [*]"
                          "[*] [*] [*]"])

(defn margin [area-size element-size margin-symbol]
  (apply str (->
               element-size
               (- area-size) abs                            ;; Calculate free space
               (/ 2) int
               (repeat margin-symbol))))

(defn centralize-line-vertically [columns line]
  (let [margin (margin columns (count line) \space)]
    (str margin line margin)))

(defn centralize-lines [rows columns lines]
  (let [margin (margin rows (count lines) \newline)
        centralized-lines (->> lines (map #(centralize-line-vertically columns %)) (str/join \newline) (apply str))]
    (str margin centralized-lines margin)))

(defn player-symbol [player]
  (case player
    0 " "
    1 "X"
    2 "O"))

(defn field-cells-content [game]
  (let [cursor-row (get-in game [:cursor-position :row])
        cursor-column (get-in game [:cursor-position :column])
        field (:field game)]
    (flatten
      (for [row (range 3)]
        (for [column (range 3)]
          (let [cell-content (player-symbol (get-in field [row column]))]
            (if (and (= row cursor-row) (= column cursor-column))
              (style cell-content :bg-white :black)
              cell-content)))))))

(defn prepare-template [template rows columns]
  (str/replace
    (centralize-lines rows columns template) #"\*" "%s"))

(defn process-template [template content rows columns]
  (apply format (conj content (prepare-template template rows columns))))


(defn draw-game-scene [game rows columns]
  (process-template game-scene-template (field-cells-content game) rows columns))

(defn restart-draw-cells-content [game]
  (let [cursor-column (get-in game [:cursor-position :column])]
    (for [column (range 2)]
      (if (= cursor-column column) (style " " :bg-white) " "))))

(defn restart-win-cells-content [game]
  (conj (restart-draw-cells-content game) (player-symbol (:end-situation game))))

(defn draw-restart-scene [game rows columns]
  (if (= 3 (:end-situation game))
    (process-template restart-draw-scene-template (restart-draw-cells-content game) rows columns)
    (process-template restart-win-scene-template (restart-win-cells-content game) rows columns)))


(defn draw-start-scene [rows columns]
  (centralize-lines rows columns [start-scene]))

(defn draw-game [game rows columns]
  (case (:scene game)
    :start (draw-start-scene rows columns)
    :game (draw-game-scene game rows columns)
    :restart (draw-restart-scene game rows columns)))
