(ns clj-ticktacktoe-tui.game)

(def new-game {:field           [[0 0 0]
                                 [0 0 0]
                                 [0 0 0]]
               :turn            0
               :end-situation   nil
               :cursor-position {:row    0
                                 :column 0}
               :scene           :start})

(def game-state (atom {}))

(def cursor-limits-for-scene {:start   {:rows    1
                                        :columns 1}
                              :game    {:rows    3
                                        :columns 3}
                              :restart {:rows    1
                                        :columns 2}})

(def not-zero? (complement zero?))
(def not-nil? (complement nil?))

(defn free-cell?
  [field row column]
  (zero? (get-in field [row column])))

(defn get-player-number [turn]
  (if (-> turn (mod 2) (= 0)) 1 2))

(defn replace-in-field [game row column]
  (let [turn (:turn game)]
    (assoc-in game [:field row column] (get-player-number turn))))


(defn diagonal-win-combination? [field]
  (or (and (not-zero? (get-in field [0 0]))
           (apply = (for [i (range 3)] (get-in field [i i])))) ;; left to right
      (and (not-zero? (get-in field [0 2]))
           (apply = (for [i (range 3)] (get-in field [i (abs (- 2 i))])))))) ;; right to left

(defn horizontal-win-combination? [field]
  (apply #(or %1 %2 %3)                                     ;; That needed because or is a macro
         (for [row (range 3)]
           (and (not-zero? (get-in field [row 0]))
                (apply = (for [column (range 3)]
                           (get-in field [row column])))))))

(defn vertical-win-combination? [field]
  (apply #(or %1 %2 %3)                                     ;; That needed because or is a macro
         (for [column (range 3)]
           (and (not-zero? (get-in field [0 column]))
                (apply = (for [row (range 3)]
                           (get-in field [row column])))))))

(defn win? [field]
  (or (diagonal-win-combination? field)
      (horizontal-win-combination? field)
      (vertical-win-combination? field)))

(defn draw? [field]
  (apply = (for [row (range 3)]
             (for [column (range 3)]
               (not-zero? (get-in field [row column]))))))


(defn detect-game-end [game]
  (let [field (:field game)
        player (get-player-number (:turn game))]
    (cond
      (win? field) (assoc game :end-situation player)
      (draw? field) (assoc game :end-situation 3)
      :else game)))

(defn reset-cursor-position-if-game-ends [game]
  (if (not-nil? (:end-situation game))
    (assoc game :cursor-position {:row    0
                                  :column 0})
    game))

(defn change-scene-if-game-ends [game]
  (assoc game :scene (if (not-nil? (:end-situation game)) :restart :game)))

(defn select-at-game-scene [game row column]
  (if (free-cell? (:field game) row column)
    (-> game
        (replace-in-field row column)
        detect-game-end
        reset-cursor-position-if-game-ends
        change-scene-if-game-ends
        (update :turn inc))
    ;; If cell isn't free return untouched game state
    game))

(defn select-at-restart-scene [column]
  (if (= column 0)
    (reset! game-state
            ;; Skip welcome screen when playing again
            new-game)
    (System/exit 0)))

(defn select-at-cursor [game]
  (let [row (get-in game [:cursor-position :row])
        column (get-in game [:cursor-position :column])
        scene (:scene game)]
    (case scene
      :start (assoc game :scene :game)
      :game (select-at-game-scene game row column)
      :restart (select-at-restart-scene column))))

(defn can-move-cursor-to? [cursor-position scene]
  (let [limit (scene cursor-limits-for-scene)
        max-row (:rows limit)
        max-column (:columns limit)
        row (:row cursor-position)
        column (:column cursor-position)]
    (and (and (>= row 0) (< row max-row))
         (and (>= column 0) (< column max-column)))))

(defn apply-delta-to-cursor-position [cursor-position row-delta column-delta]
  (->
    cursor-position
    (update :row #(+ % row-delta))
    (update :column #(+ % column-delta))))


(defn update-cursor-position-by-delta [game row-delta column-delta]
  (let [new-cursor-position (apply-delta-to-cursor-position (:cursor-position game)
                                                            row-delta
                                                            column-delta)]
    (if (can-move-cursor-to? new-cursor-position (:scene game))
      (assoc game :cursor-position new-cursor-position)
      game)))

(defn move-cursor-up [game]
  (update-cursor-position-by-delta game -1 0))

(defn move-cursor-down [game]
  (update-cursor-position-by-delta game 1 0))

(defn move-cursor-right [game]
  (update-cursor-position-by-delta game 0 1))

(defn move-cursor-left [game]
  (update-cursor-position-by-delta game 0 -1))
