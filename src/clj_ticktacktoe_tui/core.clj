(ns clj-ticktacktoe-tui.core
  (:require [clj-ticktacktoe-tui.game :as game]
            [clj-ticktacktoe-tui.tui :as tui])
  (:import (jline Terminal))
  (:gen-class))

(defn setup [game-sate rows columns]
  (add-watch game-sate :renderer
             (fn [_ _ _ new-state]
               (print (str (char 27) "[2J"))                ; clear screen
               (print (str (char 27) "[;H"))                ; move cursor to the top left corner of the screen
               (print (tui/draw-game new-state rows columns))
               (flush)))
  (reset! game-sate game/new-game))

(defn handle-input [input-char game-state]
  (case input-char
    119 (swap! game-state game/move-cursor-up)              ;; Press w
    97 (swap! game-state game/move-cursor-left)             ;; Press a
    115 (swap! game-state game/move-cursor-down)            ;; Press s
    100 (swap! game-state game/move-cursor-right)           ;; Press d
    32 (swap! game-state game/select-at-cursor)             ;; Press SPC
    nil))

(defn -main []
  (let [^Terminal terminal (Terminal/getTerminal)
        columns (.getTerminalWidth terminal)
        rows (.getTerminalHeight terminal)]
    (setup game/game-state rows columns)
    (while true
      (let [char (.readCharacter terminal System/in)]
        (handle-input char game/game-state)))))


