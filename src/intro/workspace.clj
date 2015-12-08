(ns intro.workspace
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [firstclassshapes.core :refer :all])

  (:import [connectFour ConnectFourAI]))



(defn setup []
  "Scene setup - is called only once at start"
  (q/frame-rate 40)
  (q/color-mode :rgb); cell body color green

  ; initial state
  {:board  [[0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0]]
   :active-player 1
   :player-win false})

;----

(defn bn [state x y]
  (get (get (:board state) y) x))

(defn draw-square [x y r g b c]
  (q/fill r g b)
  (q/rect (* x 100) (* y 100) 100 100 c))


(defn draw-board [state]
  (doseq [y (range 6)
          x (range 7)]
    (cond (= 0 (bn state x y))
          (draw-square x y 80 255 80 0)
          (= 1 (bn state x y))
          (draw-square x y 255 0 0 100)
          (= -1 (bn state x y))
          (draw-square x y 10 10 10 100)
          :else (println "You broke something!"))))


;----

(defn draw-red-board [state]
  (draw-board state)
  (q/fill 255)
  (q/text-size 50)
  (q/text "Red Wins" 220 200)
  (q/text "Right Click to Reset" 150 400))

(defn draw-black-board [state]
  (draw-board state)
  (q/fill 255)
  (q/text-size 50)
  (q/text "Black Wins" 220 200)
  (q/text "Right Click to Reset" 150 400))

(defn player-won [state]
  (if (= (:active-player state) -1) (draw-red-board state) (draw-black-board state)))


;----

(defn reset-board [state]
  (if (= (:player-win state) true)
  (assoc state :player-win false :board [[0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0]]) state))


(defn update-board [state x]
  (if
    (not= (get (get (:board state) 0) x) 0)
    state
    (doseq [y (range 6)]
      (if
        (= (bn state x y) 0)
        (def state-new (assoc state
                         :board (assoc (:board state) y (assoc (get (:board state) y) x (:active-player state)))
                         :active-player (- (:active-player state))))
        (:board state)))) state-new)


;----

(defn check-win [state]
  (def state-new state)
  (doseq [y (range 6)
          x (range 4)]
    (def z (+ (bn state x y) (bn state (+ x 1) y) (bn state (+ x 2) y) (bn state (+ x 3) y)))
    (if (or (= 4 z) (= -4 z)) (def state-new (assoc state-new :player-win true))))
  (doseq [y (range 3)
          x (range 7)]
    (def z (+ (bn state x y) (bn state x (+ y 1)) (bn state x (+ y 2)) (bn state x (+ y 3))))
    (if (or (= 4 z) (= -4 z)) (def state-new (assoc state-new :player-win true))))
  (doseq [y (range 3)
          x (range 4)]
    (def z (+ (bn state x y) (bn state (+ x 1) (+ y 1)) (bn state (+ x 2) (+ y 2)) (bn state (+ x 3) (+ y 3))))
    (if (or (= 4 z) (= -4 z)) (def state-new (assoc state-new :player-win true))))
  (doseq [y (range 3)
          x (range 3 7)](defn bn [state x y]
  (get (get (:board state) y) x))
    (def z (+ (bn state x y) (bn state (- x 1) (+ y 1)) (bn state (- x 2) (+ y 2)) (bn state (- x 3) (+ y 3))))
    (if (or (= 4 z) (= -4 z)) (def state-new (assoc state-new :player-win true))))
  (if (empty? (for [y (range 6)
                         x (range 7)
                         :when (= (bn state x y) 0)]
                1)) (def state-new (reset-board (assoc state :player-win true)))) state-new)

(defn ai [board]
  (rand-int 7))


;----

(defn update [state]
  state)


(defn draw [state]
  "Draws current state"
  (q/background 80 255 80)
    (q/stroke 255 255 255)
  (if (not (:player-win state))
    (draw-board state) (player-won state)))


(defn mouse-clicked [state event]
  (cond (= (:button event) :right)
        (reset-board state)
        (:player-win state)
        state
        (= (:button event) :left)
        (if
          (= (:active-player state) 1)
          (check-win (update-board state (quot (:x event) 100)))
          (check-win (update-board state (ai (:board state)))))
        (= (:button event) :center)
        state
        :else "you broke something"))


(q/defsketch gol
  :title "Connect Four"
  :size [700 600]
  :setup setup
  :update update
  :draw draw
  :mouse-clicked mouse-clicked
  :middleware [m/fun-mode])




