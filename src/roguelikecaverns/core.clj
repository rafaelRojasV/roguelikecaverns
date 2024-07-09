(ns roguelikecaverns.core
  (:require [lanterna.screen :as s]))

(defmulti tick (fn [state] (get-in state [:screen :type])))

(defmethod tick :main-menu [{:keys [screen input] :as state}]
  (let [options [["Play" (fn [] (assoc state :screen {:type :loading}))]
                 ["Exit" (fn [] (assoc state :exit? true))]]
        max-index (-> options count dec)
        [x y] [0 1]
        selection (get screen :selected 0)
        new-selection (case input
                        :up (-> selection dec (max 0))
                        :down (-> selection inc (min max-index))
                        selection)
        _ (println new-selection)
        drawings (->> options
                      (map-indexed (fn [i [name _]] [name [(+ x 2) (+ y i)]]))
                      (concat [[">" [x (+ y new-selection)]]]))
        action (second (options new-selection))]
    (if (= input :enter)
      (action)
      (-> state
          (update-in [:screen :selected] (constantly new-selection))
          (assoc :drawings drawings)))))

(defmethod tick :loading [state]
  (assoc state :drawings [["loading..." [0 0]]]))

(defn draw [state screen]
  (s/clear screen)
  (println state)
  (-> (for [[text [x y]] (:drawings state)]
        (s/put-string screen x y text))
      doall)
  (s/redraw screen)
  state)

(def init-state
  {:screen {:type :main-menu}
   :exit?  false})

(defn -main [& args]
  (let [scr (s/get-screen :swing)]
    (s/in-screen
      scr
      (loop [state init-state]
        (let [new-state (tick state)
              new-screen? (not= (get-in state [:screen :type])
                                (get-in new-state [:screen :type]))
              new-state (if new-screen? (tick new-state) new-state)]
          (when-not (:exit? new-state)
            (-> new-state
                (draw scr)
                (assoc :input (s/get-key-blocking scr))
                recur)))))))
