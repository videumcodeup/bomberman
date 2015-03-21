(ns bomberman
  (:require [clojure.core.async :refer [<! <!! >! >!! chan go go-loop timeout]]
            [clojure.data.json :as json]
            [org.httpkit.server :refer :all])
  (:import [java.util UUID]))

(def players (atom []))

(def board
  (atom [[:g :s :s :w :s :w :w :s :w :w]
         [:g :g :g :g :w :s :w :s :s :w]
         [:w :s :s :s :g :g :w :s :w :w]
         [:w :g :s :w :w :g :s :s :g :g]
         [:s :g :w :w :s :w :g :w :w :s]
         [:w :g :s :g :g :w :g :s :g :g]
         [:s :w :w :w :g :g :s :s :w :s]
         [:s :g :s :w :s :g :w :s :w :g]
         [:g :w :g :s :s :g :w :g :w :g]
         [:w :w :w :g :w :w :s :g :g :g]]))

(def snapshots (chan))

(defn handler [request]
  (with-channel request channel
    (let [id (str (UUID/randomUUID))]
      (println "New connection:" id)
      (on-close channel (fn [_]
                          (println "Connection closed:" id)
                          (swap! players (fn [ps] (remove #(= (:id %) id) ps)))))
      (go-loop []
        (let [snapshot (<! snapshots)]
          (when (open? channel)
            (send! channel (json/write-str snapshot))
            (recur))))
      (swap! players conj {:id id, :directions {:right {:position 0, :time (System/nanoTime)}}, :position [0 0]}))))

(defn push-state [_ _ _ _]
  (go (>! snapshots {:players (map :position @players), :board @board})))

(defn reposition [{{:keys [right]} :directions, position :position, :as player}]
  (let [time (System/nanoTime)]
    (if-let [{r-p :position, r-t :time} right]
      (assoc player :position [(+ r-p (* (/ (- time r-t) 1e9) 0.2)) (second position)]))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (swap! players (constantly (map reposition @players)))
    (<! (timeout 33))
    (recur))
  (add-watch players :push-state push-state)
  (add-watch board :push-state push-state)
  (run-server handler {:port 3000}))
