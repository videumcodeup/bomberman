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
      (on-close
        channel
        (fn [_]
          (println "Connection closed:" id)
          (swap! players (fn [ps] (remove #(= (:id %) id) ps)))))
      (on-receive
        channel
        (fn [data]
          (let [rpc (json/read-str data)
                command (rpc "command")
                arguments (rpc "arguments")]
            (case command
              "start-movement" (swap! players (fn [ps]
                                                (map (fn [p]
                                                       (if (= (:id p) id)
                                                         (let [direction (keyword (first arguments))
                                                               position ((:position p) ({:left 0, :up 1, :right 0, :down 1} direction))]
                                                           (assoc p :movement {:direction direction, :position position, :time (System/nanoTime)}))
                                                         p))
                                                     ps)))
              "stop-movement" (swap! players (fn [ps]
                                               (map (fn [p]
                                                      (if (= (:id p) id)
                                                        (assoc p :movement nil)
                                                        p))
                                                    ps)))))))
      (go-loop []
        (let [snapshot (<! snapshots)]
          (when (open? channel)
            (send! channel (json/write-str snapshot))
            (recur))))
      (swap! players conj {:id id, :movement nil, :position [0 0]}))))

(defn push-state [_ _ _ _]
  (go (>! snapshots {:players (map :position @players), :board @board})))

(defn reposition [{{direction :direction, position :position, then :time} :movement, [x y] :position, :as player}]
  (let [now (System/nanoTime)]
    (assoc player
           :position
           (case direction
             :left [(- position (* (/ (- now then) 1e9) 0.5)) y]
             :up [x (- position (* (/ (- now then) 1e9) 0.5))]
             :right [(+ position (* (/ (- now then) 1e9) 0.5)) y]
             :down [x (+ position (* (/ (- now then) 1e9) 0.5))]
             [x y]))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (swap! players (fn [ps] (map reposition ps)))
    (<! (timeout 17))
    (recur))
  (add-watch players :push-state push-state)
  (add-watch board :push-state push-state)
  (run-server handler {:port 3000}))
