(ns bomberman
  (:require [clojure.core.async :refer [<! <!! >! >!! chan go go-loop timeout]]
            [clojure.data.json :as json]
            [org.httpkit.server :refer :all])
  (:import [java.util UUID]))

(defprotocol Walkable)

(defrecord Dimension [size x y])

(defrecord Grass [name dimension]
  Walkable)

(defrecord Stone [name dimension])

(defrecord Wood [name dimension])

(defrecord Player [dimension])

(def snapshots (chan))

(def tile-root 10)

(def tile-size (/ 1 tile-root))

(def player-size 0.06)

(def initial-board
  (vec (map (fn [m i]
              (let [d (Dimension. tile-size
                                  (+ (/ (mod i tile-root) tile-root) (/ tile-size 2))
                                  (+ (/ (Math/floor (/ i tile-root)) tile-root) (/ tile-size 2)))]
                (case m :g (Grass. m d) :s (Stone. m d) :w (Wood. m d))))
            [:g :s :s :w :s :w :w :s :w :w
             :g :g :g :g :w :s :w :s :s :w
             :w :s :s :s :g :g :w :s :w :w
             :w :g :s :w :w :g :s :s :g :g
             :s :g :w :w :s :w :g :w :w :s
             :w :g :s :g :g :w :g :s :g :g
             :s :w :w :w :g :g :s :s :w :s
             :s :g :s :w :s :g :w :s :w :g
             :g :w :g :s :s :g :w :g :w :g
             :w :w :w :g :w :w :s :g :g :g]
            (range))))

(def game (atom {:board initial-board, :players []}))

(def axises {:left :x, :up :y, :right :x, :down :y})

(def adders {:left -, :up -, :right +, :down +})

(def opposite-direction {:left :right, :up :down, :right :left, :down :up})

(defn edge-dimension [direction dimension]
  (let [axis (axises direction)]
    (assoc
      dimension
      axis
      ((adders direction) (axis dimension) (/ (:size dimension) 2)))))

(defn tile-from-dimension [board {:keys [x y]}]
  (get board (+ (* (int (float (/ y tile-size))) tile-root)
                (int (float (/ x tile-size))))))

(defn line-from-dimensions [from to]
  (let [[op checker] (if (> to from) [+ >] [- <])]
    (loop [steps [(/ (Math/floor (* from tile-root)) tile-root)]
           to (/ (Math/floor (* to tile-root)) tile-root)]
      (let [step (/ (Math/round (float (* (op (last steps) tile-size) tile-root))) tile-root)]
        (if (checker step to)
          steps
          (recur (conj steps step) to))))))

(defn tiles-from-line [board {:keys [x y], :as from} to]
  (let [[a b pos var] (if (= (:x from) (:x to))
                        [(:y from) (:y to) {:x x} :y]
                        [(:x from) (:x to) {:y y} :x])]
    (map #(tile-from-dimension board (assoc pos var %))
         (line-from-dimensions a b))))

(defn center [direction dimension]
  (let [axis (axises direction)]
    (assoc dimension axis ((adders direction) (axis dimension) (/ (:size dimension) 2)))))

(defn distance [direction a b]
  (let [axis (axises direction)]
    (Math/abs (- (axis a) (axis b)))))

(defn closest-dimension [direction target dimensions]
  (loop [ds (next dimensions)
         closest-dimension (first dimensions)
         closest-distance (distance direction target closest-dimension)]
    (if (not ds)
      closest-dimension
      (let [current-dimension (first ds)
            current-distance (distance direction target current-dimension)]
        (if (< current-distance closest-distance)
          (recur (next ds) current-dimension current-distance)
          (recur (next ds) closest-dimension closest-distance))))))

(defn constrain [board direction player from to]
  (let [tile-line (tiles-from-line board
                                   (edge-dimension (opposite-direction direction) from)
                                   (edge-dimension direction to))
        suggested (center
                    direction
                    (assoc
                      (edge-dimension
                        direction
                        (:dimension
                          (loop [tiles (next tile-line)
                                 tile (first tile-line)]
                            (if (and tiles (satisfies? Walkable (first tiles)))
                              (recur (next tiles) (first tiles))
                              tile))))
                      :size
                      player-size))]
    (closest-dimension direction from [suggested to])))

(defn move [direction dimension from to]
  (let [axis (axises direction)]
    (assoc
      dimension
      axis
      (let [p ((adders direction) (axis dimension) (* (/ (- to from) 1e9) 0.5))]
        (max (min (- 1 (/ player-size 2)) p) (/ player-size 2))))))

(defn reposition [board now {{direction :direction, from :dimension, then :time, :as movement} :movement, dimension :dimension, :as player}]
  (if movement
    (assoc
      player
      :dimension
      (constrain
        board
        direction
        player
        from
        (move direction from then now)))
    player))

(defn push-game [_ _ _ g]
  (go (>! snapshots (merge g {:board (map :name (:board g))
                              :players (map (fn [{{:keys [x y]} :dimension}] [x y]) (:players g))}))))

(defn handler [request]
  (with-channel request channel
    (let [id (str (UUID/randomUUID))]
      (println "New connection:" id)
      (on-close
        channel
        (fn [_]
          (println "Connection closed:" id)
          (swap! game (fn [g] (assoc g :players (remove #(= (:id %) id) (:players g)))))))
      (on-receive
        channel
        (fn [data]
          (let [rpc (json/read-str data)
                command (rpc "command")
                arguments (rpc "arguments")]
            (case command
              "start-movement" (swap! game (fn [g]
                                             (assoc
                                               g
                                               :players
                                               (map (fn [p]
                                                      (if (= (:id p) id)
                                                        (assoc p :movement {:direction (keyword (first arguments))
                                                                            :dimension (:dimension p)
                                                                            :time (System/nanoTime)})
                                                        p))
                                                    (:players g)))))
              "stop-movement" (swap! game (fn [g]
                                            (assoc
                                              g
                                              :players
                                              (map (fn [p]
                                                     (if (= (:id p) id)
                                                       (assoc p :movement nil)
                                                       p))
                                                   (:players g)))))))))
      (go-loop []
        (let [snapshot (<! snapshots)]
          (when (open? channel)
            (send! channel (json/write-str snapshot))
            (recur))))
      (swap! game (fn [g] (assoc g :players (conj (:players g) {:id id, :movement nil, :dimension (Dimension. player-size 0.05 0.05)})))))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (let [now (System/nanoTime)]
      (swap! game (fn [g] (assoc g :players (map #(reposition (:board g) now %) (:players g)))))
      (<! (timeout (/ 1000 60)))
      (recur)))
  (add-watch game :push-game push-game)
  (run-server handler {:port 3000}))
