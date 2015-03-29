(ns bomberman
  (:require [clojure.core.async :refer [<! <!! >! >!! chan go go-loop pub put! sub timeout]]
            [clojure.data.json :as json]
            [org.httpkit.server :refer :all])
  (:import [java.util UUID]))

(defn div [x y]
  (with-precision 5 (/ (bigdec x) (bigdec y))))

(defprotocol Walkable)

(defrecord Dimension [size x y])

(defrecord Grass [name dimension]
  Walkable)

(defrecord Stone [name dimension])

(defrecord Wood [name dimension])

(defrecord Player [dimension])

(def snapshots (chan))

(def snapshots-pub (pub snapshots :snapshot))

(def tile-root 12)

(def tile-size (div 1 tile-root))

(def player-size 0.06)

(def initial-board
  (vec (map (fn [m i]
              (let [d (Dimension. tile-size
                                  (+ (div (mod i tile-root) tile-root) (div tile-size 2))
                                  (+ (div (Math/floor (div i tile-root)) tile-root) (div tile-size 2)))]
                (case m :g (Grass. m d) :s (Stone. m d) :w (Wood. m d))))

            [:s :s :s :s :s :s :s :s :s :s :s :s
             :s :g :s :s :w :s :w :w :s :w :w :s
             :s :g :g :g :g :w :s :w :s :s :w :s
             :s :w :s :s :s :g :g :w :s :w :w :s
             :s :w :g :s :w :w :g :s :s :g :g :s
             :s :s :g :w :w :s :w :g :w :w :s :s
             :s :w :g :s :g :g :w :g :s :g :g :s
             :s :s :w :w :w :g :g :s :s :w :s :s
             :s :s :g :s :w :s :g :w :s :w :g :s
             :s :g :w :g :s :s :g :w :g :w :g :s
             :s :w :w :w :g :w :w :s :g :g :g :s
             :s :s :s :s :s :s :s :s :s :s :s :s]
            (range))))

(def game (atom {:board initial-board, :players []}))

(def axises {:left :x, :up :y, :right :x, :down :y})

(def corresponding-axis-directions {:left :up, :up :left, :right :bottom, :bottom :right})

(def adders {:left -, :up -, :right +, :down +})

(def checkers {:left <, :up <, :right >, :down >})

(def opposite-direction {:left :right, :up :down, :right :left, :down :up})

(def opposite-axis {:x :y, :y :x})

(defn inside? [target {:keys [x y]}]
  (let [half (div (:size target) 2)]
    (and (> x (- (:x target) half)) (< x (+ (:x target) half))
         (> y (- (:y target) half)) (< y (+ (:y target) half)))))

(defn square-dimensions [{:keys [x y], :as dimension}]
  (let [half (div (:size dimension) 2)]
    [(assoc dimension :x (- x half) :y (- y half)) (assoc dimension :x (+ x half) :y (- y half))
     (assoc dimension :x (- x half) :y (+ y half)) (assoc dimension :x (+ x half) :y (+ y half))]))

(defn overlaps? [target dimension]
  (some #(inside? target %) (square-dimensions dimension)))

(defn edge-dimension [direction dimension]
  (let [axis (axises direction)]
    (assoc
      dimension
      axis
      ((adders direction) (axis dimension) (div (:size dimension) 2)))))

(defn tile-from-dimension [board {:keys [x y]}]
  (get board (+ (* (int (div y tile-size)) tile-root)
                (int (div x tile-size)))))

(defn line-from-positions [direction from to]
  (loop [steps [(div (Math/floor (* from tile-root)) tile-root)]
         to (div (Math/floor (* to tile-root)) tile-root)]
    (let [step (div (Math/round (float (* ((adders direction) (last steps) tile-size) tile-root))) tile-root)]
      (if ((checkers direction) step to)
        steps
        (recur (conj steps step) to)))))

(defn tiles-from-line [direction board from to]
  (let [axis (axises direction)
        o-axis (opposite-axis axis)
        half (div (:size from) 2)
        dimension-sub (assoc from o-axis (- (o-axis from) half))
        dimension-add (assoc from o-axis (+ (o-axis from) half))]
    (map (fn [pos]
           (filter #(overlaps? (:dimension %) (assoc from axis pos))
                   [(tile-from-dimension board (assoc dimension-sub axis pos))
                    (tile-from-dimension board (assoc dimension-add axis pos))]))
         (line-from-positions direction (axis from) (axis to)))))

(defn center [direction dimension]
  (let [axis (axises direction)]
    (assoc dimension axis ((adders (opposite-direction direction)) (axis dimension) (div (:size dimension) 2)))))

(defn distance [direction a b]
  (let [axis (axises direction)]
    (div (Math/abs (float (- (axis a) (axis b)))) 1)))

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
  (let [axis (axises direction)
        tile-line (tiles-from-line direction
                                   board
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
                            (if (and tiles (every? #(satisfies? Walkable %) (first tiles)))
                              (recur (next tiles) (first tiles))
                              (first tile)))))
                      :size
                      player-size))]
    (closest-dimension direction from [(assoc from axis (axis suggested)) to])))

(defn move [direction speed dimension from to]
  (let [axis (axises direction)]
    (assoc
      dimension
      axis
      (let [p ((adders direction) (axis dimension) (* (div (- to from) 1e9) speed))]
        (max (min (- 1 (div player-size 2)) p) (div player-size 2))))))

(defn reposition [board now {{direction :direction, from :dimension, speed :speed, then :time, :as movement} :movement, dimension :dimension, :as player}]
  (if movement
    (assoc
      player
      :dimension
      (constrain
        board
        direction
        player
        from
        (move direction speed from then now)))
    player))

(defn push-game [_ _ _ g]
  (put! snapshots {:snapshot :snapshot
                   :data (json/write-str (merge g {:board (map :name (:board g))
                                                   :players (:players g)}))}))

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
                                                                            :speed 0.5
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
      (let [snaps (chan)]
        (sub snapshots-pub :snapshot snaps)
        (go-loop []
          (let [{data :data} (<! snaps)]
            (when (open? channel)
              (send! channel data)
              (recur)))))
      (swap! game (fn [g] (assoc g :players (conj (:players g) {:id id, :movement nil, :dimension (Dimension. player-size 0.15 0.15)})))))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (let [now (System/nanoTime)]
      (let [old @game
            new (assoc old :players (map #(reposition (:board old) now %) (:players old)))]
        (if (not= old new)
          (swap! game (constantly new))))
      (<! (timeout (/ 1000 60)))
      (recur)))
  (add-watch game :push-game push-game)
  (run-server handler {:port 3000}))
