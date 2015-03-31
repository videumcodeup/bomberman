(ns bomberman
  (:require [clojure.core.async :refer [<! <!! >! >!! chan close! go go-loop pub put! sub timeout]]
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

(def snapshots-pub (pub snapshots :snapshot))

(def tile-root 12)

(def resolution (* tile-root 1000000000))

(def tile-size (quot resolution tile-root))

(def player-size (quot tile-size 4/3))

(def bomb-size (quot tile-size 4/3))

(def explosion-size (quot tile-size 4/3))

(def blood-size (quot tile-size 4/3))

;; (def initial-board
;;   (vec (map (fn [m i]
;;               (let [d (Dimension. tile-size
;;                                   (+ (* (mod i tile-root) tile-size) (quot tile-size 2))
;;                                   (+ (* (quot i tile-root) tile-size) (quot tile-size 2)))]
;;                 (case m :g (Grass. m d) :s (Stone. m d) :w (Wood. m d))))
;;             [:s :s :s :s :s :s :s :s :s :s :s :s
;;              :s :g :s :s :w :s :w :w :s :w :w :s
;;              :s :g :g :g :g :w :s :w :s :s :w :s
;;              :s :w :s :s :s :g :g :w :s :w :w :s
;;              :s :w :g :s :w :w :g :s :s :g :g :s
;;              :s :s :g :w :w :s :w :g :w :w :s :s
;;              :s :w :g :s :g :g :w :g :s :g :g :s
;;              :s :s :w :w :w :g :g :s :s :w :s :s
;;              :s :s :g :s :w :s :g :w :s :w :g :s
;;              :s :g :w :g :s :s :g :w :g :w :g :s
;;              :s :w :w :w :g :w :w :s :g :g :g :s
;;              :s :s :s :s :s :s :s :s :s :s :s :s]
;;             (range))))

(def initial-board
  (vec (map (fn [m i]
              (let [d (Dimension. tile-size
                                  (+ (* (mod i tile-root) tile-size) (quot tile-size 2))
                                  (+ (* (quot i tile-root) tile-size) (quot tile-size 2)))]
                (case m :g (Grass. m d) :s (Stone. m d) :w (Wood. m d))))
            [:g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g
             :g :g :g :g :g :g :g :g :g :g :g :g]
            (range))))

(def game (atom {:bloods []
                 :board initial-board
                 :bombs []
                 :explosions []
                 :players []}))

(def axises {:left :x, :up :y, :right :x, :down :y})

(def corresponding-axis-directions {:left :up, :up :left, :right :bottom, :bottom :right})

(def adders {:left -, :up -, :right +, :down +})

(def checkers {:left <, :up <, :right >, :down >})

(def opposite-direction {:left :right, :up :down, :right :left, :down :up})

(def opposite-axis {:x :y, :y :x})

(defn create-uuid []
  (str (UUID/randomUUID)))

(defn inside? [target {:keys [x y]}]
  (let [half (quot (:size target) 2)]
    (and (> x (- (:x target) half)) (< x (+ (:x target) half))
         (> y (- (:y target) half)) (< y (+ (:y target) half)))))

(defn square-dimensions [{:keys [x y], :as dimension}]
  (let [half (quot (:size dimension) 2)]
    [(assoc dimension :x (- x half) :y (- y half)) (assoc dimension :x (+ x half) :y (- y half))
     (assoc dimension :x (- x half) :y (+ y half)) (assoc dimension :x (+ x half) :y (+ y half))]))

(defn overlaps? [target dimension]
  (some #(inside? target %) (square-dimensions dimension)))

(defn edge-dimension [direction dimension]
  (let [axis (axises direction)]
    (assoc
      dimension
      axis
      ((adders direction) (axis dimension) (quot (:size dimension) 2)))))

(defn tile-from-dimension [board {:keys [x y]}]
  (get board (+ (* (quot y tile-size) tile-root)
                (quot x tile-size))))

(defn line-from-positions [direction from to]
  (loop [steps [(* (quot from tile-size) tile-size)]
         to (* (quot to tile-size) tile-size)]
    (let [step ((adders direction) (last steps) tile-size)]
      (if ((checkers direction) step to)
        steps
        (recur (conj steps step) to)))))

(defn tiles-from-line [direction board from to]
  (let [axis (axises direction)
        o-axis (opposite-axis axis)
        half (quot (:size from) 2)
        dimension-sub (assoc from o-axis (- (o-axis from) half))
        dimension-add (assoc from o-axis (+ (o-axis from) half))]
    (filter
      not-empty
      (map (fn [pos]
           (filter #(if-let [dimension (:dimension %)] (overlaps? dimension (assoc from axis pos)))
                   [(tile-from-dimension board (assoc dimension-sub axis pos))
                    (tile-from-dimension board (assoc dimension-add axis pos))]))
         (line-from-positions direction (axis from) (axis to))))))

(defn center [direction dimension]
  (let [axis (axises direction)]
    (assoc dimension axis ((adders (opposite-direction direction)) (axis dimension) (quot (:size dimension) 2)))))

(defn distance [direction a b]
  (let [axis (axises direction)
        d (- (axis a) (axis b))]
    (max d (- d))))

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
      (let [p ((adders direction) (axis dimension) (* (- to from) speed))]
        (max (min (- resolution (quot player-size 2)) p) (quot player-size 2))))))

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

(defn remove-old [now xs]
  (filter #(< now (:time %)) xs))

(defn create-explosions [board now bomb]
  (if (> now (:time bomb))
    [{:dimension (assoc (:dimension bomb) :size explosion-size)
      :time (+ (System/nanoTime) 500000000)}]
    []))

(defn game->json [{:keys [bloods board bombs explosions players]}]
  (json/write-str {:bloods (map (fn [{{:keys [x y]} :dimension, id :id}]
                                  {:dimension {:x (float (/ x resolution))
                                               :y (float (/ y resolution))}
                                   :id id})
                                bloods)
                   :board (map :name board)
                   :bombs (map (fn [{{:keys [x y]} :dimension}]
                                 {:dimension {:x (float (/ x resolution))
                                              :y (float (/ y resolution))}})
                               bombs)
                   :explosions (map (fn [{{:keys [x y]} :dimension}]
                                      {:dimension {:x (float (/ x resolution))
                                                   :y (float (/ y resolution))}})
                                    explosions)
                   :players (map (fn [{{:keys [direction speed], :as movement} :movement, {:keys [x y]} :dimension, :as player}]
                                   {:id (:id player)
                                    :movement (if movement
                                                {:direction direction
                                                 :speed (float (/ speed tile-root))})
                                    :dimension {:x (float (/ x resolution))
                                                :y (float (/ y resolution))}})
                                 players)}))

(defn push-game [_ _ _ g]
  (put! snapshots {:snapshot :snapshot
                   :data (game->json g)}))

(defn find-player [game id]
  (first (filter #(= (:id %) id) (:players game))))

(defn handler [request]
  (with-channel request channel
    (let [snaps (chan)
          id (create-uuid)]
      (println "New connection:" id)
      (sub snapshots-pub :snapshot snaps)
      (on-close
        channel
        (fn [_]
          (println "Connection closed:" id)
          (close! snaps)
          (swap! game (fn [g] (assoc g :players (remove #(= (:id %) id) (:players g)))))))
      (on-receive
        channel
        (fn [data]
          (let [rpc (json/read-str data)
                command (rpc "command")
                arguments (rpc "arguments")]
            (case command
              "place-bomb"
              (swap! game (fn [g]
                            (assoc
                              g
                              :bombs
                              (conj
                                (:bombs g)
                                {:dimension (assoc (:dimension (tile-from-dimension (:board g) (:dimension (find-player g id))))
                                                   :size
                                                   bomb-size)
                                 :time (+ (System/nanoTime) 2000000000)}))))
              "start-movement"
              (swap! game (fn [g]
                            (assoc
                              g
                              :players
                              (map (fn [p]
                                     (if (= (:id p) id)
                                       (assoc p :movement {:direction (keyword (first arguments))
                                                           :dimension (:dimension p)
                                                           :speed (quot tile-root 2)
                                                           :time (System/nanoTime)})
                                       p))
                                   (:players g)))))
              "stop-movement"
              (swap! game (fn [g]
                            (assoc
                              g
                              :players
                              (map (fn [p]
                                     (if (= (:id p) id)
                                       (assoc p :movement nil)
                                       p))
                                   (:players g)))))))))
      (go-loop []
        (let [snap (<! snaps)]
          (when (and snap (open? channel))
            (send! channel (:data snap))
            (recur))))
      (swap! game (fn [g] (assoc g :players (conj (:players g) {:id id, :movement nil, :dimension (Dimension. player-size (+ tile-size (quot tile-size 2)) (+ tile-size (quot tile-size 2)))}))))
      (send! channel (game->json @game)))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (let [now (System/nanoTime)]
      (let [old @game
            new (assoc old
                       :players (map #(reposition (:board old) now %) (:players old))
                       :bombs (remove-old now (:bombs old))
                       :explosions (into (remove-old now (:explosions old)) (mapcat #(create-explosions (:board old) now %) (:bombs old))))]
        (if (not= old new)
          (swap! game (constantly new))))
      (<! (timeout (/ 1000 60)))
      (recur)))
  (add-watch game :push-game push-game)
  (run-server handler {:port 3000}))
