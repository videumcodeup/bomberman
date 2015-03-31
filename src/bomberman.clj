(ns bomberman
  (:gen-class)
  (:require [clojure.core.async :refer [<! <!! >! >!! chan close! go go-loop pub put! sub timeout]]
            [clojure.data :refer [diff]]
            [clojure.data.json :as json]
            [clojure.set :refer [difference]]
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

(def immortal-time 3000000000)

(def player-names #{"alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa"})

(def max-players (Integer/parseInt (or (System/getenv "max_players") "10")))

(def initial-tiles
  (apply hash-map
         (mapcat
           (fn [m i]
             (let [d (Dimension. tile-size
                                 (+ (* (mod i tile-root) tile-size) (quot tile-size 2))
                                 (+ (* (quot i tile-root) tile-size) (quot tile-size 2)))]
               [i (case m :g (Grass. m d) :s (Stone. m d) :w (Wood. m d))]))
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

(def initial-game {:bloods (hash-map)
                   :bombs (hash-map)
                   :explosions (hash-map)
                   :players (hash-map)
                   :tiles (hash-map)})

(def game (atom (assoc initial-game :tiles initial-tiles)))

(def axises {:left :x, :up :y, :right :x, :down :y})

(def corresponding-axis-directions {:left :up, :up :left, :right :bottom, :bottom :right})

(def adders {:left -, :up -, :right +, :down +})

(def checkers {:left <, :up <, :right >, :down >})

(def opposite-direction {:left :right, :up :down, :right :left, :down :up})

(def opposite-axis {:x :y, :y :x})

(defn create-uuid []
  (str (UUID/randomUUID)))

(defn radius [{size :size}]
  (quot size 2))

(defn distance [direction a b]
  (let [axis (axises direction)
        d (- (axis a) (axis b))]
    (max d (- d))))

(defn overlaps? [target dimension]
  (let [min (+ (radius target) (radius dimension))]
    (and (< (distance :left target dimension) min)
         (< (distance :up target dimension) min))))

(defn edge-dimension [direction dimension]
  (let [axis (axises direction)]
    (assoc
      dimension
      axis
      ((adders direction) (axis dimension) (quot (:size dimension) 2)))))

(defn tile-index-from-dimension [tiles {:keys [x y]}]
  (+ (* (quot y tile-size) tile-root)
     (quot x tile-size)))

(defn tile-from-dimension [tiles dimension]
  (tiles (tile-index-from-dimension tiles dimension)))

(defn line-from-positions [direction from to]
  (loop [steps [(* (quot from tile-size) tile-size)]
         to (* (quot to tile-size) tile-size)]
    (let [step ((adders direction) (last steps) tile-size)]
      (if ((checkers direction) step to)
        steps
        (recur (conj steps step) to)))))

(defn tiles-from-line [direction tiles from to]
  (let [axis (axises direction)
        o-axis (opposite-axis axis)
        half (quot (:size from) 2)
        dimension-sub (assoc from o-axis (- (o-axis from) half))
        dimension-add (assoc from o-axis (+ (o-axis from) half))]
    (filter
      not-empty
      (map (fn [pos]
           (filter #(if-let [dimension (:dimension %)] (overlaps? dimension (assoc from axis pos)))
                   [(tile-from-dimension tiles (assoc dimension-sub axis pos))
                    (tile-from-dimension tiles (assoc dimension-add axis pos))]))
         (line-from-positions direction (axis from) (axis to))))))

(defn center [direction dimension]
  (let [axis (axises direction)]
    (assoc dimension axis ((adders (opposite-direction direction)) (axis dimension) (quot (:size dimension) 2)))))

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

(defn constrain [tiles direction player from to]
  (let [axis (axises direction)
        tile-line (tiles-from-line direction
                                   tiles
                                   (edge-dimension (opposite-direction direction) from)
                                   (edge-dimension direction to))
        suggested (center
                    direction
                    (assoc
                      (edge-dimension
                        direction
                        (:dimension
                          (loop [ts (next tile-line)
                                 t (first tile-line)]
                            (if (and ts (every? #(satisfies? Walkable %) (first ts)))
                              (recur (next ts) (first ts))
                              (first t)))))
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

(defn reposition [{{direction :direction, from :dimension, speed :speed, then :time, :as movement} :movement, dimension :dimension, :as player} tiles now]
  (if movement
    (assoc
      player
      :dimension
      (constrain
        tiles
        direction
        player
        from
        (move direction speed from then now)))
    player))

(defn remove-old [now xs]
  (apply hash-map (flatten (filter (fn [[_ x]] (< now (:time x))) xs))))

(defn explodable-bombs [now bombs explosions]
  (apply hash-map
         (flatten
           (filter (fn [[id {:keys [time dimension]}]]
                     (or (<= time now)
                         (first (filter (fn [[id e]] (overlaps? (:dimension e) dimension)) explosions))))
                   bombs))))

(defn valid-explosion? [tiles [id {{:keys [size x y], :as dimension} :dimension}]]
  (and (>= x (quot size 2))
       (<= x (- resolution (quot size 2)))
       (>= y (quot size 2))
       (<= y (- resolution (quot size 2)))
       (not (instance? Stone (tile-from-dimension tiles dimension)))))

(defn create-explosions [tiles now bombs]
  (apply
    hash-map
    (flatten
      (map
        (fn [[_ bomb]]
          (let [explosion {:dimension (assoc (:dimension bomb) :size explosion-size)
                           :time (+ now 500000000)}
                {{:keys [x y]} :dimension} explosion]
            (filter #(valid-explosion? tiles %)
                    [[(create-uuid) (update explosion :dimension assoc :x (- x tile-size))]
                     [(create-uuid) (update explosion :dimension assoc :y (- y tile-size))]
                     [(create-uuid) (update explosion :dimension assoc :x (+ x tile-size))]
                     [(create-uuid) (update explosion :dimension assoc :y (+ y tile-size))]
                     [(create-uuid) explosion]])))
        bombs))))

(defn explode-wood [tiles explosions]
  (loop [ts tiles
         es explosions]
    (if-let [[_ {:keys [dimension]}] (first es)]
      (let [index (tile-index-from-dimension ts dimension)
            tile (tile-from-dimension ts dimension)]
        (recur (assoc ts index (if (instance? Wood tile) (Grass. :g (:dimension tile)) tile)) (next es)))
      ts)))

(defn place-bomb [tiles bombs player]
  (let [dimension (assoc
                    (:dimension (tile-from-dimension tiles (:dimension player)))
                    :size
                    bomb-size)]
    (if (not-any? (fn [[id b]] (= (:dimension b) dimension)) bombs)
      {:dimension dimension
       :time (+ (System/nanoTime) 2000000000)})))

(defn place-player [tiles]
  (assoc (:dimension (second (first (shuffle (filter (fn [[id t]] (instance? Grass t)) tiles)))))
         :size
         player-size))

(defn recalculate-players [players tiles bombs explosions now]
  (loop [bloods {}
         old players
         new {}]
    (if-let [[id player] (first old)]
      (let [immortal (> (:immortal-time player) now)
            p (assoc (reposition player tiles now) :immortal immortal)]
        (if (and (not immortal) (first (filter (fn [[id e]] (overlaps? (:dimension e) (:dimension p))) explosions)))
          (recur (assoc bloods (create-uuid) {:dimension (assoc (:dimension p) :size blood-size), :time (+ now 600000000)})
                 (next old)
                 (assoc new id (assoc p :dimension (place-player tiles) :immortal true :immortal-time (+ now immortal-time) :movement nil)))
          (recur bloods (next old) (assoc new id p))))
      [new bloods])))

(defn json-value [coll val]
  (case coll
    :bloods (let [{{:keys [x y]} :dimension} val]
              {:dimension {:x (float (/ x resolution))
                           :y (float (/ y resolution))}})
    :tiles (:name val)
    :bombs (let [{{:keys [x y]} :dimension} val]
              {:dimension {:x (float (/ x resolution))
                           :y (float (/ y resolution))}})
    :explosions (let [{{:keys [x y]} :dimension} val]
                  {:dimension {:x (float (/ x resolution))
                               :y (float (/ y resolution))}})
    :players (let [{{:keys [direction speed], :as movement} :movement, {:keys [x y]} :dimension} val]
               {:movement (if movement
                            {:direction direction
                             :speed (float (/ speed tile-root))})
                :name (:name val)
                :dimension {:x (float (/ x resolution))
                            :y (float (/ y resolution))}
                :immortal (:immortal val)})))

(defn game->json [o n]
  (json/write-str
    (loop [txs []
           collections (keys initial-game)]
      (if-let [coll (first collections)]
        (let [[prev adds _] (diff (coll o) (coll n))
              [rets _ _] (diff (set (keys prev)) (set (keys adds)))]
          (recur (concat txs
                         (map #(array-map :id %, :coll coll, :type "retract") rets)
                         (map (fn [[id _]] {:id id, :coll coll, :type "add", :value (json-value coll ((coll n) id))}) adds))
                 (next collections)))
        txs))))

(defn push-game [_ _ o n]
  (put! snapshots {:snapshot :snapshot
                   :data (game->json o n)}))

(defn handler [request]
  (with-channel request channel
    (when (< (count (:players @game)) max-players)
      (let [snaps (chan)
            id (create-uuid)]
        (println "New connection:" id)
        (sub snapshots-pub :snapshot snaps)
        (on-close
          channel
          (fn [_]
            (println "Connection closed:" id)
            (close! snaps)
            (swap! game (fn [g] (update g :players dissoc id)))))
        (on-receive
          channel
          (fn [data]
            (if-let [rpc (try (json/read-str data) (catch Exception e nil))]
              (if (and (map? rpc) (string? (rpc "command")) (coll? (rpc "arguments")))
                (let [command (rpc "command")
                      arguments (rpc "arguments")
                      now (System/nanoTime)]
                  (case command
                    "place-bomb"
                    (swap! game (fn [g]
                                  (if-let [bomb (place-bomb (:tiles g) (:bombs g) ((:players g) id))]
                                    (update g :bombs assoc (create-uuid) bomb)
                                    g)))
                    "start-movement"
                    (if-let [direction (keyword (first arguments))]
                      (if (#{:left :up :right :down} direction)
                        (swap! game (fn [g]
                                      (let [p ((:players g) id)]
                                        (update g :players assoc id (assoc p :movement {:direction direction
                                                                                        :dimension (:dimension p)
                                                                                        :speed (quot tile-root 4)
                                                                                    :time now})))))))
                    "stop-movement"
                    (swap! game (fn [g]
                                  (let [p ((:players g) id)]
                                    (update g :players assoc id (assoc p :movement nil)))))))))))
        (go-loop []
          (let [snap (<! snaps)]
            (when (and snap (open? channel))
              (send! channel (:data snap))
              (recur))))
        (swap! game (fn [g] (update g :players assoc id
                                                     {:movement nil
                                                      :dimension (place-player (:tiles g))
                                                      :immortal true
                                                      :immortal-time (+ (System/nanoTime) immortal-time)
                                                      :name (let [pns (set (map (fn [[_ p]] (:name p)) (:players g)))]
                                                              (first (filter #(not (pns %)) player-names)))})))
        (send! channel (json/write-str {:id id}))
        (send! channel (game->json initial-game @game))))))

(defn map-difference [a b]
  (apply hash-map (mapcat #(vector % (a %)) (difference (set (keys a)) (set (keys b))))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (let [now (System/nanoTime)]
      (let [{:keys [tiles], :as old} @game
            [explosions
             ex-bombs] (loop [explosions (remove-old now (:explosions old))
                              bombs (explodable-bombs now (:bombs old) explosions)
                              bs bombs]
                         (if (empty? bs)
                           [explosions bombs]
                           (let [es (create-explosions tiles now bs)
                                 b (explodable-bombs now (map-difference (:bombs old) bombs) es)]
                             (recur (into explosions es) (into bombs b) b))))
            bombs (map-difference (:bombs old) ex-bombs)
            tiles (explode-wood (:tiles old) explosions)
            [players bloods] (recalculate-players (:players old) tiles bombs explosions now)
            new (assoc old
                       :bloods (into (remove-old now (:bloods old)) bloods)
                       :tiles tiles
                       :players players
                       :bombs bombs
                       :explosions explosions)]
        (if (not= old new)
          (swap! game (constantly new))))
      (<! (timeout (/ 1000 60)))
      (recur)))
  (add-watch game :push-game push-game)
  (run-server handler {:port 3000}))
