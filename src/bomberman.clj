(ns bomberman
  (:require [clojure.core.async :refer [<! <!! >! >!! chan close! go go-loop pub put! sub timeout]]
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

(def player-names ["alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa"])

(def max-players (Integer/parseInt (or (System/getenv "max_players") 10)))

(def initial-board
  (vec (map (fn [m i]
              (let [d (Dimension. tile-size
                                  (+ (* (mod i tile-root) tile-size) (quot tile-size 2))
                                  (+ (* (quot i tile-root) tile-size) (quot tile-size 2)))]
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

;; (def initial-board
;;   (vec (map (fn [m i]
;;               (let [d (Dimension. tile-size
;;                                   (+ (* (mod i tile-root) tile-size) (quot tile-size 2))
;;                                   (+ (* (quot i tile-root) tile-size) (quot tile-size 2)))]
;;                 (case m :g (Grass. m d) :s (Stone. m d) :w (Wood. m d))))
;;             [:g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :s :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :s :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :w :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :w :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g
;;              :g :g :g :g :g :g :g :g :g :g :g :g]
;;             (range))))

(def game (atom {:bloods #{}
                 :board initial-board
                 :bombs #{}
                 :explosions #{}
                 :players []}))

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

(defn tile-index-from-dimension [board {:keys [x y]}]
  (+ (* (quot y tile-size) tile-root)
     (quot x tile-size)))

(defn tile-from-dimension [board dimension]
  (get board (tile-index-from-dimension board dimension)))

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

(defn reposition [{{direction :direction, from :dimension, speed :speed, then :time, :as movement} :movement, dimension :dimension, :as player} board now]
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

(defn explodable-bombs [now bombs explosions]
  (filter (fn [{:keys [time dimension]}]
            (or (<= time now)
                (first (filter #(overlaps? (:dimension %) dimension) explosions))))
          bombs))

(defn valid-explosion? [board {{:keys [size x y], :as dimension} :dimension}]
  (and (>= x (quot size 2))
       (<= x (- resolution (quot size 2)))
       (>= y (quot size 2))
       (<= y (- resolution (quot size 2)))
       (not (instance? Stone (tile-from-dimension board dimension)))))

(defn create-explosions [board now bombs]
  (mapcat
    (fn [bomb]
      (let [explosion {:dimension (assoc (:dimension bomb) :size explosion-size)
                       :time (+ now 500000000)}
            {{:keys [x y]} :dimension} explosion]
        (filter #(valid-explosion? board %)
                [(assoc (update explosion :dimension assoc :x (- x tile-size)) :id (create-uuid))
                 (assoc (update explosion :dimension assoc :y (- y tile-size)) :id (create-uuid))
                 (assoc (update explosion :dimension assoc :x (+ x tile-size)) :id (create-uuid))
                 (assoc (update explosion :dimension assoc :y (+ y tile-size)) :id (create-uuid))
                 (assoc explosion :id (create-uuid))])))
    bombs))

(defn explode-wood [board explosions]
  (loop [b board
         es explosions]
    (if-let [{:keys [dimension]} (first es)]
      (let [index (tile-index-from-dimension b dimension)
            tile (tile-from-dimension b dimension)]
        (recur (assoc b index (if (instance? Wood tile) (Grass. :g (:dimension tile)) tile)) (next es)))
      b)))

(defn place-bomb [board bombs player]
  (let [dimension (assoc
                    (:dimension (tile-from-dimension board (:dimension player)))
                    :size
                    bomb-size)]
    (if (not-any? #(= (:dimension %) dimension) bombs)
      {:id (create-uuid)
       :dimension dimension
       :time (+ (System/nanoTime) 2000000000)})))

(defn place-player [board]
  (assoc (:dimension (first (shuffle (filter #(instance? Grass %) board))))
         :size
         player-size))

(defn recalculate-players [players board bombs explosions now]
  (loop [bloods #{}
         old players
         new []]
    (if-let [player (first old)]
      (let [immortal (> (:immortal-time player) now)
            p (assoc (reposition player board now) :immortal immortal)]
        (if (and (not immortal) (first (filter #(overlaps? (:dimension %) (:dimension p)) explosions)))
          (recur (conj bloods {:id (create-uuid), :dimension (assoc (:dimension p) :size blood-size), :time (+ now 600000000)})
                 (next old)
                 (conj new (assoc p :dimension (place-player board) :immortal true :immortal-time (+ now immortal-time) :movement nil)))
          (recur bloods (next old) (conj new p))))
      [new bloods])))

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
                                    :name (:name player)
                                    :dimension {:x (float (/ x resolution))
                                                :y (float (/ y resolution))}
                                    :immortal (:immortal player)})
                                 players)}))

(defn push-game [_ _ _ g]
  (put! snapshots {:snapshot :snapshot
                   :data (game->json g)}))

(defn find-player [game id]
  (first (filter #(= (:id %) id) (:players game))))

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
            (swap! game (fn [g] (assoc g :players (remove #(= (:id %) id) (:players g)))))))
        (on-receive
          channel
          (fn [data]
            (let [rpc (json/read-str data)
                  command (rpc "command")
                  arguments (rpc "arguments")
                  now (System/nanoTime)]
              (case command
                "place-bomb"
                (swap! game (fn [g]
                              (if-let [bomb (place-bomb (:board g) (:bombs g) (find-player g id))]
                                (update g :bombs conj bomb)
                                g)))
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
                                                             :time now})
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
        (swap! game (fn [g] (update g :players conj {:id id
                                                     :movement nil
                                                     :dimension (place-player (:board g))
                                                     :immortal true
                                                     :immortal-time (+ (System/nanoTime) immortal-time)
                                                     :name (player-names (count (:player g)))})))
        (send! channel (game->json @game))))))

(defn -main []
  (println "Starting server")
  (go-loop []
    (let [now (System/nanoTime)]
      (let [{:keys [board], :as old} @game
            [explosions
             ex-bombs] (loop [explosions (set (remove-old now (:explosions old)))
                              bombs (set (explodable-bombs now (:bombs old) explosions))
                              bs bombs]
                         (if (empty? bs)
                           [explosions bombs]
                           (let [es (create-explosions board now bs)
                                 b (explodable-bombs now (difference (:bombs old) bombs) es)]
                             (recur (into explosions es) (into bombs b) b))))
            bombs (difference (:bombs old) ex-bombs)
            board (explode-wood (:board old) explosions)
            [players bloods] (recalculate-players (:players old) board bombs explosions now)
            new (assoc old
                       :bloods (into (remove-old now (:bloods old)) bloods)
                       :board board
                       :players players
                       :bombs bombs
                       :explosions explosions)]
        (if (not= old new)
          (swap! game (constantly new))))
      (<! (timeout (/ 1000 60)))
      (recur)))
  (add-watch game :push-game push-game)
  (run-server handler {:port 3000}))
