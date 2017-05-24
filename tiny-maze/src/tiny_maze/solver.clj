(ns tiny-maze.solver
  "Example:
  [[:S 0  1]
   [1  0  1]
   [1  0 :E]]

  - :S : start of the maze
  - :E : end of the maze
  - 1 : This is a wall that you cannot pass through
  - 0 : A free space that you can move through.")

(defn solve-maze [maze]
  (letfn [(map-cells [cell] (if-not (= 1 cell) :x cell))]
    (map #(map map-cells %) maze)))

(defn index-of [s candidate]
  (first (keep-indexed #(when (= candidate %2) %1) s)))

(defn locate [maze symbol]
  (first (keep-indexed #(when-let [x (index-of %2 symbol)] {:x x, :y %1}) maze)))

(defn distance [maze from to]
  (letfn [(abs [n] (max n (- n)))]
    (let [{from-x :x, from-y, :y} (locate maze from)
          {to-x :x, to-y :y} (locate maze to)]
      (+ (abs (- from-x to-x)) (abs (- from-y to-y))))))

(defn neighbour-positions [maze symbol]
  (let [{x :x, y :y} (locate maze symbol)
        max-x (count (first maze))
        max-y (count maze)]
    (filter (fn [{x :x y :y}] (and (< -1 x max-x)
                                   (< -1 y max-y)))
            (map #(hash-map :x (+ x (first %)), :y (+ y (second %)))
                 [[0 -1] [1 0] [0 1] [-1 0]]))))

(defn update-maze [maze {x :x, y :y} symbol]
  (update-in maze [y x] (fn [_] symbol)))

(defn get-symbol [maze {x :x, y :y}]
  (get-in maze [y x]))
