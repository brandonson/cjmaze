(ns brandonson.cjmaze.core
  (:use jordanlewis.data.union-find))

;A wall in a 2D maze.  Side is one of 'horizontal or 'vertical.
;Walls are to the left or bottom of the cell they are at the
;(x,y) position of.

(defrecord Wall2D
  [x y side])

;A cell in a 2D maze
(defrecord Cell2D
  [x y])

(defrecord Maze [walls width height])

(defn wall-adjusts-coord
  "Determines whether the given wall is placed at +1 from for the given coord.
   'coord' should be either :x or :y"
  [wall coord]
  (let [side (:side wall)]
    (if (= side 'horizontal)
      (= coord :x)
      (= coord :y))))

(defn order-maze-walls
  [maze]
  (->Maze (sort-by (juxt :x :y :side) (:walls maze))
          (:width maze)
          (:height maze)))

(declare add-walls do-maze-generation create-cell-uf)
(defn gen-maze
  "Generates a maze.  The returned maze is just a list of walls."
  [width height]
  (let [with-horiz (add-walls (list) 'horizontal (- width 1) height)
        with-vert (add-walls with-horiz 'vertical width (- height 1))
        shuffled (shuffle with-vert)
        uf (create-cell-uf width height)]
    (->Maze (do-maze-generation shuffled uf) width height)))
        
(defn- add-walls
  "conj's walls onto the given maze, using the given side and dimensions"
  [start-maze side width height]
  (loop [complete-count 0
         maze start-maze]
    (if (< (quot complete-count width) height) 
      (recur (inc complete-count)
             (conj maze (->Wall2D (mod complete-count width)
                                  (quot complete-count width)
                                  side)))
      maze)))

(defn- create-cell-uf
  "Creates a cell union find structure with all the cells for the given width and height"
  [width height]
  (loop [x 0
         y 0
         uf (union-find)]
    (if (< y height)
      (if (< x width)
        (recur (inc x)
               y
               (conj uf (->Cell2D x y)))
        (recur 0
               (inc y)
               uf))
      uf)))

(declare wall-cell far-wall-cell)
(defn- do-maze-generation
  "Removes walls from a maze until we get something that works."
  [full-wall-list full-uf]
  (loop [wall-list full-wall-list
         uf full-uf
         remaining (list)]
    (if (empty? wall-list)
      remaining
      (let [wall (first wall-list)
            close-cell (wall-cell wall)
            far-cell (far-wall-cell wall)]
        (if (= (nth (get-canonical uf close-cell) 1)
               (nth (get-canonical uf far-cell) 1))
          (recur (rest wall-list)
                 uf
                 (conj remaining wall))
          (recur (rest wall-list)
                 (union uf close-cell far-cell)
                 remaining))))))

(defn- wall-cell
  "Creates a cell based on the given wall"
  [wall]
  (->Cell2D (:x wall) (:y wall)))

(defn- far-wall-cell
  "Creates a cell for the cell on the far side of the given wall."
  [wall]
  (let [x (:x wall)
        y (:y wall)]
    (if (= 'horizontal (:side wall))
      (->Cell2D (inc x) y)
      (->Cell2D x (inc y)))))
