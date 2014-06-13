(ns brandonson.cjmaze.grid
  (:require [brandonson.cjmaze.core :refer [wall-adjusts-coord]]))

;A maze in grid form.  'grid' is a 2D vector where each element is either 'open or 'wall.
(defrecord MazeGrid [grid width height])

(defn at-grid-xy
  "Gets the value in the given grid at (x,y)."
  [grid x y]
  (nth (nth (:grid grid) x) y))

(defn maze-grid-dimension
  "Determines the size of a maze grid in the given dimension for
   the given maze.  'dimension' should be either :width or :height"
  [maze dimension]
  (let [base (dimension maze)]
    ;2 spaces per cell to allow room for walls
    ;+1 for the walls on the top and left
    (+ 1 (* 2 base)))) 

(defn maze-grid-height
  "Determines the height of a maze grid for the given maze."
  [maze]
  (maze-grid-dimension maze :height))

(defn maze-grid-width
  "Determines the width of a maze grid for the given maze."
  [maze]
  (maze-grid-dimension maze :width))
  
(defn wall-grid-coord
  "Determines where the given wall would show up in a grid for the given coordinate.
   'coord' should be either :x or :y"
  [wall coord]
  (let [adjuster (if (wall-adjusts-coord wall coord)
                     2   ;adjust for both the side wall and the fact that the wall adjusts
                     1)] ;only adjust for the side wall in the grid
    (+ adjuster (* 2 (coord wall)))))

(declare maze-to-grid-internal)
(defn maze-to-grid
  "Turns a maze into a MazeGrid."
  [maze]
  (->MazeGrid (maze-to-grid-internal maze)
              (maze-grid-width maze)
              (maze-grid-height maze)))

(declare add-side-walls add-internal-walls add-wall-to-grid)
(defn- maze-to-grid-internal
  "Does the actual vector creation for maze-to-grid."
  [full-maze]
  (let [height (maze-grid-height full-maze)
        width (maze-grid-width full-maze)
        full-open (into []
                        (repeat width (into [] 
                                            (repeat height 'open))))
           with-sides (add-side-walls full-open width height)
           with-internal (add-internal-walls with-sides width height)]
    (loop [grid with-internal
           maze (:walls full-maze)]
      (if (empty? maze)
        grid
        (recur (add-wall-to-grid (first maze) grid)
               (rest maze))))))

(declare vertical-side-walls horizontal-side-walls set-grid-xy)
(defn- add-side-walls
  "Adds walls along all sides of a maze grid."
  [grid width height]
  (horizontal-side-walls (vertical-side-walls grid width height)
                         width
                         height))

(defn- vertical-side-walls
  "Sets all the vertical side walls for a grid"
  [open-grid width height]
  (loop [h-idx (- height 1)
         grid open-grid]
    (if (>= h-idx 0)
      (let [with-0-set (set-grid-xy grid 0 h-idx 'wall)
            with-end-set (set-grid-xy with-0-set (- width 1) h-idx 'wall)]
       (recur (dec h-idx)
              with-end-set))
      grid)))
            
(defn- horizontal-side-walls
  "Sets all the horizontal side walls for a grid"
  [open-grid width height]
  (loop [w-idx (- width 1)
         grid open-grid]
    (if (>= w-idx 0)
      (let [with-0-set (set-grid-xy grid w-idx 0 'wall)
            with-end-set (set-grid-xy with-0-set w-idx (- height 1) 'wall)]
        (recur (dec w-idx)
               with-end-set))
      grid)))

(defn- add-internal-walls
  "Adds walls at all the points where both coordinates are even.  These
  points are needed as they are where there are possible lines of walls in the grid."
  [no-internal-grid width height]
  (let [loc-tuples (for [x (range width)
                         y (range height)
                         :when (and (even? x) (even? y))]
                     [x y])]
    (loop [grid no-internal-grid
           locations loc-tuples]
      (if (seq locations)
        (let [loc (first locations)
              x (nth loc 0)
              y (nth loc 1)]
          (recur (set-grid-xy grid x y 'wall)
                 (rest locations)))
        grid))))

(defn- add-wall-to-grid
  "Adds a wall into a grid.  Just sets the correct index to 'wall rather than 'open"
  [wall grid]
  (let [x (wall-grid-coord wall :x)
        y (wall-grid-coord wall :y)]
    (set-grid-xy grid x y 'wall)))

(defn- set-grid-xy
  "Sets a value in a 2d maze grid"
  [grid x y value]
  (let [y-vec (nth grid x)]
    (assoc grid x (assoc y-vec y value))))
      
      