(ns life.core
  (:use [rosado.processing]
        [rosado.processing.applet]
	[life.utils]
	[life.cells])
  (:require ;; [overtone.live :as tone]
  	    [clojure.contrib.combinatorics :as combo]
	    [clojure.contrib.math :as math])
  (:gen-class))


(def *framerate*        90)
(def *screen-width*    600)
(def *screen-height*   600)
(def *grid-width*      100)
(def *grid-height*     100)
(def *cell-width*        6)
(def *generation* (atom 0))


;;;; The Grid
(def *grid* (ref []))

(defn cell
  ([idx]
     (cell idx @*grid*))
  ([idx vec]
     (nth vec idx "not-found!")))

(defn make-grid [w h]
  (vec (doall (map make-cell (range (* w h))))))

(defn init-grid []
  (dosync (ref-set *grid* (make-grid *grid-width* *grid-height*)))
  (reset! *generation* 0))

(defn clear-grid []
  (dosync (alter *grid* (fn [g] (vec (doall (map kill-cell g))))))
  (println ";;;;;;;;; Grid Cleared! ;;;;;;;;;")
  (reset! *generation* 0))

(defn randomize-grid []
  (dosync (alter *grid* (fn [g] (vec (doall (map random-cell g))))))
  (reset! *generation* 0))





;;;; Calculating neighbor's indicies
(let [w *grid-width*
      h *grid-width*]

  (defn up [i]
    (if (< i w)
      (+ i (* (dec h) w))
      (- i w)))

  (defn dn [i]
    (if (> i (* (dec h) w))
      (- i (* h w))
      (+ i w)))

  (defn lt [i]
    (if (zero? (mod i w))
      (+ i (dec w))
      (dec i)))

  (defn rt [i]
    (if (zero? (mod (inc i) w))
      (- i (dec w))
      (inc i)))

  (defn up-lt [i] (lt (up i)))
  (defn up-rt [i] (rt (up i)))
  (defn dn-lt [i] (lt (dn i)))
  (defn dn-rt [i] (rt (dn i)))


  (defn neighbors* [idx vec w h]
    (doall (map #(cell (% idx))
		[#'up-lt #'up #'up-rt #'lt #'rt #'dn-lt #'dn #'dn-rt])))

  (defn neighbors [idx]
    (vec (neighbors* idx @*grid* w h))))

(defn live-neighbors [idx]
  (filter :alive (neighbors idx)))

(defn num-live-neighbors [idx]
  (count (live-neighbors idx)))

(defn evolve-cell
  [cell]
  (let [neighbors (num-live-neighbors (:idx cell))]
    (or (and (:alive cell)
	     (cond (< neighbors 2) (kill-cell cell) ; death by under-population
		   (> neighbors 3) (kill-cell cell) ; death by over-crowding
		   :else           cell)) ; live to die another day
	(if (= neighbors 3)
	  (birth-cell cell)
	  cell))))

(defn evolve [vec]
  (doall (map evolve-cell vec)))

(def *mode* (atom :step))

(defn switch-mode
  ([]
   (reset! *mode*
	   (case @*mode*
		 :step :run
		 :run :step))
   ;; (println (str "mode switched to: " @*mode*))
   )
  
  ([new]
   (reset! *mode* new)
   ;; (println (str "mode switched to: " @*mode*))
   ))






(defn cartesian-product
  "rows and cols are seqs of y and x values (respectively)
   used to lay out the matrix on a cartesian plane."
  [rows cols]
  (combo/cartesian-product rows cols))

(def *indicies*
     (ref (vec (range (* *grid-width* *grid-height*)))))

(def *x-coords*
     (ref (vec (take *grid-width* (iterate #(+ % *cell-width*) 0)))))

(def *y-coords*
     (ref (vec (take *grid-width* (iterate #(+ % *cell-width*) 0)))))

(def *xy-coords*
     (ref (vec (cartesian-product @*x-coords* @*y-coords*))))


(defn step []
  (dosync (ref-set *grid* (vec (evolve @*grid*))))
  (swap! *generation* inc))

(def *pause-life* (atom true))
(defn pause []  (swap! *pause-life* not))
(defn paused? [] @*pause-life*)

(defn update []
  (when (= @*mode* :run) (step)))


;;; Drawing functions
(defn draw-cell [id pos]
  (let [cell (cell id)
	colr (if (:alive cell) 255 1)
	width *cell-width*
	[x y] pos]
    (with-translation [x y]
      (fill-float colr)
      (rect 0 0 width width))))

(defn draw-grid
  ([]
   (draw-grid *grid-width* *grid-height* *cell-width* *cell-width*))
  ([w h cell-w cell-h]
   (dorun (map draw-cell (range (* w h)) @*xy-coords*))))

(defn draw []
  (draw-grid)
  (update))

(defn setup
  "executes once."
  []
  (println ";;;;;;;;; LIFE! ;;;;;;;;;")
  (smooth)
  (no-stroke)
  (rect-mode CORNER)
  (background-float 200 200 255)
  (framerate *framerate*)
  (reset-coords)
  (init-grid))



(def *marking*       (ref false))
(def *erasing*       (ref false))
(def *mouse-button* (atom false))


(defn cell-idx-at-point
  "TODO: bounds checking"
  [x y]
  (let [row (Math/floor (/ x *cell-width*))
	col (Math/floor (/ y *cell-width*))]
    (if (and (< 0 x) (< x *screen-width*)
	     (< 0 y) (< y *screen-height*))
      (int (+ col (* row *grid-width*))))))

(defn cell-at-point
  ([x y]
   (cell-at-point x y @*grid*))
  ([x y v]
   (let [idx (cell-idx-at-point x y)
	 cnt (count v)]
     (and idx
	  (<  -1 idx)
	  (< idx cnt)
	  (nth v idx)))))


(defn flip-cell [cell erasing? marking?]
  (and (or erasing? marking?)
       (dosync
	(alter *grid* (fn [g]
			(assoc g (:idx cell)
			       (or (and erasing? (or (and (:alive cell) (kill-cell cell))
						     cell))
				   (and marking? (or (and (:alive cell) cell)
						     (birth-cell cell))))))))))

;;;; User Interaction
(defn mouse-pressed [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*
	cell (cell-at-point x y)]
    (dosync
     (ref-set *erasing* (:alive cell))
     (ref-set *marking* (not (:alive cell))))
    (reset! *mouse-button* true)
    ;; (println (str "\nmouse-pressed!"
    ;; 		  " button: " @*mouse-button*
    ;; 		  " cell: " cell
    ;; 		  " erasing: " @*erasing*
    ;; 		  " marking: " @*marking*))
    (flip-cell cell @*erasing* @*marking*)))

(defn mouse-released [evt]
  (let [x (.getX evt)
	y (.getY evt)
	cell (cell-at-point x y)]
    (dosync
     (ref-set *erasing* false)
     (ref-set *marking* false))
    (reset! *mouse-button* false)))

(defn mouse-moved [evt]
  (let [x (.getX evt)
	y (.getY evt)
	cell (cell-at-point x y)
	button @*mouse-button*]
    ;; (println (str "\nmouse-moved!"
    ;; 		  " button: " button))
    (and x y cell button (flip-cell cell @*erasing* @*marking*))))

(defn mouse-dragged [evt]
  (let [x (.getX evt)
	y (.getY evt)
	cell (cell-at-point x y)
	button @*mouse-button*]
        ;; (println (str "\nmouse-dragged!"
	;; 	  " button: " button
	;; 	  " erasing: " @*erasing*
	;; 	  " marking: " @*marking*
	;; 	  " point: " [x y]))
    (and x y cell button (flip-cell cell @*erasing* @*marking*))))


(defn key-pressed [evt]
  (let [char (.getKeyChar evt)]
    (case char
	  (\c \C) (clear-grid)
	  
	  (\g \G) (switch-mode)

	  (\r \R) (randomize-grid)

	  \space  (if (= @*mode* :run)
		      (switch-mode :step)
		      (step))
	  :unrecognized-key-command)))

(defapplet life
  :title "Caspary's Conway's Game of Life!"
  :setup setup
  :draw draw
  :size [*screen-width* *screen-height*]
  :mouse-moved mouse-moved
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :mouse-dragged mouse-dragged
  :key-pressed key-pressed)

;; (run life :interactive)
;; (stop life)


 (defn -main [& args]
   (run life))


