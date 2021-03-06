(ns life.cells
  (:use [life.utils]
	[rosado.processing]))

;; (defn evolve [board]
;;   (iterate evolve* board))

;; (defn- evolve* [board]
;;   (pmap (fn [cell] (case (born-live-die? idx)
;; 			 :born (make-cell i :alive)
;; 			 :live cell
;; 			 :die  (kill-cell i :dead)))
;; 	board))

(defrecord cell [idx alive])

(defn make-cell
  ([idx] (cell. idx false))
  ([idx state] (cell. idx state)))

(defn birth-cell [cell]
  (cell. (:idx cell) true))

(defn kill-cell [cell]
  (cell. (:idx cell) false))

(defn random-cell [cell]
  (cell. (:idx cell)
	 (or (and (> (random 1) 0.5) true) false)))

(defn alive? [cell] (:alive cell))

(defn dead? [cell] (not (:alive cell)))