(ns life.utils)


;;; Collision detection - Rectangle-2D
(defn in-quad?
  "returns true when x and y are within the quad of 
   whose top-left corner is (qx, qy) and dimensions are qh qw"
  [x y qx qy qw qh]
  (and (> x qx) (< x (+ qx qw))
       (> y qy) (< y (+ qy qh))))


;;; Computing Matrix coordinates 
;;
(defn n-iterate
  "Returns a (length n) sequence of x, (f x), (f (f x)), etc"
  [n f x]
  (take n (iterate f x)))


(defn coords [n f x]
  (n-iterate n f x))
		    

(let [x-coords (ref [])
      y-coords (ref [])]

  (defn reset-coords []
    (dosync (ref-set y-coords [])
	    (ref-set x-coords [])))
  
  (defn row-coords
    "returns a seq of the y-coordinates of each row in an m-row matrix,
   given an initial starting value y, and a function f, which is called iteratively on y.
   result:  [ y , (f y) , (f (f y)) , (f (f (f y))) , etc. ] 

   hint: y = *border-width*
         f = #(+ % *button-height* *row-offset*) "
    [m f y]
    (if (empty? @y-coords)
      (dosync (ref-set y-coords (vector (coords m f y))))
      @y-coords))


  (defn col-coords
    "returns a seq of the x-coordinates of each row in an n-column matrix,
   given an initial starting value x, and a function f, which is called iteratively on x.
   result:  [ x , (f x) , (f (f x)) , (f (f (f x))) , ..etc. ] 

   hint: x = *border-width*
         f = #(+ % *button-width* *col-offset*)"
    [m f x]
    (if (empty? @x-coords)
      (dosync (ref-set x-coords (vector (coords m f x))))
      @x-coords)))



