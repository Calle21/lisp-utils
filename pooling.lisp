
 ; More of an example program

(defconstant NDOTS 100)

(defvar *pool* (make-array NDOTS :fill-pointer t))

(dotimes (i NDOTS)
  (setf (aref *pool* i) (cons 0 0)))

(defvar *table* (make-array NDOTS :initial-element nil))

(defmacro get-dot ()
  '(vector-pop *pool*))

(defmacro return-dot (adot)
  `(vector-push ,adot *pool*))

(defun draw (x y color)
  (format t "Drew ~A;~A with color ~S~%" x y color))

(defun do-something ()
  (declare (special *width* *height*))
  (let ((i (random NDOTS)))
    (if (svref *table* i)
      (let ((adot (svref *table* i)))
        (setf (svref *table* i) nil)
        (draw (car adot) (cdr adot) 0)
        (return-dot adot))
      (let ((adot (get-dot)))
        (setf (svref *table* i) adot)
        (let ((x (random *width*))
              (y (random *height*)))
          (setf (car adot) x
                (cdr adot) y)
          (draw x y (random #.(expt 2 24)))))))) ; 8bit RGB? (read-time evaluation)

(let ((i 100))
  (defun activity? ()
    (minusp (decf i))))

(defun screen-saver ()
  (let ((*width* (get-screen-width))
        (*height* (get-screen-height)))
    (declare (special *width* *height*))
    (do ()
       ((activity?))
      (do-something))))

(defun get-screen-width () 100)
(defun get-screen-height () 100)
