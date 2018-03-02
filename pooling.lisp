
 ; More of an example program

(defconstant *ndots* 100)

(defstruct (dot (:print-function print-dot))
  (x 0 :type 'fixnum)
  (y 0 :type 'fixnum)
  (color 0 :type 'fixnum))

(defun print-dot (dot stream depth)
  (declare (ignore dot depth)) ; ?
  (princ "#<Dot>" stream))

(defvar *pool* (make-array *ndots* :fill-pointer t))

(dotimes (i *ndots*)
  (setf (aref *pool* i) (make-dot)))

(defvar *table* (make-array *ndots* :initial-element nil))

(defun get-dot (i)
  (let ((adot (vector-pop *pool*)))
    (setf (dot-x adot)      (random *width*)
          (dot-y adot)      (random *height*)
          (dot-color adot)  (random #.(expt 2 24)) ; 8bit RGB? (read-time evaluation)
          (svref *table* i) adot)))

(defun return-dot (i)
  (vector-push (svref *table* i) *pool*)
  (setf (svref *table* i) nil))

(defun draw (i)
  (format t "Drew ~A~%" i))

(defun do-something ()
  (let ((i (random *ndots*)))
    (if (svref *table* i)
      (progn
        (return-dot i)
        (draw i))
      (progn
        (get-dot i)
        (draw i)))))

(defun screen-saver ()
  (let ((*width* (get-screen-width))
        (*height* (get-screen-height)))
    (declare (special *width* *height*))
    (do ()
       ((activity?))
      (do-something))))

(defun get-screen-width () 100)
(defun get-screen-height () 100)
(let ((i 100))
  (defun activity? ()
    (minusp (decf i))))
