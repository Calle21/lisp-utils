(defmacro for ((var start stop) &body body) ; On Lisp
  (let ((gstop (gensym)))
   `(do ((,var ,start (+ ,var 1))
         (,gstop ,stop))
       ((< ,gstop ,var))
      ,@body)))

