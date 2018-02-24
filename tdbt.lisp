
 ; Top down binary trees

(defstruct (tdbt (:conc-name node-)
		 (:print-function print-tdbt))
  r v l)

(defun print-tdbt (tdbt stream depth)
  (progn
    (format stream "<")
    (tdbt-print tdbt stream t)
    (format stream ">")))

(defun tdbt-print (tdbt stream &optional first)
  (if tdbt
    (progn
      (when (not first)
	(format stream " "))
      (format stream "~A" (node-v tdbt))
      (tdbt-print (node-l tdbt) stream)
      (tdbt-print (node-r tdbt) stream))))

(defun tdbt-insf (tdbt &rest elts)
  (if (null elts)
    tdbt
    (apply #'tdbt-insf (cons (tdbt-insert (car elts) tdbt) (cdr elts)))))

(defun tdbt-insert (elt tdbt &optional (depth 1))
  (if (or (null tdbt)
	  (= depth 6)) ; Holds about 60 elements when saturated
    (make-tdbt :v elt)
    (if (zerop (random 2))
      (make-tdbt :v elt
		 :r (tdbt-insert (node-v tdbt) (node-r tdbt) (+ depth 1))
		 :l (node-l tdbt))
      (make-tdbt :v elt
		 :l (tdbt-insert (node-v tdbt) (node-l tdbt) (+ depth 1))
		 :r (node-r tdbt)))))

(defun tdbt-find (v tdbt &key (test #'eql))
  (bfs-tdbt v (list tdbt) test))

(defun bfs-tdbt (v queue test)
  (if (null queue)
    nil
    (let ((tdbt (car queue)))
      (if (null tdbt)
	(bfs-tdbt v (cdr queue) test)
	(if (funcall test v (node-v tdbt))
	  tdbt
	  (bfs-tdbt v (append (cdr queue) (list (node-l tdbt) (node-r tdbt))) test))))))
 
(defun tdbt-size (tdbt)
  (if (null tdbt)
    0
    (+ 1 (tdbt-size (node-l tdbt)) (tdbt-size (node-r tdbt)))))

