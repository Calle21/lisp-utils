(defstruct (bst (:print-function bst-print))
  left
  node
  right)

(defun bst-insert (node bst &key (test #'<))
  (cond
    ((null bst)
     (make-bst :node node))
    ((funcall test node (bst-node bst))
     (make-bst :node (bst-node bst)
	       :right (bst-right bst)
	       :left (bst-insert node (bst-left bst) :test test)))
    ((funcall test (bst-node bst) node)
     (make-bst :node (bst-node bst)
	       :left (bst-left bst)
	       :right (bst-insert node (bst-right bst) :test test)))
    (t bst)))

(labels ((percolate (left right)
             (if (zerop (random 2))
	       (if (null left)
	         right
		 (make-bst :node (bst-node left)
			   :right right
			   :left (percolate (bst-left left)
					    (bst-right left))))
	       (if (null right)
		 left
		 (make-bst :node (bst-node right)
			   :left left
			   :right (percolate (bst-left right)
					     (bst-right right)))))))
  (defun bst-remove (node bst &key (test #'<))
    (cond
      ((null bst)
       nil)
      ((funcall test node (bst-node bst))
       (make-bst :node (bst-node bst)
	         :right (bst-right bst)
	         :left (bst-remove node (bst-left bst) :test test)))
      ((funcall test (bst-node bst) node)
       (make-bst :node (bst-node bst)
	         :left (bst-left bst)
	         :right (bst-remove node (bst-right bst) :test test)))
      (t (percolate (bst-left bst)
		    (bst-right bst))))))

(defun bst-print () )

(defun nbst-insert () )

(defun bst-delete () )

(defun bst-search () )
