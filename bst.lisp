(defpackage :bst (:use :common-lisp)
		 (:export :insert :remove :print :ninsert :delete :member :find :size))

(in-package :bst)

(defstruct (bst (:print-function bst:print))
  left
  node
  right
  (count 1 :type fixnum))

 ; Key is probably just an accessor function if any

(defun insert (node bst &key (test #'<) (key #'identity))
  (if (null bst)
    (make-bst :node node)
    (let ((value (funcall key (bst-node bst))))
      (cond
	((funcall test (funcall key node) value)
	 (make-bst :node (bst-node bst)
		   :right (bst-right bst)
		   :left (bst:insert node (bst-left bst) :test test :key key)))
	((funcall test value (funcall key node))
	 (make-bst :node (bst-node bst)
		   :left (bst-left bst)
		   :right (bst:insert node (bst-right bst) :test test :key key)))
	(t 
	  (make-bst :left (bst-left bst)
		    :right (bst-right bst)
		    :node (bst-node bst)
		    :count (+ 1 (bst-count bst))))))))

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
  (defun remove (node bst &key (test #'<) (key #'identity))
    (if (null bst)
      nil
      (let ((value (funcall key (bst-node bst))))
        (cond
	  ((funcall test node value)
	   (make-bst :node (bst-node bst)
		     :right (bst-right bst)
		     :left (bst:remove node (bst-left bst) :test test :key key)))
	  ((funcall test value node)
	   (make-bst :node (bst-node bst)
		     :left (bst-left bst)
		     :right (bst:remove node (bst-right bst) :test test :key key)))
	  (t (if (= (bst-count bst) 1)
	       (percolate (bst-left bst)
			  (bst-right bst))
	       (make-bst :left (bst-left bst)
			 :right (bst-right bst)
			 :node (bst-node bst)
			 :count (- (bst-count bst) 1)))))))))

(defun print (bst stream depth)
  (format stream "#<BST ~A>" (bst:size bst)))

(defun ninsert (node bst &key (test #'<) (key #'identity))
  (if (null bst)
    (make-bst :node node)
    (let ((value (funcall key (bst-node bst))))
      (cond
	((funcall test (funcall key node) value)
	 (setf (bst-left bst)
	       (bst:ninsert node (bst-left bst) test key)))
	((funcall test value (funcall key node))
	 (setf (bst-right bst)
	       (bst:ninsert node (bst-right bst) test key)))
	(t
	  (incf (bst-count bst))
	  bst)))))

(labels ((percolate (left right)
	      (if (zerop (random 2))
		(if (null left)
		  right
		  (setf (bst-left left)
			(percolate (bst-left left)
				   (bst-right left))
			(bst-right left) right))
		(if (null right)
		  left
		  (setf (bst-right right)
			(percolate (bst-left right)
				   (bst-right right))
			(bst-left right) left)))))
  (defun delete (node bst &key (test #'<) (key #'identity))
    (if (null bst)
      nil
      (let ((value (funcall key (bst-node bst))))
        (cond
	  ((funcall test node value)
	   (setf (bst-left bst)
	         (bst:delete node (bst-left bst) test key)))
	  ((funcall test value node)
	   (setf (bst-right bst)
	         (bst:delete node (bst-right bst) test key)))
	  (t
	    (if (= 1 (bst-count bst))
	      (percolate (bst-left bst)
			 (bst-right bst))
	      (progn
		(decf (bst-count bst))
		bst))))))))

(defun member (node bst &key (test #'<) (key #'identity))
  (if (null bst)
    nil
    (let ((value (funcall key (bst-node bst))))
      (cond
	((funcall test node value)
	 (bst:member node (bst-left bst) :test test :key key))
	((funcall test value node)
	 (bst:member node (bst-right bst) :test test :key key))
	(t
	  bst)))))

(defun find (node bst &key (test #'<) (key #'identity))
  (bst:node (bst:member node bst test key)))

(defun size (bst)
  (if (null bst)
    0
    (+ (bst-count bst)
       (bst:size (bst-left bst))
       (bst:size (bst-right bst)))))
