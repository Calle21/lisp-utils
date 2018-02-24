
 ; Took me two hours

(defun whitespace? (ch)
  (or (char= ch #\space)
      (char= ch #\tab)
      (char= ch #\newline)))

(defun next-white (str p)
  (position-if #'whitespace? str :start p))

(defun next-word (str p)
  (position-if #'not str :key #'whitespace?
			 :start p))

(defun get-word (str p) ; p should not be a whitespace
  (subseq str p (next-white str p)))

(defun nthword (str n &key (start 0))
  (if (null start)
      nil
      (progn
	(setf start (next-word str start))
	(if (null start)
	  nil
	  (if (<= n 1) ; less or equal to avoid infinite loop (if someone gives 0)
	    (get-word str start)
	    (nthword str (1- n) :start (next-white str start)))))))

 ; There are several exit points, so this approach was better than using a do

