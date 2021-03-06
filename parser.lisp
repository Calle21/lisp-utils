
 ;; Lisp parser that allows infix read-macros by enabling access to last read object

(defvar *readttable* nil)

(defun readmacro (c)
  (declare (character c))
  (declare (special *readttable*))
  (cdr (find c *readttable* :key #'car :test #'char=)))

(defun read-file (path-string)
  (with-open-file (*in* (make-pathname path-string) :direction :input)
    (declare (special *in*))
    (read-delimited-llist nil)))

(defmacro defreadmacro (c &rest body)
 `(push (cons ,c #'(lambda () ,@body))
        *readttable*))

(defun read-delimited-llist (stop)
  (do (*last* *rest*
       (c (peek-char t *in* nil)
          (peek-char t *in* nil))
       (*none* t nil))
     ((eql c stop) (if *none*
                       nil
                       (nreverse (cons *last* *rest*))))
    (declare (special *last* *rest* *none*))
    (if (null c)
      (error "Reached end of file within structure")
      (multiple-value-bind (obj any?) (read-object)
        (if any?
          (update-last obj))))))

(defun read-object ()
  (let ((c (peek-char t *in* nil)))
    (if (null c)
      (values nil nil)
      (let ((fn (readmacro c)))
        (if fn
          (funcall fn)
          (read-token))))))

(defmacro update-last (obj)
  `(setf *rest* (cons *last* *rest*)
         *last* ,obj))

(defun read-new-object ()
  (let ((*none* t))
    (declare (special *none*))
    (multiple-value-bind (obj any?) (read-object)
      (if any?
        obj
        (error "Expected new object")))))

(defreadmacro #\:
  (declare (special *none* *last*))
  (values
    (cond
      (*none* (error "Can't concatenate unexisting object"))
      ((listp *last*)
       (setq *last* (nconc *last* (list (read-new-object)))))
      (t
        (setq *last* (list *last* (read-new-object)))))
    nil))

(defreadmacro #\( (read-delimited-llist #\)))

(defun invisible (c)
  (or (char= c #\space)
      (char= c #\tab)
      (char= c #\newline)))

(let ((buffer (make-string 100)))
  (defun read-token ()
    (let ((token (do ((c (read-char *in* nil)
                         (read-char *in* nil))
                      (i 0 (1+ i)))
                    ((or (null c)
                           (invisible c)
                           (assoc c *readttable*))
                       (subseq buffer 0 i))
                   (setf (aref buffer i) c))))
      (if (every #'digit-char-p token)
        (parse-integer token)
        (intern (nstring-upcase token))))))
