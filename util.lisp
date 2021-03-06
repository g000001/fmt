(cl:in-package :fmt.internal)

(defun butlastatom (list)
  `(,@(butlast list)
      ,(car (last list))))

(defmacro let-optionals* (args (&rest binds) &body body)
  (let ((rest (if (tailp '() binds)
                  '()
                  `(&rest ,(cdr (last binds))) )))
    (cl:loop
       :for (var . val+pred) :in (mapcar (lambda (x)
                                           (if (consp x)
                                               x
                                               (list x) ))
                                         (if rest
                                             (butlastatom binds)
                                             binds) )
       :for k :from 0
       :collect (if (cdr val+pred)
                    `(,var (or (and ,(nth 1 val+pred) ,var) ,(nth 0 val+pred)))
                    `(,var (or (nth ,k ,args) ,(nth 0 val+pred))) )
       :into var+default
       :collect var :into new-binds
       :finally (return
                  `(destructuring-bind (&optional ,@new-binds ,@rest)
                                       ,args
                     (declare (ignorable ,@new-binds))
                     (let* (,@var+default)
                       ,@body ))))))

(defun make-eq?-table ()
  (make-hash-table #'eq?))


(define-function (call-with-output-string proc)
  (let ((out (open-output-string)))
    (funcall proc out)
    (get-output-string out)))

(defun read-line (port)
  (cl:read-line port nil (rnrs:eof-object) nil))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))


;;; eof
