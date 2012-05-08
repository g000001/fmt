(cl:in-package :fmt.internal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'eq?) #'eq)
  (setf (fdefinition 'odd?) #'oddp)
  (setf (fdefinition 'integer?) #'integerp)
  (setf (fdefinition 'list?) #'listp)
  (setf (fdefinition 'negative?) #'minusp)
  (setf (fdefinition 'null?) #'null)
  (setf (fdefinition 'pair?) #'consp)
  (setf (fdefinition 'positive?) #'plusp)
  (setf (fdefinition 'zero?) #'zerop)
  (setf (fdefinition 'vector-length) #'length)
  (setf (fdefinition 'vector?) #'vectorp)
  (setf (fdefinition 'procedure?) #'functionp)
  (setf (fdefinition 'exact?) #'rationalp)
  (setf (fdefinition 'inexact->exact) #'rationalize)
  (setf (fdefinition 'even?) #'evenp)
  (setf (fdefinition 'real?) #'realp)
  (setf (fdefinition 'newline) #'terpri)
  (setf (fdefinition 'display) #'princ)
  (setf (fdefinition 'remainder)  #'rem)
  (setf (fdefinition 'string-length)  #'length)
  (setf (fdefinition 'char->integer)  #'char-code)
  (setf (fdefinition 'string-ref) #'char)
  (setf (fdefinition 'symbol->string) #'string)
  (setf (fdefinition 'string?) #'stringp)
  (setf (fdefinition 'symbol?) #'stringp)
  (setf (fdefinition 'number?) #'numberp)
  (setf (fdefinition 'char?) #'characterp)
  (setf (fdefinition 'real-part) #'realpart)
  (setf (fdefinition 'imag-part) #'imagpart)
  (setf (fdefinition 'string=?) #'string=)
  (setf (fdefinition 'string-ci=?) #'string-equal)
  )

(defmacro set! (var val)
  `(setq ,var ,val))

(declaim (inline list-tail vector-set! list-ref vector->list list->vector
                 quotient set-car! set-cdr! eqv? equal?
                 assq assv assoc for-each))

(defun exact->inexact (exact)
  (float exact 0d0))

(defun boolean? (obj)
  (typep obj 'boolean))

(defun for-each (fn &rest lists)
  (apply #'mapc fn lists)
  nil)

(defun assq (item alist)
  (cl:assoc item alist :test #'eq?))

(defun assv (item alist)
  (cl:assoc item alist :test #'eqv?))

(defun assoc (item alist)
  (cl:assoc item alist :test #'equal?))

(defun eqv? (x y)
  (eql x y))

(defun equal? (x y)
  (equal x y))

(defun set-car! (list obj)
  (rplaca list obj))

(defun set-cdr! (cons x)
  (rplacd cons x))

(defun quotient (x y)
  (values (truncate x y)))

(defun list-tail (list k)
  (nthcdr k list))

(defun list-ref (list k)
  (nth k list))

(defun vector-set! (vec index val)
  (setf (aref vec index) val))

(defun vector->list (vec)
  (coerce vec 'list))

(defun list->vector (list)
  (coerce list 'vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (typecase list
      (list (if (tailp () list)
                list
              (cl:let ((last (last list)))
                `(,@(butlast list)
                  ,(car last)
                  cl:&rest
                  ,(cdr last)))))
      (symbol `(cl:&rest ,list)))))

(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))

(defmacro letrec ((&rest binds) &body body)
  `(let (,@(mapcar (cl:lambda (x)
                     `(,(car x) #'values))
             binds))
     (declare (optimize (space 3)))
     (labels (,@(remove nil
                  (mapcar (cl:lambda (x &aux (name (car x)))
                            `(,name
                               (&rest args)
                               (apply ,name args)))
                          binds)))
       (declare (optimize (debug 0) (space 3)))
       (psetq ,@(apply #'append binds))
       ,@body)))


(defmacro define-function (name-args &body body)
  (if (consp name-args)
      (destructuring-bind (name . args)
                          name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (setf (fdefinition ',name-args)
               ,(car body)))))

(declaim (inline vector-ref))
(defun vector-ref (vec k)
  (svref vec k))

(declaim (inline modulo))
(defun modulo (x y)
  (mod x y))

(defmacro begin (&body body)
  `(progn ,@body))

(declaim (inline make-vector))
(defun make-vector (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))

(declaim (inline string-append))
(defun string-append (&rest strings)
  (format nil "~{~A~}" strings))

(declaim (inline number->string))
(defun number->string (num)
  (write-to-string num))

(defmacro dolex ((&rest varlist) endlist &body body)
  (let* ((vars (mapcar (lambda (v)
                         (if (consp v) (car v) v) )
                       varlist ))
         (binds (mapcar (lambda (b)
                          (if (consp b)
                              (destructuring-bind (var &optional init next)
                                                  b
                                (if next
                                    `(,var ,init
                                           (let (,@(mapcar (lambda (x)
                                                             (list x x) )
                                                     vars ))
                                             (declare (ignorable ,@vars))
                                             ,next ))
                                    `(,var ,init)))
                              (list b nil) ))
                        varlist )))
    `(cl:do ,binds ,endlist ,@body) ))

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

(defun inexact? (n)
  (not (rationalp n)))

;;; eof
