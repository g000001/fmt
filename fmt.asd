;;;; fmt.asd

(cl:in-package :asdf)

(defsystem :fmt
  :serial t
  :depends-on (:fiveam
               :srfi-1
               :srfi-5
               :srfi-6
               :srfi-13
               :srfi-23
               :srfi-26
               :srfi-61
               :srfi-69)
  :components ((:file "package")
               (:file "util")
               (:file "decls")
               (:file "mantissa")
               (:file "fmt")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :fmt))))
  (load-system :fmt)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :fmt.internal :fmt))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
