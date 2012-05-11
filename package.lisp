;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :fmt
  (:use)
  (:export
   :dsp :wrt :wrt/unshared :pretty :pretty/unshared :slashified :maybe-slashified
   :num :num/comma :num/si :num/fit :num/roman :num/old-roman :nl :fl :space-to
   :tab-to :fmt-null :cat :apply-cat :fmt-join :fmt-join/prefix :fmt-join/suffix
   :fmt-join/last :fmt-join/dot :pad :pad/left :pad/both :trim :trim/left :fmt
   :trim/both :trim/length :fit :fit/left :fit/both :fmt-let :fmt-bind :fmt-if
   :radix :fix :decimal-align :comma-char :decimal-char :pad-char :ellipse
   :with-width :columnar :tabular :fmt-columns :wrap-lines :justify :fmt-file
   :line-numbers :indent-space :switch-indent-space :newline-before-brace?
   :braceless-bodies? :non-spaced-ops? :no-wrap? :c-if :c-for :c-while :c-fun
   :c-prototype :c-var :c-begin :c-switch :c-case :c-case/fallthrough :c-default
   :c-label :c-goto :c-return :c-break :c-continue :c-const :c-static :c-volatile
   :c-restrict :c-register :c-auto :c-inline :c-extern :c-extern/c :c-cast
   :c-typedef :c-struct :c-union :c-class :c-attribute :c-enum :c-comment
   :cpp-include :cpp-define :cpp-if :cpp-ifdef :cpp-ifndef :cpp-elif :cpp-else
   :cpp-line :cpp-pragma :cpp-error :cpp-warning :cpp-stringify :cpp-sym-cat
   :cpp-wrap-header :js-expr :js-function :js-var :js-comment))

(defpackage :fmt.internal
  (:use :fmt :rnrs-user :fiveam :srfi-23 :srfi-69 :srfi-26)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-61 :cond)
  (:shadowing-import-from :srfi-6
                          :get-output-string
                          :open-output-string)
  (:shadowing-import-from :srfi-69 :hash-table-size :make-hash-table)
  (:shadowing-import-from :srfi-13
                          . #.(let ((ans '()))
                                (do-external-symbols (s :srfi-13 ans)
                                  (push s ans))))
  ;; (:shadow :assoc :lambda :loop)
  (:shadowing-import-from :rnrs :string-append))
