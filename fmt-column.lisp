(cl:in-package :fmt.internal)
;;;; fmt-block.scm -- columnar formatting
;;
;; Copyright (c) 2006-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Columnar formatting
;;
;; A line-oriented formatter.  Takes a list of
;;   (line-fmt1 gen-fmt1 line-fmt2 gen-fmt2 ...)
;; and formats each of the gen-fmt1 formats as columns, printed
;; side-by-side, each line allowing post-processing done by line-fmt1
;; (just use dsp if you want to display the lines verbatim).

;; Continuations come to the rescue to make this work properly,
;; letting us weave the output between different columns without
;; needing to build up intermediate strings.

(define-function (fmt-columns . ls)
  (lambda (orig-st)
    (block nil
      (with-local-define-function
          (define-function (infinite? x)
            (and (pair? x) (pair? (cdr x)) (pair? (cddr x)) (caddr x)))
          :in
          (let ((q1 '())
                (q2 '())
                (remaining (length (srfi-1:remove #'infinite? ls))))
            (let ((line-buf '())
                  (line-non-empty? 'NIL))
              (with-local-define-function
                (define-function (enq! proc) (set! q2 (cons proc q2)))
                (define-function (deq!)
                  (let ((proc (car q1))) (set! q1 (cdr q1)) proc))
                (define-function (line-init!)
                  (set! q1 (reverse q2)) (set! q2 '()))
                (define-function (line-done?) (null? q1))
                (define-function (write-column fmt str finite?)
                  (set! line-buf (cons (cons fmt str) line-buf))
                  (if finite? (set! line-non-empty? 'T)))
                (define-function (*write-line)
                  (cond
                    (line-non-empty?
                     (for-each
                      (lambda (x) (set! orig-st
                                        (funcall (funcall (car x) (cdr x))
                                                 orig-st)))
                      (reverse line-buf))
                     (set! orig-st (nl orig-st))))
                  (set! line-buf '())
                  (set! line-non-empty? 'NIL)
                  (line-init!))
                (define-function (next cont)
                  (enq! cont)
                  (cond
                    ((line-done?)
                     (*write-line)
                     (if (not (positive? remaining))
                         (finish)
                         (funcall(deq!) 'NIL)))
                    (:else (funcall (deq!) 'NIL))))
                (define-function (finish)
                  (*write-line)
                  (return orig-st))
                (define-function (make-empty-col fmt)
                  (labels ((blank (cl:&rest .ignored.)
                             (declare (ignore .ignored.))
                             (write-column fmt "" 'NIL)
                             (next #'blank))); infinite loop, next terminates for us
                    #'blank))
                (define-function (make-col st fmt gen finite?)
                  (let ((acc '()))            ; buffer incomplete lines
                    (lambda (&rest .ignored.)
                      (declare (ignore .ignored.))
                      (with-local-define-function
                        (define-function (output* str st)
                          (let lp ((i 0))
                               (let ((nli (string-index str #\newline i)))
                                 (cond
                                   (nli
                                    (let ((line
                                           (string-concatenate-reverse
                                            (cons (substring/shared str i nli)
                                                  acc))))
                                      (set! acc '())
                                      (write-column fmt line finite?)
                                      ;; (call-with-current-continuation next)
                                      (next (lambda (&rest .ignore.)
                                              (declare (ignore .ignore.))
                                              (lp (+ nli 1))))))
                                   (:else
                                    (set! acc
                                          (cons (substring/shared str i) acc))))))
                        ;; update - don't output or the string port will fill up
                        (fmt-update str st))
                        :in
                        ;; gen threads through it's own state, ignore result
                        (funcall gen (fmt-set-writer! (copy-fmt-state st)
                                                      #'output*))
                        ;; reduce # of remaining finite columns
                        (set! remaining (- remaining 1))
                        ;; write any remaining accumulated output
                        (if (pair? acc)
                            (let ((s (string-concatenate-reverse acc)))
                              (write-column fmt s (and finite? (not (equal? s ""))))))
                        ;; (maybe) loop with an empty column in place
                        (if (not (positive? remaining))
                            (finish)
                            (next (make-empty-col fmt)))))))
                :in
                ;; queue up the initial formatters
                (for-each
                 (lambda (col)
                   (let ((st (fmt-set-port! (copy-fmt-state orig-st)
                                            (open-output-string))))
                     (enq! (make-col st (car col) (dsp (cadr col))
                                     (not (infinite? col))))))
                 ls)
                (line-init!)
                ;; start
                (funcall (deq!) 'NIL) )))))))

(define-function (columnar . ls)
  (with-local-define-function
    (define-function (proportional-width? w)
      (and (number? w)
           (or (< 0 w 1)
               (and (inexact? w) (= w 1.0)) )))
    (define-function (whitespace-pad? st)
      (char-whitespace? (or (fmt-pad-char st) #\space)) )
    (define-function (build-column ls)
      (let-optionals* ls ((fixed-width 'NIL)
                          (width 'NIL)
                          (last? 'NIL)
                          (tail '())
                          (gen 'NIL)
                          (prefix '())
                          (align 'left)
                          (infinite? 'NIL) )
        (with-local-define-function
          (define-function (scale-width st)
            (max 1 (inexact->exact
                    (truncate (* width (- (fmt-width st) fixed-width))) )))
          (define-function (padder)
            (if (proportional-width? width)
                (cl:case align
                  ((right)
                   (lambda (str) (lambda (st) (funcall (pad/left (scale-width st) str) st))) )
                  ((center)
                   (lambda (str) (lambda (st) (funcall (pad/both (scale-width st) str) st))) )
                  (cl:otherwise
                   (lambda (str) (lambda (st) (funcall (pad/right (scale-width st) str) st))) ))
                (cl:case align
                  ((right) (lambda (str) (pad/left width str)))
                  ((center) (lambda (str) (pad/both width str)))
                  (cl:otherwise (lambda (str) (pad/right width str))) )))
          (define-function (affix x)
            (cond
              ((pair? tail)
               (lambda (str)
                 (cat (string-concatenate prefix)
                      (funcall x str)
                      (string-concatenate tail) )))
              ((pair? prefix)
               (lambda (str) (cat (string-concatenate prefix) (funcall x str))) )
              (:else x) ))
          :in
          (list
           ;; line formatter
           (affix
            (let ((pad (padder)))
              (if (and last? (not (pair? tail)) (eq? align 'left))
                  (lambda (str)
                    (lambda (st)
                      (funcall (funcall (if (whitespace-pad? st) #'dsp pad) str) st) ))
                  pad )))
           ;; generator
           (if (proportional-width? width)
               (lambda (st) (funcall (with-width (scale-width st) gen) st))
               (with-width width gen) )
           infinite? ))))
    (define-function (adjust-widths ls border-width)
      (let* ((fixed-ls
              (srfi-1:filter (lambda (x) (and (number? (car x)) (>= (car x) 1))) ls) )
             (fixed-total (srfi-1:fold #'+ border-width (map #'car fixed-ls)))
             (scaled-ls (srfi-1:filter (lambda (x) (proportional-width? (car x))) ls))
             (denom (- (length ls) (+ (length fixed-ls) (length scaled-ls))))
             (rest (if (zero? denom)
                       0
                       (exact->inexact
                        (/ (- 1 (srfi-1:fold  #'+ 0 (map #'car scaled-ls))) denom) ))))
        (if (negative? rest)
            (error "fractional widths must sum to less than 1"
                   (map #'car scaled-ls) ))
        (map
            (lambda (col)
              (cons fixed-total
                    (if (not (number? (car col))) (cons rest (cdr col)) col) ))
          ls )))
    (define-function (finish ls border-width)
      (apply #'fmt-columns
             (map #'build-column (adjust-widths (reverse ls) border-width)) ))
    :in
    (let lp ((ls ls) (strs '()) (align 'left) (infinite? 'NIL)
             (width 'NIL) (border-width 0) (res '()))
           (cond
             ((null? ls)
              (if (pair? strs)
                  (finish (cons (cons (caar res)
                                      (cons 'NIL (cons (append (reverse strs)
                                                               (caddar res) )
                                                       (cdddar res) )))
                                (cdr res) )
                          border-width )
                  (finish (cons (cons (caar res) (cons 'NIL (cddar res))) (cdr res))
                          border-width )))
             ((string? (car ls))
              (if (string-index (car ls) #\newline)
                  (error "column string literals can't contain newlines")
                  (lp (cdr ls) (cons (car ls) strs) align infinite?
                      width (+ border-width (string-length (car ls))) res)))
             ((number? (car ls))
              (print 
               (list strs align infinite? (car ls) border-width res))
              (lp (cdr ls) strs align infinite? (car ls) border-width res) )
             ((eq? (car ls) 'infinite)
              (lp (cdr ls) strs align 'NIL width border-width res) )
             ((symbol? (car ls))
              (lp (cdr ls) strs (car ls) infinite? width border-width res) )
             ((procedure? (car ls))
              (lp (cdr ls) '() 'left 'NIL 'NIL border-width
                  (cons (list width 'NIL '() (car ls) (reverse strs) align infinite?)
                        res )))
             (:else
              (error "invalid column" (car ls)) )))))

(define-function (max-line-width string-width str)
  (let lp ((i 0) (hi 0))
    (let ((j (string-index str #\newline i)))
      (if j
          (lp (+ j 1) (max hi (funcall string-width (subseq str i j))))
          (max hi (funcall string-width (subseq str i (string-length str))))))))

(define-function (pad-finite st proc width)
  (let* ((str (funcall (fmt-to-string proc) (copy-fmt-state st)))
         (w (max-line-width (or (fmt-string-width st) #'string-length) str)))
    (list (cat str)
          (if (and (integer? width) (exact? width))
              (max width w)
              w))))

(define-function (tabular . ls)
  (lambda (st)
    (let lp ((ls ls) (infinite? 'NIL) (width 'NIL) (res '()))
      (cond
       ((null? ls)
        (funcall (apply #'columnar (reverse res)) st))
       ((number? (car ls))
        (lp (cdr ls) infinite? (car ls) res))
       ((eq? 'infinite (car ls))
        (lp (cdr ls) 'NIL width (cons (car ls) res)))
       ((procedure? (car ls))
        (if infinite?
            (if width
                (lp (cdr ls) 'NIL 'NIL (cons (car ls) (cons width res)))
                (lp (cdr ls) 'NIL 'NIL (cons (car ls) res)))
            (let ((gen+width (pad-finite st (car ls) width)))
              (lp (cdr ls) 'NIL 'NIL (append gen+width res)))))
       (:else
        (lp (cdr ls) infinite? width (cons (car ls) res)))))))

;; break lines only, don't fmt-join short lines or justify
(define-function (fold-lines . ls)
  (lambda (st)
    (let ((output (fmt-writer st)))
      (with-local-define-function
        (define-function (kons-in-line str st)
          (let ((len (funcall (or (fmt-string-width st) #'string-length) str))
                (space (- (fmt-width st) (fmt-col st))) )
            (cond
              ((or (<= len space) (not (positive? space)))
               (funcall output str st) )
              (:else
               (kons-in-line
                (substring/shared str space len)
                (funcall output nl-str
                         (funcall output (substring/shared str 0 space) st) ))))))
        :in
        (funcall (fmt-let
                  'writer
                  (lambda (str st)
                    (let lp ((str str) (st st))
                         (let ((nli (string-index str #\newline)))
                           (cond
                             ((not nli)
                              (kons-in-line str st) )
                             (:else
                              (lp (substring/shared str (+ nli 1))
                                  (funcall output nl-str
                                           (kons-in-line
                                            (substring/shared str 0 nli)
                                            st ))))))))
                  (apply-cat ls) )
                 st )))))

(define-function (wrap-fold-words seq knil max-width get-width line . o)
  (let* ((last-line (if (pair? o) (car o) line))
         (vec (if (list? seq) (list->vector seq) seq))
         (len (vector-length vec))
         (len-1 (- len 1))
         (breaks (make-vector len 'NIL))
         (penalties (make-vector len 'NIL))
         (widths
          (list->vector
           (map get-width (if (list? seq) seq (vector->list vec))) )))
    (with-local-define-function
      (define-function (largest-fit i)
        (let lp ((j (+ i 1)) (width (vector-ref widths i)))
             (let ((width (+ width 1 (vector-ref widths j))))
               (cond
                 ((>= width max-width) (- j 1))
                 ((>= j len-1) len-1)
                 (:else (lp (+ j 1) width)) ))))
      (define-function (min-penalty! i)
        (cond
          ((>= i len-1) 0)
          ((vector-ref penalties i))
          (:else
           (vector-set! penalties i (expt (+ max-width 1) 3))
           (vector-set! breaks i i)
           (let ((k (largest-fit i)))
             (let lp ((j i) (width 0))
                  (if (<= j k)
                      (let* ((width (+ width (vector-ref widths j)))
                             (break-penalty
                              (+ (max 0 (expt (- max-width (+ width (- j i))) 3))
                                 (min-penalty! (+ j 1)) )))
                        (cond
                          ((< break-penalty (vector-ref penalties i))
                           (vector-set! breaks i j)
                           (vector-set! penalties i break-penalty) ))
                        (lp (+ j 1) width) ))))
           (if (>= (vector-ref breaks i) len-1)
               (vector-set! penalties i 0) )
           (vector-ref penalties i) )))
      (define-function (sub-list i j)
        (let lp ((i i) (res '()))
             (if (> i j)
                 (reverse res)
                 (lp (+ i 1) (cons (vector-ref vec i) res)) )))
      :in
      (cond
        ((zero? len)
         ;; degenerate case
         (funcall last-line '() knil) )
        (:else
         ;; compute optimum breaks
         (vector-set! breaks len-1 len-1)
         (vector-set! penalties len-1 0)
         (min-penalty! 0)
         ;; fold
         (let lp ((i 0) (acc knil))
              (let ((break (vector-ref breaks i)))
                (if (>= break len-1)
                    (funcall last-line (sub-list i len-1) acc)
                    (lp (+ break 1) (funcall line (sub-list i break) acc)) ))))))))

;; XXXX don't split, traverse the string manually and keep track of
;; sentence endings so we can insert two spaces
(define-function (wrap-fold str . o)
  (apply #'wrap-fold-words (string-tokenize str) o))

(define-function (wrap-lines . ls)
  (let ((buffer '()))
    (with-local-define-function
      (define-function (print-line ls st)
        (nl (funcall (fmt-join #'dsp ls " ") st)) )
      :in
      (lambda (st)
        (funcall (fmt-let
                  'writer
                  (lambda (str st) (set! buffer (cons str buffer)) st)
                  (apply-cat ls) )
                 st )
        (wrap-fold (string-concatenate-reverse buffer)
                   st (fmt-width st)
                   (or (fmt-string-width st) #'string-length)
                   #'print-line )))))

(define-function (justify . ls)
  (lambda (st)
    (let ((width (fmt-width st))
          (string-width (or (fmt-string-width st) #'string-length))
          (output (fmt-writer st))
          (buffer '()) )
      (with-local-define-function
        (define-function (justify-line ls st)
          (if (null? ls)
              (nl st)
              (let* ((sum (srfi-1:fold (lambda (s n) (+ n (funcall string-width s))) 0 ls))
                     (len (length ls))
                     (diff (max 0 (- width sum)))
                     (sep (make-string (if (= len 1)
                                           0
                                           (quotient diff (- len 1)) )
                                       #\space ))
                     (rem (if (= len 1)
                              diff
                              (remainder diff (- len 1)) )))
                (funcall output
                 (call-with-output-string
                  (lambda (p)
                    (display (car ls) p)
                    (let lp ((ls (cdr ls)) (i 1))
                         (cond
                           ((pair? ls)
                            (display sep p)
                            (if (<= i rem) (write-char #\space p))
                            (display (car ls) p)
                            (lp (cdr ls) (+ i 1)) )))
                    (newline p) ))
                 st ))))
        (define-function  (justify-last ls st)
          (nl (funcall (fmt-join #'dsp ls " ") st)) )
        :in
        (funcall (fmt-let
                  'writer
                  (lambda (str st) (set! buffer (cons str buffer)) st)
                  (apply-cat ls) )
                 st )
        (wrap-fold (string-concatenate-reverse buffer)
                   st width string-width #'justify-line #'justify-last)))))

(define-function (fmt-file path)
  (lambda (st)
    (with-open-file (p path)
      (let lp ((st st))
           (let ((line (read-line p)))
             (if (eof-object? line)
                 st
                 (lp (nl (funcall (dsp line) st)))))))))

(define-function (line-numbers . o)
  (let ((start (if (pair? o) (car o) 1)))
    (fmt-join/range #'dsp start 'NIL nl-str)))
