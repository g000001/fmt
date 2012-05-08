(cl:in-package :fmt.internal)

(def-suite fmt)

(in-suite fmt)

(defmacro iss (x y)
  `(is (string= ,x ,y)))

(defun string-split (str delim)
  (ppcre:split delim str))

(defun char-whitespace? (c)
  (and (member c '(#\Space #\Newline #\Tab #\Newline #\Return #\Page))
       T))

;; basic data types

(test |basic data types|
  (iss "hi" (fmt nil "hi"))
  (iss "\"hi\"" (fmt nil (wrt "hi")))
  (iss "\"hi \\\"bob\\\"\"" (fmt nil (wrt "hi \"bob\"")))
  (iss "\"hello
world\"" (fmt nil (wrt "hello
world")))
  (iss "ABC" (fmt nil (upcase "abc")))
  (iss "abc" (fmt nil (downcase "ABC")))
  (iss "Abc" (fmt nil (titlecase "abc")))

  (iss "abc     def" (fmt nil "abc" (tab-to) "def"))
  (iss "abc  def" (fmt nil "abc" (tab-to 5) "def"))
  (iss "abcdef" (fmt nil "abc" (tab-to 3) "def"))

  (iss "-1" (fmt nil -1))
  (iss "0" (fmt nil 0))
  (iss "1" (fmt nil 1))
  (iss "10" (fmt nil 10))
  (iss "100" (fmt nil 100))
  (iss "-1" (fmt nil (num -1)))
  (iss "0" (fmt nil (num 0)))
  (iss "1" (fmt nil (num 1)))
  (iss "10" (fmt nil (num 10)))
  (iss "100" (fmt nil (num 100)))
  ;; (iss "1e+15" (fmt nil (num 1e+15)))
  (iss "1.e15" (fmt nil (num 1e+15)))
  ;; (iss "1e+23" (fmt nil (num 1e+23)))
  (iss "1.e23" (fmt nil (num 1e+23)))
  ;; (iss "1.2e+23" (fmt nil (num 1.2e+23)))
  (iss "1.2e23" (fmt nil (num 1.2e+23)))
  ;; (iss "1e-5" (fmt nil (num 1e-5)))
  (iss "1.e-5" (fmt nil (num 1e-5)))
  ;; (iss "1e-6" (fmt nil (num 1e-6)))
  (iss "1.e-6" (fmt nil (num 1e-6)))
  ;; (iss "1e-7" (fmt nil (num 1e-7)))
  (iss "1.e-7" (fmt nil (num 1e-7)))
  ;; (iss "2e-6" (fmt nil (num 2e-6)))
  (iss "57005" (fmt nil #xDEAD))
  (iss "#xDEAD" (fmt nil (radix 16 #xDEAD)))
  (iss "#xDEAD1234" (fmt nil (radix 16 #xDEAD) 1234))
  (iss "#xDE.AD" (fmt nil (radix 16 (exact->inexact (/ #xDEAD #x100)))))
  (iss "#xD.EAD" (fmt nil (radix 16 (exact->inexact (/ #xDEAD #x1000)))))
  (iss "#x0.DEAD" (fmt nil (radix 16 (exact->inexact (/ #xDEAD #x10000)))))
  (iss "1G" (fmt nil (radix 17 (num 33))))
  (iss "1G" (fmt nil (num 33 17)))

  (iss "3.14159" (fmt nil 3.14159))
  (iss "3.14" (fmt nil (fix 2 3.14159)))
  (iss "3.14" (fmt nil (fix 2 3.14)))
  (iss "3.00" (fmt nil (fix 2 3.)))
  (iss "1.10" (fmt nil (num 1.099 10 2)))
  (iss "0.00" (fmt nil (fix 2 1e-17)))
  (iss "0.0000000000" (fmt nil (fix 10 1e-17)))
  (iss "0.00000000000000001000" (fmt nil (fix 20 1e-17)))
  (signals (cl:error) (fmt nil (num 1e-17 0)))
  (iss "0.000004" (fmt nil (num 0.000004 10 6)))
  (iss "0.0000040" (fmt nil (num 0.000004 10 7)))
  (iss "0.00000400" (fmt nil (num 0.000004 10 8)))
  ;; (iss "0.000004" (fmt nil (num 0.000004)))
#||
  (iss "   3.14159" (fmt nil (decimal-align 5 (num 3.14159))))
  (iss "  31.4159" (fmt nil (decimal-align 5 (num 31.4159))))
  (iss " 314.159" (fmt nil (decimal-align 5 (num 314.159))))
  (iss "3141.59" (fmt nil (decimal-align 5 (num 3141.59))))
  (iss "31415.9" (fmt nil (decimal-align 5 (num 31415.9))))
  (iss "  -3.14159" (fmt nil (decimal-align 5 (num -3.14159))))
  (iss " -31.4159" (fmt nil (decimal-align 5 (num -31.4159))))
  (iss "-314.159" (fmt nil (decimal-align 5 (num -314.159))))
  (iss "-3141.59" (fmt nil (decimal-align 5 (num -3141.59))))
  (iss "-31415.9" (fmt nil (decimal-align 5 (num -31415.9))))
||#
  (cond
   ((exact? (/ 1 3)) ;; exact rationals
    (iss "333.333333333333333333333333333333" (fmt nil (fix 30 1000/3)))
    (iss  "33.333333333333333333333333333333" (fmt nil (fix 30 100/3)))
    (iss   "3.333333333333333333333333333333" (fmt nil (fix 30 10/3)))
    (iss   "0.333333333333333333333333333333" (fmt nil (fix 30 1/3)))
    (iss   "0.033333333333333333333333333333" (fmt nil (fix 30 1/30)))
    (iss   "0.003333333333333333333333333333" (fmt nil (fix 30 1/300)))
    (iss   "0.000333333333333333333333333333" (fmt nil (fix 30 1/3000)))
    (iss   "0.666666666666666666666666666667" (fmt nil (fix 30 2/3)))
    (iss   "0.090909090909090909090909090909" (fmt nil (fix 30 1/11)))
    (iss   "1.428571428571428571428571428571" (fmt nil (fix 30 10/7)))
    (iss "0.123456789012345678901234567890"
        (fmt nil (fix 30 (/  123456789012345678901234567890
                           1000000000000000000000000000000 ))))
    (iss  " 333.333333333333333333333333333333" (fmt nil (decimal-align 5 (fix 30 1000/3))))
    (iss  "  33.333333333333333333333333333333" (fmt nil (decimal-align 5 (fix 30 100/3))))
    (iss  "   3.333333333333333333333333333333" (fmt nil (decimal-align 5 (fix 30 10/3))))
    (iss  "   0.333333333333333333333333333333" (fmt nil (decimal-align 5 (fix 30 1/3))))
    ))

  (iss "11.75" (fmt nil (num (/ 47 4) 10 2)))
  (iss "-11.75" (fmt nil (num (/ -47 4) 10 2)))

  (iss "(#x11 #x22 #x33)" (fmt nil (radix 16 '(#x11 #x22 #x33))))
#||
  (iss "299,792,458" (fmt nil (num 299792458 10 nil nil T)))
  (iss "299,792,458" (fmt nil (num/comma 299792458)))
  (iss "299.792.458" (fmt nil (comma-char #\. (num/comma 299792458))))
  (iss "299.792.458,0" (fmt nil (comma-char #\. (num/comma 299792458.0))))
||#
  (iss "100,000" (fmt nil (num 100000 10 0 nil 3)))
  (iss "100,000.0" (fmt nil (num 100000 10 1 nil 3)))
  (iss "100,000.00" (fmt nil (num 100000 10 2 nil 3)))

  ;(iss "1.23" (fmt nil (fix 2 (num/fit 4 1.2345))))
  (iss "1.00" (fmt nil (fix 2 (num/fit 4 1))))
  (iss "#.##" (fmt nil (fix 2 (num/fit 4 12.345))))

  ;; (cond
  ;;  ((feature? 'full-numeric-tower)
  ;;   (iss "1+2i" (fmt nil (string->number "1+2i")))
  ;;   (iss "1+2i" (fmt nil (num (string->number "1+2i"))))
  ;;   (iss "1.00+2.00i" (fmt nil (fix 2 (num (string->number "1+2i")))))
  ;;   (iss "3.14+2.00i" (fmt nil (fix 2 (num (string->number "3.14159+2i")))))))

  ;(iss "3.9Ki" (fmt nil (num/si 3986)))
  (iss "4k" (fmt nil (num/si 3986 1000)))
  (iss "608" (fmt nil (num/si 608)))
  (iss "3G" (fmt nil (num/si 12345.12355 16))) )


(test |padding/trimming|
  (iss "abc  " (fmt nil (pad 5 "abc")))
  (iss "  abc" (fmt nil (pad/left 5 "abc")))
  (iss " abc " (fmt nil (pad/both 5 "abc")))
  (iss "abcde" (fmt nil (pad 5 "abcde")))
  (iss "abcdef" (fmt nil (pad 5 "abcdef")))

  (iss "abc" (fmt nil (trim 3 "abcde")))
  (iss "abc" (fmt nil (trim/length 3 "abcde")))
  (iss "abc" (fmt nil (trim/length 3 "abc\nde")))
  (iss "cde" (fmt nil (trim/left 3 "abcde")))
  (iss "bcd" (fmt nil (trim/both 3 "abcde")))

  (iss "prefix: abc" (fmt nil "prefix: " (trim 3 "abcde")))
  (iss "prefix: abc" (fmt nil "prefix: " (trim/length 3 "abcde")))
  (iss "prefix: abc" (fmt nil "prefix: " (trim/length 3 "abc\nde")))
  (iss "prefix: cde" (fmt nil "prefix: " (trim/left 3 "abcde")))
  (iss "prefix: bcd" (fmt nil "prefix: " (trim/both 3 "abcde")))

  (iss "abcde" (fmt nil (ellipses "..." (trim 5 "abcde"))))
  (iss "ab..." (fmt nil (ellipses "..." (trim 5 "abcdef"))))
  (iss "abc..." (fmt nil (ellipses "..." (trim 6 "abcdefg"))))
  (iss "abcde" (fmt nil (ellipses "..." (trim/left 5 "abcde"))))
  (iss "...ef" (fmt nil (ellipses "..." (trim/left 5 "abcdef"))))
  (iss "...efg" (fmt nil (ellipses "..." (trim/left 6 "abcdefg"))))
  (iss "abcdefg" (fmt nil (ellipses "..." (trim/both 7 "abcdefg"))))
  (iss "...d..." (fmt nil (ellipses "..." (trim/both 7 "abcdefgh"))))
  (iss "...e..." (fmt nil (ellipses "..." (trim/both 7 "abcdefghi"))))

  (iss "abc  " (fmt nil (fit 5 "abc")))
  (iss "  abc" (fmt nil (fit/left 5 "abc")))
  (iss " abc " (fmt nil (fit/both 5 "abc")))
  (iss "abcde" (fmt nil (fit 5 "abcde")))
  (iss "abcde" (fmt nil (fit/left 5 "abcde")))
  (iss "abcde" (fmt nil (fit/both 5 "abcde")))
  (iss "abcde" (fmt nil (fit 5 "abcdefgh")))
  (iss "defgh" (fmt nil (fit/left 5 "abcdefgh")))
  (iss "cdefg" (fmt nil (fit/both 5 "abcdefgh")))

  (iss "prefix: abc  " (fmt nil "prefix: " (fit 5 "abc")))
  (iss "prefix:   abc" (fmt nil "prefix: " (fit/left 5 "abc")))
  (iss "prefix:  abc " (fmt nil "prefix: " (fit/both 5 "abc")))
  (iss "prefix: abcde" (fmt nil "prefix: " (fit 5 "abcde")))
  (iss "prefix: abcde" (fmt nil "prefix: " (fit/left 5 "abcde")))
  (iss "prefix: abcde" (fmt nil "prefix: " (fit/both 5 "abcde")))
  (iss "prefix: abcde" (fmt nil "prefix: " (fit 5 "abcdefgh")))
  (iss "prefix: defgh" (fmt nil "prefix: " (fit/left 5 "abcdefgh")))
  (iss "prefix: cdefg" (fmt nil "prefix: " (fit/both 5 "abcdefgh")))

  (iss "abc
123
"
       (fmt nil (fmt-join/suffix (cut #'trim 3 <>)
                                 (string-split "abcdef
123456
" "
")
                                 #'nl)))
  )

(test |utilities|
  (iss "1 2 3" (fmt nil (fmt-join #'dsp '(1 2 3) " "))))


#|(test |shared structures|
  (iss "#0=(1 . #0#)"
       (fmt nil (wrt (let ((ones (list 1))) (set-cdr! ones ones) ones)))))|#

#||

;;


(test "(0 . #0=(1 . #0#))"
    (fmt nil (wrt (let ((ones (list 1)))
                   (set-cdr! ones ones)
                   (cons 0 ones)))))
(test "(sym . #0=(sym . #0#))"
    (fmt nil (wrt (let ((syms (list 'sym)))
                   (set-cdr! syms syms)
                   (cons 'sym syms)))))
(test "(#0=(1 . #0#) #1=(2 . #1#))"
    (fmt nil (wrt (let ((ones (list 1))
                       (twos (list 2)))
                   (set-cdr! ones ones)
                   (set-cdr! twos twos)
                   (list ones twos)))))
||#

#|(test |without shared detection|
  (iss "(1 1 1 1 1"
       (fmt nil (trim/length
                 10
                 (wrt/unshared
                  (let ((ones (list 1))) (set-cdr! ones ones) ones))))))|#

#||
;; without shared detection

 (test "(1 1 1 1 1 "
    (fmt nil (trim/length
             11
             (wrt/unshared
              (let ((ones (list 1))) (set-cdr! ones ones) ones)))))
||#

;;
#||
(defmacro test-pretty (str)
  (let ((sexp (read-from-string str)))
    `(test ,(gensym "test-pretty-")
       (iss ,str (fmt nil (pretty ',sexp))))))

#|(test-pretty "(foo bar)\n")|#

(test-pretty
"((self . aquanet-paper-1991)
 (type . paper)
 (title . \"Aquanet: a hypertext tool to hold your\"))
")

(test-pretty
"(abracadabra xylophone
             bananarama
             yellowstonepark
             cryptoanalysis
             zebramania
             delightful
             wubbleflubbery)\n")

(test-pretty
 "#(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
   25 26 27 28 29 30 31 32 33 34 35 36 37)\n")

(test-pretty
 "(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
  25 26 27 28 29 30 31 32 33 34 35 36 37)\n")

(test-pretty
 "(define (fold kons knil ls)
  (define (loop ls acc)
    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))
  (loop ls knil))\n")

(test-pretty
"(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))\n")

(test-pretty
"(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec)
  (vector-set! vec i 'supercalifrajalisticexpialidocious))\n")

(test-pretty
"(do ((my-vector (make-vector 5)) (index 0 (+ index 1)))
    ((= index 5) my-vector)
  (vector-set! my-vector index index))\n")

(test-pretty
 "(define (fold kons knil ls)
  (let loop ((ls ls) (acc knil))
    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc)))))\n")

(test-pretty
 "(define (file->sexp-list pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let loop ((res '()))
        (let ((line (read port)))
          (if (eof-object? line) (reverse res) (loop (cons line res))))))))\n")

(test "(let ((ones '#0=(1 . #0#))) ones)\n"
    (fmt nil (pretty (let ((ones (list 1))) (set-cdr! ones ones) `(let ((ones ',ones)) ones)))))

'(test
"(let ((zeros '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      (ones '#0=(1 . #0#)))
  (append zeros ones))\n"
    (fmt nil (pretty
             (let ((ones (list 1)))
               (set-cdr! ones ones)
               `(let ((zeros '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                      (ones ',ones))
                  (append zeros ones))))))

||#

(test |slashify|
  #|(iss "\"note\",\"very simple\",\"csv\",\"writer\",\"\"\"yay!\"\"\""
    ;; "\"note\",\"very simple\",\"csv\",\"writer\",\"\\\"yay!\\\"\""
       (fmt nil (fmt-join (lambda (x) (cat "\"" (slashified x #\" nil) "\""))
                          '("note" "very simple" "csv" "writer" "\"yay!\"")
                          ","))
       )|#
  #|(iss "note,\"very simple\",csv,writer,\"\"\"yay!\"\"\""
       (fmt nil
            (fmt-join
             (cut #'maybe-slashified <> #'char-whitespace? #\" nil)
                       '("note" "very simple" "csv" "writer" "\"yay!\"")
                       ","))

       )|#)


#||
;; columnar formatting

(test "abc\ndef\n" (fmt nil (fmt-columns (list #'dsp "abc\ndef\n"))))
(test "abc123\ndef456\n" (fmt nil (fmt-columns (list dsp "abc\ndef\n") (list dsp "123\n456\n"))))
(test "abc123\ndef456\n" (fmt nil (fmt-columns (list dsp "abc\ndef\n") (list dsp "123\n456"))))
(test "abc123\ndef456\n" (fmt nil (fmt-columns (list dsp "abc\ndef") (list dsp "123\n456\n"))))
(test "abc123\ndef456\nghi789\n"
    (fmt nil (fmt-columns (list dsp "abc\ndef\nghi\n") (list dsp "123\n456\n789\n"))))
(test "abc123wuv\ndef456xyz\n"
    (fmt nil (fmt-columns (list dsp "abc\ndef\n") (list dsp "123\n456\n") (list dsp "wuv\nxyz\n"))))
(test "abc  123\ndef  456\n"
    (fmt nil (fmt-columns (list (cut pad/right 5 <>) "abc\ndef\n") (list dsp "123\n456\n"))))
(test "ABC  123\nDEF  456\n"
    (fmt nil (fmt-columns (list (compose upcase (cut pad/right 5 <>)) "abc\ndef\n")
                         (list dsp "123\n456\n"))))
(test "ABC  123\nDEF  456\n"
    (fmt nil (fmt-columns (list (compose (cut pad/right 5 <>) upcase) "abc\ndef\n")
                         (list dsp "123\n456\n"))))

(test "hello\nworld\n" (fmt nil (with-width 8 (wrap-lines "hello world"))))
(test "\n" (fmt nil (wrap-lines "    ")))

(test          ;; test divide by zero error
 "The  quick
brown  fox
jumped
over   the
lazy dog
"
 (fmt nil (with-width 10 (justify "The quick brown fox jumped over the lazy dog"))))

(test "his message
(http://lists.nongnu.org/archive/html/chicken-users/2010-10/msg00171.html)
to the chicken-users
(http://lists.nongnu.org/mailman/listinfo/chicken-users)\n"
      (fmt nil (with-width 67 (wrap-lines "his message (http://lists.nongnu.org/archive/html/chicken-users/2010-10/msg00171.html) to the chicken-users (http://lists.nongnu.org/mailman/listinfo/chicken-users)"))))

(test "The fundamental list iterator.
Applies KONS to each element of
LS and the result of the previous
application, beginning with KNIL.
With KONS as CONS and KNIL as '(),
equivalent to REVERSE.
"
    (fmt nil (with-width 36 (wrap-lines "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE."))))

(test
"The   fundamental   list   iterator.
Applies  KONS  to  each  element  of
LS  and  the  result of the previous
application,  beginning  with  KNIL.
With  KONS  as CONS and KNIL as '(),
equivalent to REVERSE.
"
    (fmt nil (with-width 36 (justify "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE."))))

(test
"(define (fold kons knil ls)          ; The fundamental list iterator.
  (let lp ((ls ls) (acc knil))       ; Applies KONS to each element of
    (if (null? ls)                   ; LS and the result of the previous
        acc                          ; application, beginning with KNIL.
        (lp (cdr ls)                 ; With KONS as CONS and KNIL as '(),
            (kons (car ls) acc)))))  ; equivalent to REVERSE.
"
    (fmt nil (fmt-columns
             (list
              (cut pad/right 36 <>)
              (with-width 36
                (pretty '(define (fold kons knil ls)
                           (let lp ((ls ls) (acc knil))
                             (if (null? ls)
                                 acc
                                 (lp (cdr ls)
                                     (kons (car ls) acc))))))))
             (list
              (cut cat " ; " <>)
              (with-width 36
                (wrap-lines "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE."))))))

(test
"(define (fold kons knil ls)          ; The fundamental list iterator.
  (let lp ((ls ls) (acc knil))       ; Applies KONS to each element of
    (if (null? ls)                   ; LS and the result of the previous
        acc                          ; application, beginning with KNIL.
        (lp (cdr ls)                 ; With KONS as CONS and KNIL as '(),
            (kons (car ls) acc)))))  ; equivalent to REVERSE.
"
    (fmt nil (with-width 76
              (columnar
               (pretty '(define (fold kons knil ls)
                          (let lp ((ls ls) (acc knil))
                            (if (null? ls)
                                acc
                                (lp (cdr ls)
                                    (kons (car ls) acc))))))
               " ; "
               (wrap-lines "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE.")))))

(test
"- Item 1: The text here is
          indented according
          to the space \"Item
          1\" takes, and one
          does not known what
          goes here.
"
    (fmt nil (columnar 9 (dsp "- Item 1:") " " (with-width 20 (wrap-lines "The text here is indented according to the space \"Item 1\" takes, and one does not known what goes here.")))))

(test
"- Item 1: The text here is
          indented according
          to the space \"Item
          1\" takes, and one
          does not known what
          goes here.
"
    (fmt nil (columnar 9 (dsp "- Item 1:\n") " " (with-width 20 (wrap-lines "The text here is indented according to the space \"Item 1\" takes, and one does not known what goes here.")))))

(test
"- Item 1: The text here is----------------------------------------------------
--------- indented according--------------------------------------------------
--------- to the space \"Item--------------------------------------------------
--------- 1\" takes, and one---------------------------------------------------
--------- does not known what-------------------------------------------------
--------- goes here.----------------------------------------------------------
"
    (fmt nil (pad-char #\- (columnar 9 (dsp "- Item 1:\n") " " (with-width 20 (wrap-lines "The text here is indented according to the space \"Item 1\" takes, and one does not known what goes here."))))))

(test
"a   | 123
bc  | 45
def | 6
"
    (fmt nil (with-width
             20
             (tabular (dsp "a\nbc\ndef\n") " | " (dsp "123\n45\n6\n")))))

;; misc extras

(define-function (string-hide-passwords str)
  (string-substitute (regexp "(pass(?:w(?:or)?d)?\\s?[:=>]\\s+)\\S+" T)
                     "\\1******"
                     str
                     T))

(define hide-passwords
  (make-string-fmt-transformer string-hide-passwords))

(define (string-mangle-email str)
  (string-substitute
   (regexp "\\b([-+.\\w]+)@((?:[-+\\w]+\\.)+[a-z]{2,4})\\b" T)
   "\\1 _at_ \\2"
   str
   T))

(define mangle-email
  (make-string-fmt-transformer string-mangle-email))

(test-end)
||#
