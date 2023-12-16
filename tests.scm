(load "linker.scm")

(define (run-test expr expected-result)
    (let ((result (run expr)))
        (if (string=? result expected-result)
            (begin (display expr)
                (display ": passed\n"))
            (error 'failed expr))))

; integers
(run-test 0 "0\n")
(run-test 1 "1\n")
(run-test -1 "-1\n")
(run-test 10 "10\n")
(run-test -10 "-10\n")
(run-test 2736 "2736\n")
(run-test -2736 "-2736\n")
(run-test 536870911 "536870911\n")
(run-test -536870912 "-536870912\n")

; characters
(run-test #\! "#\\!\n")
(run-test #\" "#\\\"\n")
(run-test #\# "#\\#\n")
(run-test #\' "#\\'\n")

(run-test #\0 "#\\0\n")
(run-test #\1 "#\\1\n")
(run-test #\; "#\\;\n")

(run-test #\A "#\\A\n")
(run-test #\B "#\\B\n")
(run-test #\Y "#\\Y\n")
(run-test #\Z "#\\Z\n")
(run-test #\a "#\\a\n")
(run-test #\b "#\\b\n")
(run-test #\y "#\\y\n")
(run-test #\z "#\\z\n")

; booleans
(run-test #t "#t\n")
(run-test #f "#f\n")

; empty list
(run-test '() "()\n")

; add1
(run-test '(add1 0) "1\n")
(run-test '(add1 -1) "0\n")
(run-test '(add1 1) "2\n")
(run-test '(add1 -100) "-99\n")
(run-test '(add1 536870910) "536870911\n")
(run-test '(add1 -536870912) "-536870911\n")
(run-test '(add1 (add1 0)) "2\n")
(run-test '(add1 (add1 (add1 (add1 (add1 (add1 12)))))) "18\n")

; sub1
(run-test '(sub1 0) "-1\n")
(run-test '(sub1 -1) "-2\n")
(run-test '(sub1 1) "0\n")
(run-test '(sub1 -100) "-101\n")
(run-test '(sub1 (add1 0)) "0\n")

; integer->char, char->integer
(run-test '(integer->char 65) "#\\A\n")
(run-test '(integer->char 97) "#\\a\n")
(run-test '(integer->char 122) "#\\z\n")
(run-test '(integer->char 90) "#\\Z\n")
(run-test '(integer->char 48) "#\\0\n")
(run-test '(integer->char 57) "#\\9\n")
(run-test '(char->integer #\A) "65\n")
(run-test '(char->integer #\a) "97\n")
(run-test '(char->integer #\z) "122\n")
(run-test '(char->integer #\Z) "90\n")
(run-test '(char->integer #\0) "48\n")
(run-test '(char->integer #\9) "57\n")
(run-test '(char->integer (integer->char 12)) "12\n")
(run-test '(integer->char (char->integer #\x)) "#\\x\n")