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
