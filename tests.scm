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

; zero?
(run-test '(zero? 0) "#t\n")
(run-test '(zero? 1) "#f\n")
(run-test '(zero? -1) "#f\n")
(run-test '(zero? (sub1 1)) "#t\n")
; null?
(run-test '(null? ()) "#t\n")
(run-test '(null? #f) "#f\n")
(run-test '(null? #t) "#f\n")
(run-test '(null? (null? ())) "#f\n")
(run-test '(null? #\a) "#f\n")
(run-test '(null? 0) "#f\n")
(run-test '(null? -10) "#f\n")
(run-test '(null? 10) "#f\n")
; integer?
(run-test '(integer? 0) "#t\n")
(run-test '(integer? 1) "#t\n")
(run-test '(integer? -1) "#t\n")
(run-test '(integer? 37287) "#t\n")
(run-test '(integer? -23873) "#t\n")
(run-test '(integer? 536870911) "#t\n")
(run-test '(integer? -536870912) "#t\n")
(run-test '(integer? #t) "#f\n")
(run-test '(integer? #f) "#f\n")
(run-test '(integer? ()) "#f\n")
(run-test '(integer? #\Q) "#f\n")
(run-test '(integer? (integer? 12)) "#f\n")
(run-test '(integer? (integer? #f)) "#f\n")
(run-test '(integer? (integer? #\A)) "#f\n")
(run-test '(integer? (char->integer #\r)) "#t\n")
(run-test '(integer? (integer->char 12)) "#f\n")
; char?
(run-test '(char? #\a) "#t\n")
(run-test '(char? #\Z) "#t\n")
(run-test '(char? #t) "#f\n")
(run-test '(char? #f) "#f\n")
(run-test '(char? ()) "#f\n")
(run-test '(char? (char? #t)) "#f\n")
(run-test '(char? 0) "#f\n")
(run-test '(char? 23870) "#f\n")
(run-test '(char? -23789) "#f\n")
; boolean?
(run-test '(boolean? #t) "#t\n")
(run-test '(boolean? #f) "#t\n")
(run-test '(boolean? 0) "#f\n")
(run-test '(boolean? 1) "#f\n")
(run-test '(boolean? -1) "#f\n")
(run-test '(boolean? ()) "#f\n")
(run-test '(boolean? #\a) "#f\n")
(run-test '(boolean? (boolean? 0)) "#t\n")
(run-test '(boolean? (integer? (boolean? 0))) "#t\n")
; not
(run-test '(not #t) "#f\n")
(run-test '(not #f) "#t\n")
(run-test '(not 15) "#f\n")
(run-test '(not ()) "#f\n")
(run-test '(not #\A) "#f\n")
(run-test '(not (not #t)) "#t\n")
(run-test '(not (not #f)) "#f\n")
(run-test '(not (not 15)) "#t\n")
(run-test '(not (integer? 15)) "#f\n")
(run-test '(not (integer? #f)) "#t\n")


; +
(run-test '(+ 1 2) "3\n")
(run-test '(+ 1 -2) "-1\n")
(run-test '(+ -1 2) "1\n")
(run-test '(+ -1 -2) "-3\n")
(run-test '(+ 536870911 -1) "536870910\n")
(run-test '(+ 536870910 1) "536870911\n")
(run-test '(+ -536870912 1) "-536870911\n")
(run-test '(+ -536870911 -1) "-536870912\n")
(run-test '(+ 536870911 -536870912) "-1\n")
(run-test '(+ 1 (+ 2 3)) "6\n")
(run-test '(+ 1 (+ 2 -3)) "0\n")
(run-test '(+ 1 (+ -2 3)) "2\n")
(run-test '(+ 1 (+ -2 -3)) "-4\n")
(run-test '(+ -1 (+ 2 3)) "4\n")
(run-test '(+ -1 (+ 2 -3)) "-2\n")
(run-test '(+ -1 (+ -2 3)) "0\n")
(run-test '(+ -1 (+ -2 -3)) "-6\n")
(run-test '(+ (+ 1 2) 3) "6\n")
(run-test '(+ (+ 1 2) -3) "0\n")
(run-test '(+ (+ 1 -2) 3) "2\n")
(run-test '(+ (+ 1 -2) -3) "-4\n")
(run-test '(+ (+ -1 2) 3) "4\n")
(run-test '(+ (+ -1 2) -3) "-2\n")
(run-test '(+ (+ -1 -2) 3) "0\n")
(run-test '(+ (+ -1 -2) -3) "-6\n")
(run-test '(+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) "45\n")
(run-test '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 9)))))))) "45\n")

; -
(run-test '(- 1 2) "-1\n")
(run-test '(- 1 -2) "3\n")
(run-test '(- -1 2) "-3\n")
(run-test '(- -1 -2) "1\n")
(run-test '(- 536870910 -1) "536870911\n")
(run-test '(- 536870911 1) "536870910\n")
(run-test '(- -536870911 1) "-536870912\n")
(run-test '(- -536870912 -1) "-536870911\n")
(run-test '(- 1 536870911) "-536870910\n")
(run-test '(- -1 536870911) "-536870912\n")
(run-test '(- 1 -536870910) "536870911\n")
(run-test '(- -1 -536870912) "536870911\n")
(run-test '(- 536870911 536870911) "0\n")
(run-test '(- -536870911 -536870912) "1\n")
(run-test '(- 1 (- 2 3)) "2\n")
(run-test '(- 1 (- 2 -3)) "-4\n")
(run-test '(- 1 (- -2 3)) "6\n")
(run-test '(- 1 (- -2 -3)) "0\n")
(run-test '(- -1 (- 2 3)) "0\n")
(run-test '(- -1 (- 2 -3)) "-6\n")
(run-test '(- -1 (- -2 3)) "4\n")
(run-test '(- -1 (- -2 -3)) "-2\n")
(run-test '(- 0 (- -2 -3)) "-1\n")
(run-test '(- (- 1 2) 3) "-4\n")
(run-test '(- (- 1 2) -3) "2\n")
(run-test '(- (- 1 -2) 3) "0\n")
(run-test '(- (- 1 -2) -3) "6\n")
(run-test '(- (- -1 2) 3) "-6\n")
(run-test '(- (- -1 2) -3) "0\n")
(run-test '(- (- -1 -2) 3) "-2\n")
(run-test '(- (- -1 -2) -3) "4\n")
(run-test '(- (- (- (- (- (- (- (- 1 2) 3) 4) 5) 6) 7) 8) 9) "-43\n")
(run-test '(- 1 (- 2 (- 3 (- 4 (- 5 (- 6 (- 7 (- 8 9)))))))) "5\n")

; *
(run-test '(* 2 3) "6\n")
(run-test '(* 2 -3) "-6\n")
(run-test '(* -2 3) "-6\n")
(run-test '(* -2 -3) "6\n")
(run-test '(* 536870911 1) "536870911\n")
(run-test '(* 536870911 -1) "-536870911\n")
(run-test '(* -536870912 1) "-536870912\n")
(run-test '(* -536870911 -1) "536870911\n")
(run-test '(* 2 (* 3 4)) "24\n")
(run-test '(* (* 2 3) 4) "24\n")
(run-test '(* (* (* (* (* 2 3) 4) 5) 6) 7) "5040\n")
(run-test '(* 2 (* 3 (* 4 (* 5 (* 6 7))))) "5040\n")

; =
(run-test '(= 12 13) "#f\n")
(run-test '(= 12 12) "#t\n")
(run-test '(= 16 (+ 13 3)) "#t\n")
(run-test '(= 16 (+ 13 13)) "#f\n")
(run-test '(= (+ 13 3) 16) "#t\n")
(run-test '(= (+ 13 13) 16) "#f\n")

; <
(run-test '(< 12 13) "#t\n")
(run-test '(< 12 12) "#f\n")
(run-test '(< 13 12) "#f\n")
(run-test '(< 16 (+ 13 1)) "#f\n")
(run-test '(< 16 (+ 13 3)) "#f\n")
(run-test '(< 16 (+ 13 13)) "#t\n")
(run-test '(< (+ 13 1) 16) "#t\n")
(run-test '(< (+ 13 3) 16) "#f\n")
(run-test '(< (+ 13 13) 16) "#f\n")

; <=
(run-test '(<= 12 13)  "#t\n")
(run-test '(<= 12 12)  "#t\n")
(run-test '(<= 13 12)  "#f\n")
(run-test '(<= 16 (+ 13 1))  "#f\n")
(run-test '(<= 16 (+ 13 3))  "#t\n")
(run-test '(<= 16 (+ 13 13))  "#t\n")
(run-test '(<= (+ 13 1) 16)  "#t\n")
(run-test '(<= (+ 13 3) 16)  "#t\n")
(run-test '(<= (+ 13 13) 16)  "#f\n")

; >
(run-test '(> 12 13) "#f\n")
(run-test '(> 12 12) "#f\n")
(run-test '(> 13 12) "#t\n")
(run-test '(> 16 (+ 13 1)) "#t\n")
(run-test '(> 16 (+ 13 3)) "#f\n")
(run-test '(> 16 (+ 13 13)) "#f\n")
(run-test '(> (+ 13 1) 16) "#f\n")
(run-test '(> (+ 13 3) 16) "#f\n")
(run-test '(> (+ 13 13) 16) "#t\n")

; >=
(run-test '(>= 12 13) "#f\n")
(run-test '(>= 12 12) "#t\n")
(run-test '(>= 13 12) "#t\n")
(run-test '(>= 16 (+ 13 1)) "#t\n")
(run-test '(>= 16 (+ 13 3)) "#t\n")
(run-test '(>= 16 (+ 13 13)) "#f\n")
(run-test '(>= (+ 13 1) 16) "#f\n")
(run-test '(>= (+ 13 3) 16) "#t\n")
(run-test '(>= (+ 13 13) 16) "#t\n")

; let
(run-test '(let ((x 5)) x) "5\n")
(run-test '(let ((x (+ 1 2))) x) "3\n")
(run-test '(let ((x (+ 1 2))) 
     (let ((y (+ 3 4)))
       (+ x y))) 
   "10\n")
(run-test '(let ((x (+ 1 2))) 
     (let ((y (+ 3 4)))
       (- y x)))
   "4\n")
(run-test '(let ((x (+ 1 2))
         (y (+ 3 4)))
     (- y x))
   "4\n")
(run-test '(let ((x (let ((y (+ 1 2))) (* y y))))
     (+ x x))
   "18\n")
(run-test '(let ((x (+ 1 2)))
     (let ((x (+ 3 4)))
       x)) 
   "7\n")
(run-test '(let ((x (+ 1 2)))
     (let ((x (+ x 4)))
       x)) 
   "7\n")
(run-test '(let ((t (let ((t (let ((t (let ((t (+ 1 2))) t))) t))) t))) t)
   "3\n")
(run-test '(let ((x 12))
     (let ((x (+ x x)))
       (let ((x (+ x x)))
         (let ((x (+ x x)))
           (+ x x)))))
   "192\n")

; if
(run-test '(if #t 12 13) "12\n")
(run-test '(if #f 12 13) "13\n")
(run-test '(if 0 12 13)  "12\n")
(run-test '(if () 43 ()) "43\n")
(run-test '(if #t (if 12 13 4) 17) "13\n")
(run-test '(if #f 12 (if #f 13 4)) "4\n")
(run-test '(if #\X (if 1 2 3) (if 4 5 6)) "2\n")
(run-test '(if (not (boolean? #t)) 15 (boolean? #f)) "#t\n")
(run-test '(if (if (char? #\a)
                  (boolean? #\b) (integer? #\c)) 119 -23) "-23\n")
(run-test '(if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7) "6\n") 
(run-test '(if (not (if 
                (if (not 1) (not 2) (not 3)) 4 5)) 6 7) "7\n") 
(run-test '(not (if (not (if 
                (if (not 1) (not 2) (not 3)) 4 5)) 6 7)) "#f\n") 
(run-test '(if (char? 12) 13 14) "14\n")
(run-test '(if (char? #\a) 13 14) "13\n")
(run-test '(add1 (if (sub1 1) (sub1 13) 14)) "13\n")

; cons
(run-test '(pair? (cons 1 2)) "#t\n")
(run-test '(car (cons 1 2)) "1\n")
(run-test '(cdr (cons 1 2)) "2\n")
(run-test '(car (cons 1 (cons 2 3))) "1\n")
(run-test '(car (cdr (cons 1 (cons 2 3)))) "2\n")
(run-test '(pair? (cons 1 2)) "#t\n")
(run-test '(pair? 12) "#f\n")
(run-test '(pair? #t) "#f\n")
(run-test '(pair? #f) "#f\n")
(run-test '(pair? ()) "#f\n")
(run-test '(integer? (cons 12 43)) "#f\n")
(run-test '(boolean? (cons 12 43)) "#f\n")
(run-test '(null? (cons 12 43)) "#f\n")
(run-test '(not (cons 12 43)) "#f\n")
(run-test '(if (cons 12 43) 32 43) "32\n")
(run-test '(car (cons 1 23)) "1\n")
(run-test '(cdr (cons 43 123)) "123\n")
(run-test '(car (car (cons (cons 12 3) (cons #t #f)))) "12\n")
(run-test '(cdr (car (cons (cons 12 3) (cons #t #f)))) "3\n")
(run-test '(car (cdr (cons (cons 12 3) (cons #t #f)))) "#t\n")
(run-test '(cdr (cdr (cons (cons 12 3) (cons #t #f)))) "#f\n")
(run-test '(let ((x (let ((y (+ 1 2))) (* y y))))
            (cdr (cons x (+ x x)))) "18\n")

; procedures
(run-test '(letrec () 12) "12\n")
(run-test '(letrec () (let ((x 5)) (+ x x))) "10\n")
(run-test '(letrec ((f (lambda () 5))) 7) "7\n")
(run-test '(letrec ((f (lambda () 5))) 
                (let ((x 12)) x)) "12\n")
(run-test '(letrec ((f (lambda () 5)))
                (app f)) "5\n")
(run-test '(letrec ((f (lambda () 5)))
                (let ((x (app f))) x)) "5\n")
(run-test '(letrec ((f (lambda () 5)))
                (+ (app f) 6)) "11\n")
(run-test '(letrec ((f (lambda () 5)))
                (- 20 (app f))) "15\n")
(run-test '(letrec ((f (lambda () 5)))
                (+ (app f) (app f))) "10\n")
(run-test '(letrec ((f (lambda () (+ 5 7)))
                    (g (lambda () 13)))
                (+ (app f) (app g))) "25\n")
(run-test '(letrec ((f (lambda (x) (+ x 12))))
                (app f 13)) "25\n")
(run-test '(letrec ((f (lambda (x) (+ x 12))))
                (app f (app f 10))) "34\n")
(run-test '(letrec ((f (lambda (x) (+ x 12))))
                (app f (app f (app f 0)))) "36\n")
(run-test '(letrec ((f (lambda (x y) (+ x y))) 
                    (g (lambda (x) (+ x 12))))
                (app f 16 
                    (app f (app g 0) 
                    (+ 1 (app g 0))))) "41\n")
(run-test '(letrec ((f (lambda (x) (app g x x)))
                    (g (lambda (x y) (+ x y))))
                (app f 12)) "24\n")
(run-test '(letrec ((f (lambda (x) 
                            (if (zero? x)
                                1
                                (* x (app f (sub1 x)))))))
                (app f 5)) "120\n")
(run-test '(letrec ((e (lambda (x)
                            (if (zero? x) #t (app o (sub1 x)))))
                    (o (lambda (x)
                            (if (zero? x) #f (app e (sub1 x))))))
                (app e 25)) "#f\n")