(define (compile-program x)
    (emit "movl $~a, %eax" x)
    (emit "ret"))

