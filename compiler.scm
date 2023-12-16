(define fixnum-shift 2)
(define char-shift 8)
(define char-tag #b00001111)
(define bool-shift 7)
(define bool-tag #b0011111)
(define null-val #b00101111)


(define (compile-program x)
    (define (immediate-rep x)
        ;Convert x into tagged pointer representation
        (cond
            ; bit shift integers by 2 bits
            ((integer? x) (ash x fixnum-shift))
            ; bit shift by 8 integers and add tag
            ; for character
            ((char? x)
                (logior char-tag (ash (char->integer x) char-shift)))
            ; same for boolean
            ((boolean? x) 
                (logior bool-tag (ash (if x 1 0) bool-shift)))
            ; empty list
            ((null? x) null-val)
            (else (error "no immediate representation for" x))

        ))

    (emit "movl $~a, %eax" (immediate-rep x))
    (emit "ret"))
