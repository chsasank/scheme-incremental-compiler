(define fixnum-shift 2)
(define char-shift 8)
(define char-tag #b00001111)
(define bool-shift 7)
(define bool-tag #b0011111)
(define null-val #b00101111)
(define word-size 8)

(define (immediate? x)
    ;check if x is one of immediate type
    (or (integer? x) (char? x) (boolean? x) (null? x)))

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
        (else (error "no immediate representation for" x))))

; infra for primitives 
(define-syntax define-primitive
    (syntax-rules ()
        ((_ (prim-name si arg* ...) b b* ...)
            (begin
                (set-symbol-property! 'prim-name '*is-prim* #t)
                (set-symbol-property! 'prim-name '*arg-count* 
                    (length '(arg* ...)))
                (set-symbol-property! 'prim-name '*emitter*
                    (lambda (si arg* ...) b b* ...))))))

(define (primitive? x)
    ; is x a primitve
    (and (symbol? x) (symbol-property x '*is-prim*)))

(define (primitive-emitter x)
    ; get primitve emitter of x
    (or (symbol-property x '*emitter*) (error "not primitive"))) 

(define (primcall? expr)
    ; is expr a primitive call
    (and (pair? expr) (primitive? (car expr))))

; cases for compiler
(define (compile-program x)    
    (emit-expr (- word-size) x)
    (emit "ret"))

(define (emit-expr si expr)
    (cond
        ((immediate? expr) (emit-immediate expr))
        ((primcall? expr) (emit-primcall si expr))
        (else (error "syntax error"))))

(define (emit-primcall si expr)
    (let ((prim (car expr))
          (args (cdr expr)))
        (apply (primitive-emitter prim) si args)))

(define (emit-immediate expr)
    (emit "movl $~a, %eax" (immediate-rep expr)))

; add primitives
(define-primitive (add1 si arg)
    (emit-expr si arg)
    (emit "addl $~a, %eax" (immediate-rep 1)))

(define-primitive (sub1 si arg)
    (emit-expr si arg)
    (emit "subl $~a, %eax" (immediate-rep 1)))

(define-primitive (integer->char si arg)
    (emit-expr si arg)
    (emit "shll $~a, %eax" (- char-shift fixnum-shift))
    (emit "orl $~a, %eax" char-tag))

(define-primitive (char->integer si arg)
    (emit-expr si arg)
    (emit "shrl $~a, %eax" (- char-shift fixnum-shift)))

(define (zeroflag-to-bool)
    ; convert zeroflag set by cmp to boolean
    ; used in other primitives
    ; make eax 0
    (emit "movl $0, %eax")
    ; set lower byte of eax register to 0/1 based on zeroflag
    (emit "sete %al")
    ; convert 0/1 in eax to bool
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (zero? si arg)
    (emit-expr si arg)
    ; compare 0 to result and set zero flag if true
    (emit "cmpl $0, %eax")
    (zeroflag-to-bool))

(define-primitive (null? si arg)
    (emit-expr si arg)
    ; compare null value bits to result and set zero flag if true
    (emit "cmpl $~a, %eax" null-val)
    (zeroflag-to-bool))

(define-primitive (not si arg)
    ; return true only if arg is #f
    (emit-expr si arg)
    ; compare immediate rep of #f to result
    ; set zero flag if true
    (emit "cmpl $~a, %eax" (immediate-rep #f))
    (zeroflag-to-bool))

(define-primitive (integer? si arg)
    (emit-expr si arg)
    ; apply fixnum mask (1 bits fixnum-shift times)
    (emit "and $~s, %al" (- (ash 1 fixnum-shift) 1))
    (emit "cmp $~s, %al" #b00)
    (zeroflag-to-bool))

(define-primitive (char? si arg)
    (emit-expr si arg)
    ; apply fixnum mask (1 bits char-shift times)
    (emit "and $~s, %al" (- (ash 1 char-shift) 1))
    (emit "cmp $~s, %al" char-tag)
    (zeroflag-to-bool))

(define-primitive (boolean? si arg)
    (emit-expr si arg)
    ; apply fixnum mask (1 bits bool-shift times)
    (emit "and $~s, %al" (- (ash 1 bool-shift) 1))
    (emit "cmp $~s, %al" bool-tag)
    (zeroflag-to-bool))

; binary primitives

(define-primitive (+ si arg1 arg2)
    (emit-expr si arg1)
    (emit "movl %eax, ~a(%rsp)" si)
    (emit-expr (- si word-size) arg2)
    (emit "addl ~a(%rsp), %eax" si)
)