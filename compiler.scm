(define fixnum-shift 2)
(define char-shift 8)
(define char-tag #b00001111)
(define bool-shift 7)
(define bool-tag #b0011111)
(define null-val #b00101111)
(define word-size 8)

; cases for compiler
(define (compile-program x)
    ; initialize stack index - word-size so as not to 
    ; overwrite return address
    (emit-expr (- word-size) '() x)
    (emit "ret"))

(define (emit-expr si env expr)
    (cond
        ((immediate? expr) (emit-immediate expr))
        ((variable? expr) (emit-variable-ref env expr))
        ((let? expr) (emit-let si env expr))
        ((if? expr) (emit-if si env expr))
        ((primcall? expr) (emit-primcall si env expr))
        (else (error "syntax error: " expr))))

; immediate constants
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
(define (emit-immediate expr)
    (emit "movl $~a, %eax" (immediate-rep expr)))

; infra for primitives 
(define-syntax define-primitive
    (syntax-rules ()
        ((_ (prim-name si env arg* ...) b b* ...)
            (begin
                (set-symbol-property! 'prim-name '*is-prim* #t)
                (set-symbol-property! 'prim-name '*arg-count* 
                    (length '(arg* ...)))
                (set-symbol-property! 'prim-name '*emitter*
                    (lambda (si env arg* ...) b b* ...))))))

(define (primitive? x)
    ; is x a primitve
    (and (symbol? x) (symbol-property x '*is-prim*)))

(define (primitive-emitter x)
    ; get primitve emitter of x
    (or (symbol-property x '*emitter*) (error "not primitive"))) 

(define (primcall? expr)
    ; is expr a primitive call
    (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall si env expr)
    (let ((prim (car expr))
          (args (cdr expr)))
        (apply (primitive-emitter prim) si env args)))


; unary primitives
(define-primitive (add1 si env arg)
    (emit-expr si env arg)
    (emit "addl $~a, %eax" (immediate-rep 1)))

(define-primitive (sub1 si env arg)
    (emit-expr si env arg)
    (emit "subl $~a, %eax" (immediate-rep 1)))

(define-primitive (integer->char si env arg)
    (emit-expr si env arg)
    (emit "shll $~a, %eax" (- char-shift fixnum-shift))
    (emit "orl $~a, %eax" char-tag))

(define-primitive (char->integer si env arg)
    (emit-expr si env arg)
    (emit "shrl $~a, %eax" (- char-shift fixnum-shift)))

(define (emit-zeroflag-to-bool)
    ; convert zeroflag set by cmp to boolean
    ; used in other primitives
    ; make eax 0
    (emit "movl $0, %eax")
    ; set lower byte of eax register to 0/1 based on zeroflag
    (emit "sete %al")
    ; convert 0/1 in eax to bool
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (zero? si env arg)
    (emit-expr si env arg)
    ; compare 0 to result and set zero flag if true
    (emit "cmpl $0, %eax")
    (emit-zeroflag-to-bool))

(define-primitive (null? si env arg)
    (emit-expr si env arg)
    ; compare null value bits to result and set zero flag if true
    (emit "cmpl $~a, %eax" null-val)
    (emit-zeroflag-to-bool))

(define-primitive (not si env arg)
    ; return true only if arg is #f
    (emit-expr si env arg)
    ; compare immediate rep of #f to result
    ; set zero flag if true
    (emit "cmpl $~a, %eax" (immediate-rep #f))
    (emit-zeroflag-to-bool))

(define-primitive (integer? si env arg)
    (emit-expr si env arg)
    ; apply fixnum mask (1 bits fixnum-shift times)
    (emit "and $~s, %al" (- (ash 1 fixnum-shift) 1))
    (emit "cmp $~s, %al" #b00)
    (emit-zeroflag-to-bool))

(define-primitive (char? si env arg)
    (emit-expr si env arg)
    ; apply fixnum mask (1 bits char-shift times)
    (emit "and $~s, %al" (- (ash 1 char-shift) 1))
    (emit "cmp $~s, %al" char-tag)
    (emit-zeroflag-to-bool))

(define-primitive (boolean? si env arg)
    (emit-expr si env arg)
    ; apply fixnum mask (1 bits bool-shift times)
    (emit "and $~s, %al" (- (ash 1 bool-shift) 1))
    (emit "cmp $~s, %al" bool-tag)
    (emit-zeroflag-to-bool))

; binary primitives
(define (next-stack-index si)
    (- si word-size))

(define (emit-stack-save si)
    ; save output of previous instructions to stack
    (emit "movl %eax, ~a(%rsp)" si))

(define-primitive (+ si env arg1 arg2)
    (emit-expr si env arg1)
    
    ; save result of arg1 in stack
    (emit-stack-save si)

    ; move stack index by a word so that 
    ; above is not overwritten
    (emit-expr (next-stack-index si) env arg2)

    ; add result from stack with arg2 result
    ; this works because of integer tag = b00
    (emit "addl ~a(%rsp), %eax" si))

(define-primitive (- si env arg1 arg2)
    (emit-expr si env arg2)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg1)
    (emit "subl ~a(%rsp), %eax" si))

(define-primitive (* si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg2)
    ; remove b00 from int for one of the args
    (emit "shrl $~a, %eax" fixnum-shift)
    (emit "imull ~a(%rsp), %eax" si))

(define-primitive (= si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg2)
    (emit "cmpl ~a(%rsp), %eax" si)
    (emit-zeroflag-to-bool))

(define-primitive (< si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    ; set lower byte of eax register to 0/1 if cmpl is less than
    (emit "setl %al")
    ; convert 0/1 in eax to bool
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (<= si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    (emit "setle %al")
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (> si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    (emit "setg %al")
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

(define-primitive (>= si env arg1 arg2)
    (emit-expr si env arg1)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env arg2)
    (emit "cmpl %eax, ~a(%rsp)" si)
    
    (emit "movl $0, %eax")
    (emit "setge %al")
    (emit "sall $~a, %eax" bool-shift)
    (emit "orl $~a, %eax" bool-tag))

; variables and let
(define (variable? x)
    ; variable is any symbol that's not a primitive
    (and (symbol? x) (not (primitive? x))))

(define (emit-variable-ref env var)
    (cond
        ((assoc var env)
            (emit "movl ~a(%rsp), %eax" (cdr (assoc var env))))
        (else (error "unknown variable: " var))))

(define (let? expr)
    (and (pair? expr) (eq? (car expr) 'let)))

(define (let-bindings expr)
    ; get bindings list from expr
    (car (cdr expr)))

(define (let-body expr)
    ; get bindings list from expr
    (car (cdr (cdr expr))))

(define (process-let si env bindings body)
    ; recursively add bindings to env
    (cond
        ((null? bindings)
            (emit-expr si env body))
        (else
            (let* ((b (car bindings))
                    (lhs (car b))
                    (rhs (car (cdr b))))
                ; emit code for rhs of binding b
                (emit-expr si env rhs)
                ; save output into stack
                (emit-stack-save si)
                (process-let
                    (next-stack-index si) (acons lhs si env)
                    (cdr bindings) body)))))

(define (emit-let si env expr)
    (process-let si env (let-bindings expr) (let-body expr)))

; conditional expressions
(define label-count 0)

(define (unique-label)
    (let ((L (format #f "L_~a" label-count)))
        (set! label-count (+ label-count 1))
        L))

(define (if? expr)
    (and (pair? expr) (eq? (car expr) 'if)))

(define (emit-if si env expr)
    (let ((altern-label (unique-label))
          (end-label (unique-label))
          (test (cadr expr))
          (conseq (caddr expr))
          (altern (cadddr expr)))
        (emit-expr si env test)
        (emit "cmpl $~a, %eax" (immediate-rep #f))
        ; jump to alternative label if test is 
        ; equal to false
        (emit "je ~a" altern-label)
        (emit-expr si env conseq)
        ; jump unconditionally to end so that altern 
        ; is not executed if test is truthy
        (emit "jmp ~a" end-label)
        ; emit label for altern
        (emit "~a:" altern-label)
        (emit-expr si env altern)
        ; emit label for end
        (emit "~a:" end-label)))
