(load "./compiler.scm")
(use-modules (ice-9 textual-ports))

(define output-file-param (make-parameter #f))

(define (emit format-string . args)
  ; Format the assembly code using format
  (apply format (output-file-param) format-string args)
  (newline (output-file-param)))

(define (build x)
    (define output-file (open-output-file "/tmp/scheme_entry.s"))
    (output-file-param output-file)
    (display (string-append 
        ".text\n"
        ".p2align 4\n"
        ".globl	scheme_entry\n"        
        ".type	scheme_entry, @function\n"
        "scheme_entry:\n"
        "mov %rsp, %rcx\n"
        "mov %rdi, %rsp\n"
        "call L_scheme_entry\n"
        "mov %rcx, %rsp\n"
        "ret\n")
        output-file)

    (compile-program x)

    (display (string-append 
        ".LFE0:\n"
        ".size	scheme_entry, .-scheme_entry\n"
        ".section	.note.GNU-stack,\"\",@progbits\n")
        output-file)
    
    (close-output-port output-file))

(define (run x)
    (build x)
    (system "gcc -w runtime.c /tmp/scheme_entry.s -o /tmp/scheme_entry")
    (system "/tmp/scheme_entry > /tmp/scheme_entry.out")
    (call-with-input-file "/tmp/scheme_entry.out"
        get-string-all))
