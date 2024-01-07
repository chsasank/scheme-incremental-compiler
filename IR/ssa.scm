(define (fun x y)
    (define (loop x1 y1)
        (define (if-cond)
            (let ((x-gt-y (> x1 y1)))
                (if x-gt-y (if-x-greater) (if-y-greater))))

        (define (if-x-greater)
            (let ((new-x (- x1 y1)))
                (loop new-x y1)))

        (define (if-y-greater)
            (let ((new-y (- y1 x1)))
                (loop x1 new-y)))

        (define (end) x1)
        
        (let ((cmp (not (= x1 y1))))
            (if cmp (if-cond) (end))))
    
    (loop x y))

(display (fun 28 2821))