(define (do-work-rec x y) (+ x y (if (< 0 x)
                                     (if (< 0 y)
                                         (do-work-rec x (- y 1))
                                         (do-work-rec (- x 1) x))
                                     0)))
(define (do-work x y) (Concept (string-concatenate
                                (map number->string (iota (do-work-rec x y))))))
