(define loop (lambda (i l) (if (< i l) (loop (+ 1 i) l) l)))
(loop 0 3000000)

