
(begin

(define free-majs #f)
(define free-rows #f)
(define free-mins #f)
(define qrows (make-vector 8))

(define (tryit col)
  (let loop ((row 0))
    (if (and (vector-ref free-rows row)
	     (vector-ref free-majs (+ col row))
	     (vector-ref free-mins (+ (- col row) 7)))
	(begin
	  (vector-set! qrows col row)
	  (vector-set! free-rows row #f)
	  (vector-set! free-majs (+ col row) #f)
	  (vector-set! free-mins (+ (- col row) 7) #f)
	  (if (= col 7)
	      #t
	      (if (tryit (+ col 1))
		  #t
		  (begin
		    (vector-set! free-rows row #t)
		    (vector-set! free-majs (+ col row) #t)
		    (vector-set! free-mins (+ (- col row) 7) #t)
		    (if (< row 7) (loop (+ row 1)) #f)))))
	(if (< row 7) (loop (+ row 1)) #f))))

(do ((i 0 (+ i 1))) ((= i 50))
  (set! free-majs (make-vector 15 #t))
  (set! free-rows (make-vector 8 #t))
  (set! free-mins (make-vector 15 #t))
  (tryit 0))

)


	
