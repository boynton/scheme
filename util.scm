(provide 'util)
(require 'macros)

(define-syntax dolist
  (syntax-rules ()
     ((dolist (<var> <init>) <expr> ...)
      (let ((init <init>))
	(if (pair? init)
	    (let loop ((<var> (car init)) (rest (cdr init)))
	      (begin <expr> ...)
	      (if (pair? rest)
		  (loop (car rest) (cdr rest)))))))))


(define-syntax dotimes
  (syntax-rules ()
     ((dotimes (<var> <count>) <expr> ...)
		(let loop ((<var> 0))
			(if (< <var> <count>)
			  (begin
				(begin <expr> ...)
			    (loop (+ 1 <var>))))))))

(define-syntax dovector
  (syntax-rules ()
    ((dovector (<var> <vec>) <expr> ...)
     (let loop ((__i 0) (__n (vector-length <vec>)))
       (if (< __i __n)
	   (begin
	     (let ((<var> (vector-ref <vec> __i)))
	       <expr> ...)
	     (loop (+ 1 __i) __n)))))))

