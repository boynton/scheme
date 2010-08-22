;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  mflop.scm
;;;;  (C) Nicolas Neuss (Nicolas.Neuss@iwr.uni-heidelberg.de)
;;;;  mflop.lisp is in the public domain.
;;;   Converted to Scheme by Lee Boynton (lee@boynton.com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'util)

(define +N-long+ #x100000)  ; does not fit in secondary cache
(define +N-short+ #x100)    ; fits in primary cache

(define *mflop-delta* 5.0)

(define (current-milliseconds) (system:timestamp))

(define (make-double-float-array size)
  (make-vector size 0.0)
;   (make-signal size 0.0)
)

(define (ddot x y n)
  '(declare (type fixnum n)
	   (type (simple-array double-float (*)) x y))
  '(declare (optimize (safety 0) (space 0) (debug 0) (speed 3)))
  (let ((sum 0))
    (dotimes (i n)
	     (set! sum (+ sum (* (vector-ref x i) (vector-ref y i)))))
    sum))


'(defun daxpy (x y n)
  (declare (type fixnum n)
	   (type (simple-array double-float (*)) x y))
  (declare (optimize (safety 0) (space 0) (debug 0) (speed 3)))
  (loop with s of-type double-float = 0.3d0
	for i of-type fixnum from 0 below n do
	(incf (aref y i) (* s (aref x i)))))

(define (test fn name size)
  (let ((x (make-double-float-array +N-long+))
	(y (make-double-float-array +N-long+)))
    (print 
      name "-"
     (if (= size +N-long+) "long" "short") ": "
     (let loop ((before (current-milliseconds))
		(count 1))
       (let loop2 ((_n_ count))
	 (if (> _n_ 0)
	     (begin (fn x y size) (loop2 (- _n_ 1)))))
       (let ((after (current-milliseconds)))
	 (if (> (/ (- after before) 1000) *mflop-delta*)
	     (/ (* 2 size count 1000)
		(* 1e6 (- after before)))
	     (loop after (* 2 count)))))
     " MFLOPS")))

(define (mflop-test)
  (test ddot 'ddot +N-long+)
  (test ddot 'ddot +N-short+)
  '(test daxpy 'daxpy +N-long+)
  '(test daxpy 'daxpy +N-short+)
)

(mflop-test)
