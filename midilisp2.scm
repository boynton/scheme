;
;; Non-real-time portable version of music system described in
;; "Music Programming in Scheme". Lee Richard Boynton. ICMC 1992.
;

(provide 'midilisp2)
(require 'macros)

(define *now* 0)
(define *waiting* '())
(define *ready* '())
(define *signals* '())
(define *defaults* '())
(define *global-defaults* '())

(define (schedule continuation timeout cue)
  (let* ((time (+ *now* timeout))
	 (element (list time continuation cue #f *defaults*)))
    (if (or (null? *waiting*) (<= time (caar *waiting*)))
	(set! *waiting* (cons element *waiting*))
	(let ((tmp (cdr *waiting*)))
	  (if (null? tmp)
	      (set-cdr! *waiting* (cons element '()))
	      (let loop ((tail *waiting*) (tail-cdr tmp))
		(if (<= time (caar tail-cdr))
		    (set-cdr! tail (cons element tail-cdr))
		    (if (null? (cdr tail-cdr))
			(set-cdr! tail-cdr (cons element '()))
			(loop (cdr tail) (cdr tail-cdr))))))))))

(define (wait-until t)
	(let ((dur (- t (system:timestamp))))
		(if (> dur 0)
			(system:pause dur))))
		
(define (context-switch)
  (let next-context ((ready *ready*))
    (if (not (null? ready))
	(let ((task (car ready)))
	  (set! *now* (car task))
	  (set! *ready* (cdr ready))
	  (set! *defaults* (cadddr (cdr task)))
	  ((cadr task) (cadddr task))))
    (if (pair? *signals*)
	(let signal-loop ((signal (car *signals*)))
	  (set! *signals* (cdr *signals*))
	  (let ((cue (car signal))
		(value (cdr signal))
		(tail *waiting*))
	    (let loop ((w *waiting*))
	      (if (not (null? w))
		  (if (eq? (caddar w) cue)
		      (let ((tmp (cdr w)))
			(if (eq? w *waiting*)
			    (set! *waiting* tmp)
			    (set-cdr! tail tmp))
			(set-cdr! w *ready*)
			(set! *ready* w)
			(set-car! (car w) *now*)
			(set-car! (cdddar w) value)
			(loop tmp))
		      (begin
			(set! tail w)
			(loop (cdr w)))))))
	  (if (pair? *signals*)
	      (signal-loop (car *signals*))
	      (next-context *ready*))))
    (if (not (null? *waiting*))
	(let ((time (caar *waiting*)))
	  (let wait-loop ((e (car *waiting*)))
	    (wait-until (car e))
	    (set! *waiting* (cdr *waiting*))
	    (set! *ready* (cons e *ready*))
	    (if (and (not (null? *waiting*))
		     (<= (caar *waiting*) time))
		(wait-loop (car *waiting*))))
	  (next-context *ready*)))))

(define (unique-cue) (make-vector 0))

(define (reset-scheduler)
  (set! *now* 0)
  (set! *waiting* '())
  (set! *ready* '())
  (set! *signals* '())
  (set! *defaults* *global-defaults*))

(define (now) *now*)

(define (wait cue timeout)
  (call-with-current-continuation
   (lambda (continuation)
     (schedule continuation timeout cue)
     (context-switch))))

(define (sleep delay)
  (wait (unique-cue) delay))

(define-syntax parallel
  (syntax-rules ()
    ((parallel <exp1> <exp2> ...)
     (let ((count (length '(<exp1> <exp2> ...))))
       (call-with-current-continuation
	(lambda (join)
	  (schedule (lambda (ignore) <exp1> (join #t)) 0 (unique-cue))
	  (schedule (lambda (ignore) <exp2> (join #t)) 0 (unique-cue))
	  ...))
       (if (> count 0)
	   (begin
	     (set! count (- count 1))
	     (context-switch)))))))

(define (signal cue value)
  (call-with-current-continuation
   (lambda (continuation)
     (schedule continuation 0 cue)
     (set! *signals* (cons (cons cue value) *signals*))
     (context-switch))))

(define-syntax define-default
  (syntax-rules ()
    ((default <name> <value>)
     (let ((tmp (cons '<name> <value>)))
       (set! *global-defaults* (cons tmp *global-defaults*))
       (set! *defaults* (cons tmp *defaults*))))))

(define-syntax default
  (syntax-rules ()
    ((default name)
     (let ((binding (assq 'name *defaults*)))
       (if binding
	   (cdr binding)
	   (error "unbound default:" 'name))))))

(define-syntax let-default
  (syntax-rules ()
    ((let-default ((<name1> <val1>) (<name2> <val2>) ...)
		  <expr1> <expr2> ...)
     (let ((saved-defaults *defaults*) (result #f))
       (set! *defaults* (cons (cons '<name1> <val1>) *defaults*))
       (set! *defaults* (cons (cons '<name2> <val2>) *defaults*))
       ...
       (set! result (begin <expr1> <expr2> ...))
       (set! *defaults* saved-defaults)
       result))))


(define-default *trans* 0)

(define (prog n)
	(midi:write 192 n))

(define (note k dur)
	(let ((k2 (+ (default *trans*) k)))
		(midi:write 144 k2 100)
		(sleep dur)
		(midi:write 144 k2 0)))

(define-syntax run
  (syntax-rules ()
    ((run <expr> ...)
     (begin
	   (midi:open)
       (set! *now* (system:timestamp))
       (begin <expr> ...)
	   (midi:close)))))

