;
;; A real-time version of music system described in
;; "Music Programming in Scheme". Lee Richard Boynton. ICMC 1992.
;; Same as scheduler.scm, except that it uses non-portable
;; methods to operate with midi or serial I/O in real time.
;;
;; Requires the following non-standard primitives:
;;  (system:time)                               ; returns a millisecond timestamp
;;  (system:pause <ms>)                         ; pause the specified number of milliseconds
;;  (midi:open)                                 ; open and initialize the MIDI interface
;;  (midi:close)                                ; close the MIDI interface
;;  (midi:write <b1> ...)                       ; write the bytes to the MIDI interface
;;  (system:open-serial-output <device> <baud>) ; open and return a serial output port
;;  (system:write-fixnums <port> <b1> ...)      ; write the bytes to the port
;;
;; Copyright (c) 1992-2000 Lee Richard Boynton.
;

(provide 'midilisp)
(require 'macros)

(define (midilisp:write . args) (error "midilisp not open"))
(define (midilisp:close) (error "midilisp not open"))
(define midilisp:time system:timestamp)
(define midilisp:sleep system:pause)


(define *midilisp:open* #f)
(define *midilisp:port-name* 'midi)
(define *midilisp:serial-port* #f)
(define *midilisp:file* #f)

(define (midilisp:open . args)
    (if *midilisp:open*
        (midilisp:close))
    (let ((sym (if (null? args) *midilisp:port-name* (car args))))
        (set! *midilisp:port-name* sym)
        (let ((str (symbol->string sym)))
            (if (eq? sym 'midi)
                (begin
                    (set! *midilisp:serial-port* #f)
                    (set! midilisp:write midi:write)
                    (set! midilisp:close (lambda () (midi:close) (set! *midilisp:open* #f)))
                    (midi:open))
		(if (eq? sym 'file)
		    (begin
			(set! *midilisp:serial-port* #f)
			(set! *midilisp:file* (midifile:open-output-file (cadr args)))
			(set! midilisp:write (lambda (b1 b2 b3) (print (now) ": " b1 " " b2 " " b3) (midifile:write-event! *midilisp:file* b1 b2 b3)))
			(set! midilisp:close (lambda () (midifile:close *midilisp:file*) (set! *midilisp:file* #f) (set! *midilisp:open* #f)))
			(midi:open))
		    (begin
			(set! *midilisp:serial-port* (system:open-serial-output str 38400))
			(set! midilisp:write (lambda args (apply system:write-fixnums *midilisp:serial-port* args)))
			(set! midilisp:close (lambda () (close-output-port *midilisp:serial-port*) (set! *midilisp:open* #f))) ))))
        sym))

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
	(let ((dur (- t (midilisp:time))))
		(if (> dur 0)
			(midilisp:sleep dur))))
		
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
	    (if (not *midilisp:file*) (wait-until (car e)))
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

(define *scaler* 1)
'(define (now) (/ *now* *scaler*))
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
(define-default *vol* 100)
(define-default *dur* 1)


;(define (prog n)p
;	(midilisp:write 192 n))

(define (damper n)
    (midilisp:write #xb0 #x40 n))


'(define (note k d)
    (let ( (k2 (+ (default *trans*) k))
	   (d2 (* (default *dur*)))
	   (v (* (default *vol*))) )
	(midilisp:write #x90 k2 v)
	(sleep d2)
	(midilisp:write #x90 k2 0)))

(define (note k d)
    (midilisp:write #x90 k 100)
    (sleep d)
    (midilisp:write #x90 k 0))

(define midilisp:port '(midi))

(require 'midifile)

(define-syntax run
  (syntax-rules ()
    ((run <expr> ...)
     (begin
        (print "Running...")
	(apply midilisp:open midilisp:port)
	(if (not *midilisp:file*) (set! *now* (midilisp:time)))
	(begin <expr> ...)
	   (midilisp:close)))))

