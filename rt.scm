
(require 'midilisp)

;quote the next line to play in real time instead of writing to a midifile
; the real time stuff isn't portable
(begin
    (set! midilisp:port '(file "test.mid"))
    '(set! *scaler* 2))


(define (foo)
    (parallel
	(begin (note 60 1000) (note 64 1000) (note 67 1000) (note 72 3000))
	(begin (parallel
		(note 48 6000)
		(note 52 6000)
		(note 55 6000)))
	(begin (sleep 3000) (note 36 3000)) ))

(define (riff1 k)
    (note k 200)
    (note (+ 2 k) 200)
    (note (+ 4 k) 200))

(define (riff2 k)
    (riff1 k)
    (riff1 (+ k 2))
    (riff1 (+ k 4)))

(define (bar)
    ;(print (now))
    (parallel
	(riff2 48)
	(riff2 60)))

(run (bar))

'(begin
(define *serial-port* #f)
(define-syntax runxx
  (syntax-rules ()
    ((run <exp1> <exp2> ...)
     (begin
	(set! *serial-port* (system:open-serial-output "COM2" 38400))
	(reset-scheduler)
	(begin <exp1> <exp2> ...)
	(close-output-port *serial-port*)
	(set! *serial-port* #f)
	(print "...done") )))))

