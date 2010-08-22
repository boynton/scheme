;;
;; Midifile output. Copyright (c) 1994-2000 Lee Richard Boynton.
;;

(provide 'midifile)
(require 'struct)

(define-structure midifile
  (file #f)
  (time 0)
  (size 0)
  (head #f)
  (tail #f))

(define midifile:*default-tempo* 600)

(define (midifile:open-output-file name . rest)
  (let ((tempo (if (pair? rest) (car rest) midifile:*default-tempo*))
	(mf (midifile:create name)))
    (let ((header (list (list 'dummy))))
      (midifile:set-head! mf header)
      (midifile:set-tail! mf header)
      (midifile:tempo-change mf tempo))
    (midifile:set-head! mf (cdr (midifile:head mf)))
    mf))

(define (midifile:write-event! mf . bytes)
  (define (encode-dt d t)
    (if (< d 128)
	(list (+ d t))
	(append (encode-dt (quotient d 128) 128)
		(list (+ t (remainder d 128))))))
  (let ((dt (- (now) (midifile:time mf)))
	(tmp (midifile:tail mf)))
    (midifile:set-time! mf (now))
    (let ((e (list (append (if (< dt 128) (list dt) (encode-dt dt 0)) bytes))))
      (midifile:set-size! mf (+ (midifile:size mf) (length (car e))))
      (set-cdr! tmp e)
      (midifile:set-tail! mf e))))

(define (midifile:tempo-change mf beats-per-second)
(print "tempo = " beats-per-second)
  (let ((tempo (inexact->exact (round (/ 60000000 beats-per-second)))))
    (midifile:write-event! mf
			   #xff #x51 #x03
			   (remainder (quotient tempo 65536) 256)
			   (remainder (quotient tempo 256) 256)
			   (remainder tempo 256)))
  beats-per-second)


(define (midifile:close mf)
  (let ((p (open-output-file (midifile:file mf)))
	(e (midifile:head mf)))
    (define (encode-byte i)
      (write-char (integer->char i) p))
    (define (encode-short i)
      (encode-byte (quotient i 256))
      (encode-byte (remainder i 256)))
    (define (encode-long i)
      (encode-byte (quotient i 16777216))
      (encode-byte (remainder (quotient i 65536) 256))
      (encode-byte (remainder (quotient i 256) 256))
      (encode-byte (remainder i 256)))
    (define (encode-string s)
      (display s p))
    (if (not (pair? e))
	(error "Empty midifile"))
    (midifile:write-event! mf #xff #x2f 0)
    (encode-string "MThd")
    (encode-long 6)
    (encode-short 0)
    (encode-short 1)
    (encode-short 96)
    (encode-string "MTrk")
    (encode-long (midifile:size mf))
    (let loop ((this (car e)) (rest (cdr e)) )
      (for-each (lambda (c) (write-char (integer->char c) p)) this)
      (if (pair? rest) (loop (car rest) (cdr rest))))
    (close-output-port p)))

;
