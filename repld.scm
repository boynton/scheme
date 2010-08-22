;; LeeScheme/repld.scm. Copyright (C) Lee Richard Boynton, 1993-2000.
;
; REPL - a telnet daemon read-eval-print-loop
;

;(standard-io)
(define system:standard-io standard-io)
(define (standard-io) #f)

(system:enable-keyboard-interrupts #t)

;(begin (newline) (display (system:version)) (newline) (newline))



;
;The read-eval-print-loop global variables.
;
(define system:*prompt* "? ")
(define system:*result-prompt* "= ")

(define (system:*abort-hook*) #t) ; called after an error or an interrupt
(define (system:*exec-hook*) #t) ; called before executing toplevel expression

(define exit (lambda args (print 'no-way)))

(define system:client #f)

(define (reset-io)
    (if system:client
        (begin
            (set! system:*current-input-port* system:client)
            (set! system:*current-output-port* system:client))
        (standard-io)))

(define (system:repl)
  (define (void? obj)
    (eq? obj system:*void*))
  (define (repl:eval expr)
    (let ((code (compile expr)))
      (system:*exec-hook*)
      (code)))
  (let ((expr #f) (result #f))
    (do () ((eof-object? expr))
      (case (call-with-current-continuation
			 (lambda (restart-repl)
			   (set! system:*restart* restart-repl)
			   (system:*abort-hook*)
			   (system:enable-interrupts)
			   (do () ((eof-object? expr) 'eof)
				 (display system:*prompt*)
				 (set! expr (read))
				 (if (not (eof-object? expr))
					 (begin
					   (set! result (repl:eval expr))
					   (if (not (void? result))
						   (begin
							 (display system:*result-prompt*)
							 (write result)
							 (newline))))))))
        ((eof)
		 (print "end of input!")
		 (exit))
        (else
		 (reset-io)
		 (system:print-error))))))

(let ((server (system:listen 23000)))
    (print "listening on port 23000")
    (let main ()
        (call-with-current-continuation
            (lambda (big-restart)
                (if system:client
                    (begin
                        (close-input-port system:client)
                        (set! system:client #f)))
                (set! exit (lambda args (big-restart args)))
                (let next-connection ((client (system:accept server)))
                    (system:port-set-edlin! client #t)
                    (set! system:client client)
                    (set! system:*current-input-port* client)
                    (set! system:*current-output-port* client)
                    ;load config?
                    (begin (newline) (display (system:version)) (newline) (newline))
                    (system:repl))))
        (main)))

