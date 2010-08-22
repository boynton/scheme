;; LeeScheme/repl.scm. Copyright (C) Lee Richard Boynton, 1993-2000.
;
; REPL - a read-eval-print-loop and miscellaneous utilities.
;

(standard-io)
(system:enable-keyboard-interrupts #t)

(begin (newline) (display (system:version)) (newline) (newline))

(require 'apropos)

;
;The read-eval-print-loop global variables.
;
(define system:*prompt* "? ")
(define system:*result-prompt* "= ")

(define (system:*abort-hook*) #t) ; called after an error or an interrupt
(define (system:*exec-hook*) #t) ; called before executing toplevel expression

(define exit system:exit)

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
		 (system:exit))
        (else
		 (standard-io)
		 (system:print-error))))))

(let ((config (system:find-file "config.scm")))
;  (if config (load config))
  (system:repl))


