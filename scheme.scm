;;
;; LeeScheme/scheme.scm. Copyright (C) Lee Richard Boynton, 1992-1994.
;;

;
;The first expression bootstraps the world by reading the rest of the file.
;
(let loop ((obj (read)))
  (if (eof-object? obj)
      #t
      (begin
	((system:compile (system:macroexpand obj)))
	(loop (read)))))

;(system:declare! '(bind-global-procs #t) '(disable-type-checks #t))
(system:declare! '(bind-global-procs #t))

;
;; this normally gets redefined in the read-eval-print-loop to catch errors
;
(define (system:*restart* . whatever)
  (display system:*error-info*) (newline)
  (system:exit -1))

(define (system:raise-error the-error)
  (system:*restart* the-error))

(define system:*display-line* #f) ;gets redefined below

(define (system:object->string obj)
  (let ((buf (make-string 256)))
    (let ((port (system:open-output-string buf)))
      (write obj port)
      (let ((buf (system:string-port-buffer port))
	    (pos (system:string-port-position port)))
	(substring buf 0 pos)))))

(define (system:void? obj) (eq? obj system:*void*))

(define (system:error-string)
  (let ((type (vector-ref system:*error-info* 0))
	(desc (vector-ref system:*error-info* 1))
	(obj (vector-ref system:*error-info* 2))
	(state (vector-ref system:*error-info* 3))
	(fun (vector-ref system:*error-info* 4)))
    (case type
      ((interrupt) "Interrupt")
      ((error)
       (string-append desc 
		      (if (system:void? obj)
			  ""
			  (string-append " : "
					 (system:object->string obj))))))))

(define (system:error-context-string)
  (let ((fun (vector-ref system:*error-info* 4))
        (type (vector-ref system:*error-info* 3)))
    (if (symbol? type)
        (string-append "While "
                       (symbol->string type)
		       " '" (symbol->string fun) "'")
        #f)))

(define (system:print-error . rest)
  (let ((port (if (pair? rest) (car rest) (current-output-port)))
        (context (system:error-context-string)))
    (if (eq? (vector-ref system:*error-info* 0) 'interrupt)
        (begin
          (display "*** Interrupt ***" port)
          (newline port))
        (begin
          (display "*** Error: " port)
          (display (system:error-string) port)
          (newline port)
          (if context 
              (begin
                (display "*** " port)
                (display context port)
                (newline port)))))))


(define (system:error . args)
  (apply system:*display-line* "*** Error: " args)
  (system:raise-error 'generic-error))

(define (system:interrupt)
  (system:*display-line* "*** Interrupt ***")
  (system:raise-error 'interrupt))

(define system:*load-verbose* #f)
(define system:*macroexpand-hook* (lambda (expr) (system:macroexpand expr)))
(define (load file)
  (let ((path (system:find-file file)))
    (if path
	(let ((inport (open-input-file path)))
	  (if system:*load-verbose*
	      (system:*display-line* "[loading " path "]"))
	  (let loop ((obj (read inport)))
	    (if (eof-object? obj)
		#t
		(begin
		  ((system:compile (system:*macroexpand-hook* obj)))
		  (loop (read inport))))
	  (close-input-port inport)
	  #t))
	(system:error "File not found: " file))))

(define (features) system:*features*)

(define (provided? sym) (and (memq sym system:*features*) #t))

(define (provide sym)
  (if (not (memq sym system:*features*))
      (set! system:*features* (cons sym system:*features*)))
  sym)

(define (require sym . rest)
  (if (not (memq sym system:*features*))
      (load (if (null? rest) sym (car rest)))
      #t))

(define (open filename)
  (let ((path (system:find-file filename)))
    (and path (system:command (string-append "open " path)))))

(define (print-object obj)
  (display obj))

(define (print . objects)
  (define (print:internal args)
    (if (null? args)
	(newline)
	(begin
	  (print-object (car args))
	  (print:internal (cdr args)))))
  (print:internal objects)
  (system:flush-output-port))
(set! system:*display-line* print)

(define error system:error)

(define unbound? system:unbound?)

(define (macroexpand expr)
  (system:*macroexpand-hook* expr))

(define (compile expr)
  (system:compile (macroexpand expr)))

(define (eval expr)
  ((compile expr)))

(define (standard-io)
  (set! system:*current-input-port* (system:stdin))
  (set! system:*current-output-port* (system:stdout))
  #t)

(define (force-output)
  (system:flush-output-port))

(begin
  (load "r4rs.scm")
  (close-input-port (current-input-port))
  (standard-io)
  (system:enable-keyboard-interrupts #t)
  (load system:*repl-filename*)
  (system:exit))

