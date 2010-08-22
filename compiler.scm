;; LeeScheme/compiler.scm. Copyright (C) Lee Richard Boynton, 1993-2000.

(define *bind-primitives* #t)
(define *compiler-constants* '(+)) ;;

(define *compiler-label-num* 0)
(define (generate-label)
  (set! *compiler-label-num* (+ 1 *compiler-label-num*))
  *compiler-label-num*)

(define (compile-top-level expr)
  (set! *compiler-label-num* 0)
  (compile-lambda '() (list expr) '()))

(define (pretty-code p . rest)
  (define port (if (pair? rest) (car rest) (current-output-port)))
  (define (pretty1 c indent tab)
    (if (pair? c)
	(let ((cc (car c)))
	    (if (pair? cc)
		(if (eq? (car cc) 'closure)
		    (begin
		      (display indent port)
		      (display tab port)
		      (display (car cc) port)
		      (newline port)
		      (pretty1 (cdadr cc) (string-append indent tab) tab))
		    (begin
		      (display indent port)
		      (display tab port)
		      (display (car cc) port)
		      (if (pair? (cdr cc))
			(begin
			   (display tab port)
			   (display (cadr cc) port)
			   (for-each (lambda (e)
					  (display " " port)
					  (display e port))
				     (cddr cc))))
		      (newline port)))
		(begin
		   (display indent port)
	           (display (car c) port)
		   (newline port)))
	    (pretty1 (cdr c) indent tab))))
  (pretty1 (cdr p) "" (make-string 1 #\tab)))

(define (disassemble p)
  (define (print-code c tab)
    (if (pair? c)
	(begin
	    (if (pair? (car c))
	        (begin
		   (for-each (lambda (e) (display tab) (display e))
			     (car c))
		   (newline))
	        (display (car c)))
	    (print-code (cdr c) tab))))
  (print-code (caddr p) (make-string 1 #\tab)))

(define (c expr)
  (disassemble (compile-top-level expr)))

(define (make-procedure args code env name)
    `(proc ,@code))

(define *unspecified* (vector))

(define (compile-expr x env val? more?)
  (cond
    ((symbol? x) (compile-variable x env val? more?))
    ((eq? *unspecified* x) '())
    ((pair? x)
     (if (macro? (car x))
	 (compile-expr (macroexpand x) env val? more?)
         (let ((argc (length (cdr x))))
           (case (car x)
	    ((quote)
	     (if (not (= argc 1)) (syntax-error x))
	     (compile-constant (cadr x) val? more?))
	    ((begin)
	     (if (< argc 1) (syntax-error x))
	     (compile-sequence (cdr x) env val? more?))
	    ((set!)
	     (if (not (= argc 2)) (syntax-error x))
	     (if (not (symbol? (cadr x))) (syntax-error x))
	     (append (compile-expr (caddr x) env #t #t)
		     (emit-set (cadr x) env)
		     (if val? '() (emit 'pop))
		     (if more? '() (emit 'return))))
	    ((if)
	     (if (= argc 2)
	         (compile-if (cadr x) (caddr x) *unspecified* env val? more?)
	         (if (= argc 3)
	             (compile-if (cadr x) (caddr x) (cadddr x) env val? more?)
		     (syntax-error x))))
	    ((lambda)
	     (if (< argc 2) (syntax-error x))
	     (if val?
	         (let ((f (compile-lambda (cadr x) (cddr x) env)))
		    (append (emit 'closure f) (if more? '() (emit 'return))))
	         '()))
	    (else ; a funcall
		(compile-funcall (car x) (cdr x) env val? more?))))))
    (else (compile-constant x val? more?))))

(define (compile-constant k val? more?)
  (if val?
    (if more?
      (emit 'const k)
      (append (emit 'const k) (emit 'return)))
    '()))

(define (compile-variable sym env val? more?)
  (if val?
    (if more?
      (emit-ref sym env)
      (append (emit-ref sym env) (emit 'return)))
    '()))

(define (compile-sequence s env val? more?)
  (if (null? (cdr s))
      (compile-expr (car s) env val? more?)
      (append (compile-expr (car s) env #f #t)
	      (compile-sequence (cdr s) env val? more?))))

(define (compile-args s env)
  (if (null? s)
    '()
    (append (compile-args (cdr s) env)
	    (compile-expr (car s) env #t #t))))

(define (compile-lambda args body env)
  (define (compile-frame args n)
    (cond
      ((null? args) (emit 'frame n))
      ((symbol? args) (emit 'frame-rest n))
      ((and (pair? args) (symbol? (car args)))
       (compile-frame (cdr args) (+ n 1)))
      (else (error "illegal argument list: " full-arglist))))
  (define (args->env l)
    (cond
      ((null? l) l)
      ((pair? l) (cons (car l) (args->env (cdr l))))
      (else (list l))))
  (if (null? args)
      (make-procedure args (compile-sequence body env #t #f) env "")
      (make-procedure args
		  (append (compile-frame args 0)
			  (compile-sequence body
					    (cons (args->env args) env)
					    #t
					    #f))
		  env
		  "")))

(define *primitives* '(;(sym argc primop side-effect-free?)
			(+ 2 add #t)
			(- 2 sub #t)
			(- 1 neg #t)
			(* 2 mul #t)
			(/ 2 div #t)
			(= 2 numeq #t)
			(car 1 car #t)
			(cdr 1 cdr #t)
			(cons 2 cons #t) ))			

(define (primitive? sym env)
  (and (symbol? sym) (not (find-local sym env)) (assq sym *primitives*)))

(define (macro? sym) #f)

(define (side-effect-free-primitive? sym)
  (cdr (assq sym *primitives*)))

(define (compile-funcall fun args env val? more?)
  (cond
   ((primitive? fun env)
    (if (and (not val?) (side-effect-free-primitive? fun))
        (compile-sequence args env #f more?)
        (append (compile-args args env) (emit 'primop fun))))
   ((and (pair? fun) (> (length fun) 2)
	 (eq? (car fun) 'lambda) (null? (cadr fun)))
    (if (null? args)
        (compile-sequence (cddr fun) env val? more?)
        (syntax-error (cons fun args))))
   (more?
    (let ((k (generate-label)))
      (append (compile-args args env)
	      (compile-expr fun env #t #t)
	      (emit 'call (length args))
	      (if val? '() (emit 'pop)))))
   (else
    (append (compile-args args env)
	    (compile-expr fun env #t #t)
	    (emit 'jump (length args))))))

(define (compile-if predicate consequent antecedent env val? more?)
  (cond
    ((not predicate) ; (if #f x y) -> y
     (if (eq? antecedent *unspecified*)
         (compile-expr #f env val? more?)
         (compile-expr antecedent env val? more?)))
    ((or (symbol? predicate) (pair? predicate))
	(if (and (pair? predicate) (eq? (car predicate) 'not)
		(primitive? 'not env)
		(= (length predicate) 2)) ; (if (not p) x y) -> (if p y x)
	   (compile-if (cadr predicate) antecedent consequent env val? more?)
	   (let ((pcode (compile-expr predicate env #t #t))
		 (ccode (compile-expr consequent env val? more?))
		 (acode (compile-expr antecedent env val? more?)))
	     (cond
		((equal? ccode acode) ; (if p x x) -> (begin p x)
		 (append (compile-expr predicate env #f #t) ccode))
		((null? ccode)
		 (let ((l2 (generate-label)))
		   (append pcode (emit 'tbra l2) acode (list l2))))
		((null? acode)
		 (let ((l1 (generate-label)))
                   (append pcode (emit 'fbra l1) ccode (list l1)
			   (if more? '() (emit 'return)))))
		(else
		 (let ((l1 (generate-label))
		       (l2 (if more? (generate-label))))
		    (append pcode
			    (emit 'fbra l1)
			    ccode
			    (if more? (emit 'bra l2) '())
			    (list l1)
			    acode
			    (if more? (list l2) '()))))))))
    (else
	(compile-expr consequent env val? more?))))

(define (syntax-error x)
  (error "Syntax error: " x))


(define (find-local sym env)  ;((i j) (k) (m n)), "j" -> (0.1), "m" -> (2.0)
  (if (null? env)
      #f
      (let loop1 ((i 0) (e env))
        (let loop2 ((j 0) (f (car e)))
	  (if (eq? (car f) sym)
	      (cons i j)
	      (if (null? (cdr f))
		(if (null? (cdr e))
		    #f
  		    (loop1 (+ i 1) (cdr e)))
		(loop2 (+ j 1) (cdr f))))))))

;
;code emission
;
(define (emit opcode . args)
  (list (cons opcode args)))

(define (emit-ref sym env)
  (let ((loc (find-local sym env)))
    (if loc
	(emit 'loc (car loc) (cdr loc) sym)
	(emit 'glob sym))))

(define (emit-set sym env)  
  (let ((loc (find-local sym env)))
    (if loc
	(emit 'setloc (car loc) (cdr loc) ";" sym)
	(if (memq sym *compiler-constants*)
	    (error "Cannot set constant: " sym)
	    (emit 'setglob sym)))))

(define (compile-file infile outfile)
	(define outport (open-output-file outfile))
	(define (cf filename)
		(let ((path filename))
			(let ((inport (open-input-file path)))
				(let loop ((obj (read inport)))
					(if (eof-object? obj)
						#t
						(if (and (pair? obj) (eq? (car obj) 'load))
							(begin
								(cf (if (symbol? (cadr obj))
									(symbol->string (cadr obj))
									(cadr obj)))
								(loop (read inport)))
							(begin
								(begin (display ";" outport) (newline outport)
									(display "; "  outport) (write obj outport) (newline outport)
									(display ";" outport) (newline outport))
								(let ((tmp (compile-top-level obj)))
									(begin (display "; "  outport) (write tmp outport) (newline outport) (display ";" outport) (newline outport))
									(pretty-code tmp outport)
									(newline outport)
									(loop (read inport))))))
					(close-input-port inport)))))
	(cf infile)
	(close-output-port outport))

(define testfile "../LeeLisp/test")
;(define testfile "benchx")

(define (compile-ll basepath)
	(let ( (infile (string-append basepath ".lsp"))
		   (outfile (string-append basepath ".lap")) )
		(print "compiling " infile)
		(compile-file infile outfile)
		(print "output written to " outfile)
		'ok))

;; the following is for batch mode, but still reports errors
(standard-io)
(let ((err (call-with-current-continuation
		(lambda (restart-repl)
			(set! system:*restart* restart-repl)
			shit
			(compile-ll testfile))) ))
	(if (eq? err 'error)
		(begin
			(system:print-error))))

(system:exit)
