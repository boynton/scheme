;; LeeScheme/r4rs.scm. Copyright (C) Lee Richard Boynton, 1993-2000.
;
;Standard Scheme syntax and non-primitive functions
;

(define (list . values) values)
(define (caar p) (car (car p)))
(define (cadr p) (car (cdr p)))
(define (cdar p) (cdr (car p)))
(define (cddr p) (cdr (cdr p)))
(define (caaar p) (car (car (car p))))
(define (caadr p) (car (car (cdr p))))
(define (cadar p) (car (cdr (car p))))
(define (caddr p) (car (cdr (cdr p))))
(define (cdaar p) (cdr (car (car p))))
(define (cdadr p) (cdr (car (cdr p))))
(define (cddar p) (cdr (cdr (car p))))
(define (cdddr p) (cdr (cdr (cdr p))))
(define (caaaar p) (car (car (car (car p)))))
(define (caaadr p) (car (car (car (cdr p)))))
(define (caadar p) (car (car (cdr (car p)))))
(define (caaddr p) (car (car (cdr (cdr p)))))
(define (cadaar p) (car (cdr (car (car p)))))
(define (cadadr p) (car (cdr (car (cdr p)))))
(define (caddar p) (car (cdr (cdr (car p)))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))
(define (cdaaar p) (cdr (car (car (car p)))))
(define (cdaadr p) (cdr (car (car (cdr p)))))
(define (cdadar p) (cdr (car (cdr (car p)))))
(define (cdaddr p) (cdr (car (cdr (cdr p)))))
(define (cddaar p) (cdr (cdr (car (car p)))))
(define (cddadr p) (cdr (cdr (car (cdr p)))))
(define (cdddar p) (cdr (cdr (cdr (car p)))))
(define (cddddr p) (cdr (cdr (cdr (cdr p)))))

(define (system:make-promise proc)
  (let ((result-ready? #f) (result #f))
    (lambda ()
      (if result-ready?
	  result
	  (let ((x (proc)))
	    (if result-ready?
		result
		(begin
		  (set! result-ready? #t)
		  (set! result x)
		  result)))))))

(define (force promise)
  (if (procedure? promise)
      (promise)
      promise))

(define (call-with-input-file name proc)
  (let* ((inport (open-input-file name))
	 (result (proc inport)))
    (close-input-port inport)
    result))

(define (call-with-output-file name proc)
  (let* ((outport (open-output-file name))
	 (result (proc outport)))
    (close-output-port outport)
    result))

(define (map fun arg1 . args)
  (define (map1 fun args)
    (if (null? args) '() (cons (fun (car args)) (map1 fun (cdr args)))))
  (define (mapn fun arglists)
    (if (null? arglists)
	'()
	(cons (apply fun (car arglists)) (mapn fun (cdr arglists)))))
  (if (null? args)
      (map1 fun arg1)
      (mapn fun (apply system:crack-map-args arg1 args))))

(define (for-each fun arg1 . args)
  (define (for-each1 fun args)
    (if (pair? args)
	(begin (fun (car args)) (for-each1 fun (cdr args)))))
  (define (for-eachn fun arglists)
    (if (pair? arglists)
	(begin (apply fun (car arglists)) (for-eachn fun (cdr arglists)))))
  (if (null? args)
      (for-each1 fun arg1)
      (for-eachn fun (apply system:crack-map-args arg1 args))))

(define (current-output-port) system:*current-output-port*)

(define (current-input-port) system:*current-input-port*)

(define (with-output-to-file name thunk)
  ;;should use dynamic-wind
  (let ((outport (open-output-file name))
	(old-current-output-port system:*current-output-port*))
    (set! system:*current-output-port* outport)
    (thunk)
    (set! system:*current-output-port* old-current-output-port)
    (close-output-port outport)
    #t))

(define (with-input-from-file name thunk)
  ;;should use dynamic-wind
  (let ((inport (open-input-file name))
	(old-current-input-port system:*current-input-port*))
    (set! system:*current-input-port* inport)
    (thunk)
    (set! system:*current-input-port* old-current-input-port)
    (close-input-port inport)
    #t))

(provide 'r4rs)

