;; LeeScheme/struct.scm. Copyright (C) Lee Richard Boynton, 1993.
;
; User-defined structured types
; 
; The "define-structure" macro is a simplified version of CL's "defstruct" and
; MIT Scheme's "define-structure"
;
; For example:
;
;    (define-structure foo bar glorp)
;
; creates a type of object named foo that has the two fields bar and glorp,
; each initialized with the value #f. The following:
;
;    (define-structure foo (bar 23) glorp)
;
; is the same except that the field "bar" gets initialized to 23 (must be a
; constant -- it does not get evaluated. This is weird!)
; In both cases, the following functions are all automatically generated:
;
;    (foo? obj) -> #t only if obj is a foo object
;    (foo:create [bar-init [glorp-init]]) -> #<foo>
;    (foo:bar obj) -> the value of the bar field of obj
;    (foo:set-bar! obj val) ; sets the bar field, return value unspecified
;    (foo:glorp obj)
;    (foo:set-glorp! obj val)
;
;;

(provide 'struct)
(require 'macros)

;
;function:  (type-of <object>) -> <type-name-symbol>
;
(define type-of system:type-of)

;
;function:  (make-structure <name> [<symbol> | (<symbol> <expr>)]*)
;syntax:    (define-structure <name> [<symbol> | (<symbol> <expr>)]*)
;
(define (structure:define . rest) #f) ; gets redefined below

(define-syntax define-structure
  (syntax-rules ()
    ((define-structure <name> <slot-spec> ...)
     (structure:define '<name> '(<slot-spec> ...)))))


;
;implementation
;

(let ((*types* '()))

  (define (object-predicate-name type-name)
    (string->symbol (string-append 
		     (symbol->string type-name)
		     "?")))
  
  (define (object-constructor-name type-name)
    (string->symbol (string-append 
		     (symbol->string type-name)
		     ":create")))
  
  (define (object-slot-accessor-name type-name slot-name)
    (string->symbol (string-append 
		     (symbol->string type-name)
		     ":"
		     (symbol->string slot-name))))
  
  (define (object-slot-modifier-name type-name slot-name)
    (string->symbol (string-append 
		     (symbol->string type-name)
		     ":set-"
		     (symbol->string slot-name)
		     "!")))
  
  (define (object-constructor type-name)
    (lambda rest
      (let ((v (apply vector
	  	      (append rest (list-tail (car (find-type type-name))
				      (length rest))))))
 	(system:set-vector-type! v type-name)
	v)))
  
  (define (object-predicate type-name)
    (lambda (o) (eq? (type-of o) type-name)))
  
  (define (object-accessor slotnum)
    (lambda (obj)
      (vector-ref obj slotnum)))
  
  (define (object-modifier slotnum)
    (lambda (obj val)
      (vector-set! obj slotnum val)))
  
  (define (find-type name)
    (let ((entry (assq name *types*)))
      (if entry
	 (cdr entry)
	 (error "Unknown structure type: " name))))

  (define (new-type name slots)
    (define (register-type name def)
      (let ((previous-entry (assq name *types*)))
	(if previous-entry
	    (set-cdr! previous-entry def)
	    (set! *types* (cons (cons name def) *types*))))
      name)
    (let loop ((spec (car slots)) (remaining (cdr slots)) (index 0))
      (let ((slot-name (if (pair? spec)
			   (car spec)
			   (if (symbol? spec) spec
			       (error "Not a symbol: " spec)))))
	(system:set-global! (object-slot-accessor-name name slot-name)
			    (object-accessor index))
	(system:set-global! (object-slot-modifier-name name slot-name)
			    (object-modifier index)))
      (if (pair? remaining)
	  (loop (car remaining) (cdr remaining) (+ index 1))))
    (system:set-global! (object-constructor-name name) (object-constructor name))
    (system:set-global! (object-predicate-name name) (object-predicate name))
    (let ((inits (map (lambda (o) (if (pair? o) (cadr o) #f)) slots)))
      (register-type name (cons inits '())))
    name)

  (set! structure:define new-type)
    
  #t)

