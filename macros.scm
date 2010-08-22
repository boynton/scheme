;;; -*- Scheme -*-
;;;; macwork.scm: Will Clinger's macros that work.  Modified by Ken Dickey.
;;;  Further modified by Lee Boynton.
;
; Copyright 1992 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.

;
; The external entry points and kernel of the macro expander.
;
; Part of this code is snarfed from the Twobit macro expander.

(define mw:define-syntax-scope
  (let ((flag 'letrec))
    (lambda args
      (cond ((null? args) flag)
	    ((not (null? (cdr args)))
	     (apply mw:warn
		    "Too many arguments passed to define-syntax-scope"
		    args))
	    ((memq (car args) '(letrec letrec* let*))
	     (set! flag (car args)))
	    (else (mw:warn "Unrecognized argument to define-syntax-scope"
			  (car args)))))))

(define (macwork:macroexpand def-or-exp)
  (set! mw:renaming-counter 0)
  (mw:desugar-definitions def-or-exp mw:global-syntax-environment))

(define (mw:desugar-definitions exp env)
  (letrec 
    ((define-loop 
       (lambda (exp rest first)
	 (cond ((and (pair? exp)
		     (eq? (mw:syntax-lookup env (car exp))
			  mw:denote-of-begin)
		     (pair? (cdr exp)))
		(define-loop (cadr exp) (append (cddr exp) rest) first))
	       ((and (pair? exp)
		     (eq? (mw:syntax-lookup env (car exp))
			  mw:denote-of-define))
		(let ((exp (desugar-define exp env)))
		  (cond ((and (null? first) (null? rest))
			 exp)
			((null? rest)
			 (cons mw:begin1 (reverse (cons exp first))))
			(else (define-loop (car rest)
					   (cdr rest)
					   (cons exp first))))))
	       ((and (pair? exp)
		     (eq? (mw:syntax-lookup env (car exp))
			  mw:denote-of-define-syntax)
		     (null? first))
		(define-syntax-loop exp rest))
	       ((and (null? first) (null? rest))
		(mw:expand exp env))
	       ((null? rest)
		(cons mw:begin1 (reverse (cons (mw:expand exp env) first))))
	       (else (cons mw:begin1
			   (append (reverse first)
				   (map (lambda (exp) (mw:expand exp env))
					(cons exp rest))))))))
     
     (desugar-define
      (lambda (exp env)
	(cond 
	 ((null? (cdr exp)) (mw:error "Malformed definition" exp))
	 ; (define foo) syntax is transformed into (define foo (undefined)).
	 ((null? (cddr exp))
	  (let ((id (cadr exp)))
	    (redefinition id)
	    (mw:syntax-bind-globally! id (mw:make-identifier-denotation id))
	    (list mw:define1 id mw:undefined)))
	 ((pair? (cadr exp))
	  ; mw:lambda0 is an unforgeable lambda, needed here because the
	  ; lambda expression will undergo further expansion.
	  (desugar-define `(,mw:define1 ,(car (cadr exp))
				     (,mw:lambda0 ,(cdr (cadr exp))
					       ,@(cddr exp)))
			  env))
	 ((> (length exp) 3) (mw:error "Malformed definition" exp))
	 (else (let ((id (cadr exp)))
		 (redefinition id)
		 (mw:syntax-bind-globally! id (mw:make-identifier-denotation id))
		 `(,mw:define1 ,id ,(mw:expand (caddr exp) env)))))))
     
     (define-syntax-loop 
       (lambda (exp rest)
	 (cond ((and (pair? exp)
		     (eq? (mw:syntax-lookup env (car exp))
			  mw:denote-of-begin)
		     (pair? (cdr exp)))
		(define-syntax-loop (cadr exp) (append (cddr exp) rest)))
	       ((and (pair? exp)
		     (eq? (mw:syntax-lookup env (car exp))
			  mw:denote-of-define-syntax))
		(if (pair? (cdr exp))
		    (redefinition (cadr exp)))
		(if (null? rest)
		    (mw:define-syntax exp env)
		    (begin (mw:define-syntax exp env)
			   (define-syntax-loop (car rest) (cdr rest)))))
	       ((null? rest)
		(mw:expand exp env))
	       (else (cons mw:begin1
			   (map (lambda (exp) (mw:expand exp env))
					(cons exp rest)))))))
     
     (redefinition
      (lambda (id)
	(if (symbol? id)
	    (if (not (mw:identifier?
		      (mw:syntax-lookup mw:global-syntax-environment id)))
		(mw:warn "Redefining keyword" id))
	    (mw:error "Malformed variable or keyword" id)))))
    
    ; body of letrec
    
    (define-loop exp '() '())))

; Given an expression and a syntactic environment,
; returns an expression in core Scheme.

(define (mw:expand exp env)
  (if (not (pair? exp))
      (mw:atom exp env)
      (let ((keyword (mw:syntax-lookup env (car exp))))
	(case (mw:denote-class keyword)
	  ((special)
	   (cond
	    ((eq? keyword mw:denote-of-quote)         (mw:quote exp))
	    ((eq? keyword mw:denote-of-lambda)        (mw:lambda exp env))
	    ((eq? keyword mw:denote-of-if)            (mw:if exp env))
	    ((eq? keyword mw:denote-of-set!)          (mw:set exp env))
	    ((eq? keyword mw:denote-of-begin)         (mw:begin exp env))
	    ((eq? keyword mw:denote-of-let-syntax)    (mw:let-syntax exp env))
	    ((eq? keyword mw:denote-of-letrec-syntax)
	     (mw:letrec-syntax exp env))
	    ; @@ let, let*, letrec, paint within quasiquotation -- kend
	    ((eq? keyword mw:denote-of-let)           (mw:let    exp env))
	    ((eq? keyword mw:denote-of-let*)          (mw:let*   exp env))
	    ((eq? keyword mw:denote-of-letrec)        (mw:letrec exp env))
	    ((eq? keyword mw:denote-of-quasiquote)    (mw:quasiquote exp env))
	    ((eq? keyword mw:denote-of-do)            (mw:do     exp env))
	    ((eq? keyword mw:denote-of-case)          (mw:case   exp env)) ;lrb
	    ((or (eq? keyword mw:denote-of-define)
		 (eq? keyword mw:denote-of-define-syntax))
	     ;; slight hack to allow expansion into defines -KenD
	     (if mw:in-define? 
	       (mw:error "Definition out of context" exp)
	       (begin
		 (set! mw:in-define? #t)
		 (let ( (result (mw:desugar-definitions exp env)) )
		   (set! mw:in-define? #f)
		   result))
	    ))
	    (else (mw:bug "Bug detected in mw:expand" exp env))))
	  ((macro) (mw:macro exp env))
	  ((identifier) (mw:application exp env))
	  (else (mw:bug "Bug detected in mw:expand" exp env))
      ) )
) )

(define mw:in-define? #f)  ; should be fluid

(define (mw:atom exp env)
  (cond ((not (symbol? exp))
	 ; Here exp ought to be a boolean, number, character, or string,
	 ; but I'll allow for non-standard extensions by passing exp
	 ; to the underlying Scheme system without further checking.
	 exp)
	(else (let ((denotation (mw:syntax-lookup env exp)))
		(case (mw:denote-class denotation)
		  ((special macro)
		   (mw:error "Syntactic keyword used as a variable" exp))
		  ((identifier) (mw:identifier-name denotation))
		  (else (mw:bug "Bug detected by mw:atom" exp env)))))))

(define (mw:quote exp)
  (if (= (mw:safe-length exp) 2)
      (list mw:quote1 (mw:strip (cadr exp)))
      (mw:error "Malformed quoted constant" exp)))

(define (mw:lambda exp env)
  (if (> (mw:safe-length exp) 2)
      (let* ((formals (cadr exp))
	     (alist (mw:rename-vars (mw:make-null-terminated formals)))
	     (env (mw:syntax-rename env alist))
	     (body (cddr exp)))
	(list mw:lambda1
	      (mw:rename-formals formals alist)
	      (mw:body body env)))
      (mw:error "Malformed lambda expression" exp)))

(define (mw:body body env)
  (define (loop body env defs)
    (if (null? body)
	(mw:error "Empty body"))
    (let ((exp (car body)))
      (if (and (pair? exp)
	       (symbol? (car exp)))
	  (let ((denotation (mw:syntax-lookup env (car exp))))
	    (case (mw:denote-class denotation)
	      ((special)
	       (cond ((eq? denotation mw:denote-of-begin)
		      (loop (append (cdr exp) (cdr body)) env defs))
		     ((eq? denotation mw:denote-of-define)
		      (loop (cdr body) env (cons exp defs)))
		     (else (mw:finalize-body body env defs))))
	      ((macro)
	       (mw:transcribe exp
			     env
			     (lambda (exp env)
			       (loop (cons exp (cdr body))
				     env
				     defs))))
	      ((identifier)
	       (mw:finalize-body body env defs))
	      (else (mw:bug "Bug detected in mw:body" body env))))
	  (mw:finalize-body body env defs))))
  (loop body env '()))

(define (mw:finalize-body body env defs)
  (if (null? defs)
      (let ((body (map (lambda (exp) (mw:expand exp env))
		       body)))
	(if (null? (cdr body))
	    (car body)
	    (cons mw:begin1 body)))
      (let* ((alist (mw:rename-vars '(quote lambda set!)))
	     (env (mw:syntax-alias env alist mw:standard-syntax-environment))
	     (new-quote  (cdr (assq 'quote alist)))
	     (new-lambda (cdr (assq 'lambda alist)))
	     (new-set!   (cdr (assq 'set!   alist))))
	(define (desugar-definition def)
	  (if (> (mw:safe-length def) 2)
	      (cond ((pair? (cadr def))
		     (desugar-definition
		      `(,(car def)
			,(car (cadr def))
			(,new-lambda
			  ,(cdr (cadr def))
			  ,@(cddr def)))))
		    ((= (length def) 3)
		     (cdr def))
		    (else (mw:error "Malformed definition" def env)))
	      (mw:error "Malformed definition" def env)))
	(mw:letrec
	 `(letrec ,(map desugar-definition (reverse defs)) ,@body)
	  env)))
  )

(define (mw:if exp env)
  (let ((n (mw:safe-length exp)))
    (if (or (= n 3) (= n 4))
	(cons mw:if1 (map (lambda (exp) (mw:expand exp env)) (cdr exp)))
	(mw:error "Malformed if expression" exp env))))

(define (mw:set exp env)
  (if (= (mw:safe-length exp) 3)
      `(,mw:set!1 ,(mw:expand (cadr exp) env) ,(mw:expand (caddr exp) env))
      (mw:error "Malformed assignment" exp env)))

(define (mw:begin exp env)
  (if (positive? (mw:safe-length exp))
      `(,mw:begin1 ,@(map (lambda (exp) (mw:expand exp env)) (cdr exp)))
      (mw:error "Malformed begin expression" exp env)))

(define (mw:application exp env)
  (if (> (mw:safe-length exp) 0)
      (map (lambda (exp) (mw:expand exp env))
	   exp)
      (mw:error "Malformed application")))

; I think the environment argument should always be global here.

(define (mw:define-syntax exp env)
  (cond ((and (= (mw:safe-length exp) 3)
	      (symbol? (cadr exp)))
	 (mw:define-syntax1 (cadr exp)
			   (caddr exp)
			   env
			   (mw:define-syntax-scope)))
	((and (= (mw:safe-length exp) 4)
	      (symbol? (cadr exp))
	      (memq (caddr exp) '(letrec letrec* let*)))
	 (mw:define-syntax1 (cadr exp)
			   (cadddr exp)
			   env
			   (caddr exp)))
	(else (mw:error "Malformed define-syntax" exp env))))

(define (mw:define-syntax1 keyword spec env scope)
  (case scope
    ((letrec)  (mw:define-syntax-letrec keyword spec env))
    ((letrec*) (mw:define-syntax-letrec* keyword spec env))
    ((let*)    (mw:define-syntax-let* keyword spec env))
    (else      (mw:bug "Weird scope" scope)))
  (list mw:quote1 keyword))

(define (mw:define-syntax-letrec keyword spec env)
  (mw:syntax-bind-globally!
   keyword
   (mw:compile-transformer-spec spec env)))

(define (mw:define-syntax-letrec* keyword spec env)
  (let* ((env (mw:syntax-extend (mw:syntax-copy env)
				(list keyword)
				'((fake denotation))))
	 (transformer (mw:compile-transformer-spec spec env)))
    (mw:syntax-assign! env keyword transformer)
    (mw:syntax-bind-globally! keyword transformer)))

(define (mw:define-syntax-let* keyword spec env)
  (mw:syntax-bind-globally!
   keyword
   (mw:compile-transformer-spec spec (mw:syntax-copy env))))

(define (mw:let-syntax exp env)
  (if (and (> (mw:safe-length exp) 2)
	   (every (lambda (binding)
		    (and (pair? binding)
			 (symbol? (car binding))
			 (pair? (cdr binding))
			 (null? (cddr binding))))
		    (cadr exp)))
      (mw:body (cddr exp)
	      (mw:syntax-extend env
				(map car (cadr exp))
				(map (lambda (spec)
				       (mw:compile-transformer-spec
					spec
					env))
				     (map cadr (cadr exp)))))
      (mw:error "Malformed let-syntax" exp env)))

(define (mw:letrec-syntax exp env)
  (if (and (> (mw:safe-length exp) 2)
	   (every (lambda (binding)
		    (and (pair? binding)
			 (symbol? (car binding))
			 (pair? (cdr binding))
			 (null? (cddr binding))))
		    (cadr exp)))
      (let ((env (mw:syntax-extend env
				   (map car (cadr exp))
				   (map (lambda (id)
					  '(fake denotation))
					(cadr exp)))))
	(for-each (lambda (id spec)
		    (mw:syntax-assign!
		     env
		     id
		     (mw:compile-transformer-spec spec env)))
		  (map car (cadr exp))
		  (map cadr (cadr exp)))
	(mw:body (cddr exp) env))
      (mw:error "Malformed let-syntax" exp env)))

(define (mw:macro exp env)
  (mw:transcribe exp
		env
		(lambda (exp env)
		  (mw:expand exp env))))

; To do:
; Clean up alist hacking et cetera.

;;-----------------------------------------------------------------
;; The following was added to allow expansion without flattening 
;; LETs to LAMBDAs so that the original structure of the program 
;; is preserved by macro expansion.  I.e. so that usual.scm is not 
;; required. -- added KenD 

(define (mw:process-let-bindings alist binding-list env)  ;; helper proc
  (map (lambda (bind)
	 (list (cdr (assq (car bind) alist)) ; renamed name
	       (mw:body (cdr bind) env)))     ; alpha renamed value expression
       binding-list)
)

(define (mw:strip-begin exp) ;; helper proc: mw:body sometimes puts one in
  (if (and (pair? exp) (eq? (car exp) 'begin))
    (cdr exp)
    exp)
)

; LET
(define (mw:let exp env)
  (let* ( (name (if (or (pair? (cadr exp)) (null? (cadr exp)))
		    #f 
		    (cadr exp)))  ; named let?
	  (binds (if name (caddr exp) (cadr exp)))
	  (body  (if name (cdddr exp) (cddr exp)))
	  (vars  (if (null? binds) #f (map car binds)))
	  (alist (if vars (mw:rename-vars vars) #f))
	  (newenv (if alist (mw:syntax-rename env alist) env))
	)
    (if name  ;; extend env with new name
	(let ( (rename (mw:rename-vars (list name))) )
	  (set! alist (append rename alist))
	  (set! newenv (mw:syntax-rename newenv rename))
    )   )
    `(let
	 ,@(if name (list (cdr (assq name alist))) '())
	 ,(mw:process-let-bindings alist binds env)
	 ,(mw:body body newenv))
) )


; LETREC differs from LET in that the binding values are processed in the
; new rather than the original environment.

(define (mw:letrec exp env)
  (let* ( (binds (cadr exp))
	  (body  (cddr exp))
	  (vars  (if (null? binds) #f (map car binds)))
	  (alist (if vars (mw:rename-vars vars) #f))
	  (newenv (if alist (mw:syntax-rename env alist) env))
	)
    `(letrec
	  ,(mw:process-let-bindings alist binds newenv)
	  ,(mw:body body newenv))
) )


; LET* adds to ENV for each new binding.

(define (mw:let* exp env)
  (let ( (binds (cadr exp))
	 (body  (cddr exp))
       )
    (let bind-loop ( (bindings binds) (newbinds '()) (newenv env) )
       (if (null? bindings)
	  `(let* ,(reverse newbinds) ,(mw:body body newenv))
	   (let* ( (bind (car bindings))
		   (var    (car bind)) 
		   (valexp (cdr bind))
		   (rename (mw:rename-vars (list var)))
		   (next-newenv (mw:syntax-rename newenv rename))
		 )
	     (bind-loop (cdr bindings) 
			(cons (list (cdr (assq var rename))
				    (mw:body valexp newenv))
			      newbinds)
			next-newenv))
) ) ) )


; DO

(define (mw:process-do-bindings var-init-steps alist oldenv newenv)  ;; helper proc
  (map (lambda (vis)
	 (let ( (v (car vis))
		(i (cadr vis))
		(s (if (null? (cddr vis)) (car vis) (caddr vis))))
	   `( ,(cdr (assq v alist)) ; renamed name
	      ,(mw:body (list i) oldenv)     ; init in outer/old env
	      ,(mw:body (list s) newenv) ))) ; step in letrec/inner/new env
       var-init-steps)
)

(define (mw:do exp env)
  (let* ( (vis  (cadr exp))  ; (Var Init Step ...)
	  (ts   (caddr exp)) ; (Test Sequence ...)
	  (com  (cdddr exp)) ; (COMmand ...)
	  (vars (if (null? vis) #f (map car vis)))
	  (rename (if vars (mw:rename-vars vars) #f))
	  (newenv (if vars (mw:syntax-rename env rename) env))
	)
    `(do ,(if vars (mw:process-do-bindings vis rename env newenv) '())
	 ,(if  (null? ts)  '() (mw:strip-begin (mw:body (list ts) newenv)))
	 ,@(if (null? com) '() (list (mw:body com newenv))))
) )

; CASE added by LRB
(define (mw:case exp env)
  `(case ,(mw:expand (cadr exp) env)
     ,@(map (lambda (e) (list (car e) (mw:body (cdr e) env))) (cddr exp))))


;
; Quasiquotation (backquote)           
;
; At level 0, unquoted forms are left painted (not mw:strip'ed).
; At higher levels, forms which are unquoted to level 0 are painted.
; This includes forms within quotes.  E.g.:
;   (lambda (a) 
;     (quasiquote 
;       (a (unquote a) b (quasiquote (a (unquote (unquote a)) b)))))
;or equivalently:
;  (lambda (a) `(a ,a b `(a ,,a b)))
;=>
;  (lambda (a|1) `(a ,a|1 b `(a ,,a|1 b)))

(define (mw:quasiquote exp env)

  (define (mw:atom exp env)
    (if (not (symbol? exp))
	exp
	(let ((denotation (mw:syntax-lookup env exp)))
	  (case (mw:denote-class denotation)
	    ((special macro identifier) (mw:identifier-name denotation))
	    (else (mw:bug "Bug detected by mw:atom" exp env))))
  ) )

  (define (quasi subexp level)
     (cond
	((null? subexp) subexp)
	((not (pair? subexp))
	 (if (zero? level) (mw:atom subexp env) subexp) ; the work is here
	)
	(else
	  (let ( (keyword (mw:syntax-lookup env (car subexp))) )
	    (cond
	      ((eq? keyword mw:denote-of-unquote)
	       (cons 'unquote (quasi (cdr subexp) (- level 1)))
	      )
	      ((eq? keyword mw:denote-of-unquote-splicing)
	       (cons 'unquote-splicing (quasi (cdr subexp) (- level 1)))
	      )
	      ((eq? keyword mw:denote-of-quasiquote)
	       (cons 'quasiquote (quasi (cdr subexp) (+ level 1)))
	      )
	      (else (map (lambda (ex) (quasi ex level)) subexp)
	      )
	    )
	) ) ; end else, let
     ) ; end cond 
  )

  (quasi exp 0) ; need to unquote to level 0 to paint
)

;;                                      --- E O F ---

;;;; Miscellaneous routines.


(define (mw:warn msg . more)
  (if (pair? more)
      (print "*** Warning: " msg ": " (car more))
      (print "*** Warning: " msg)))

(define (mw:error msg . more)
  (if (pair? more)
      (error msg ": " (car more))
      (error msg)))

(define (mw:bug msg . more)
  (apply print "*** BUG in macro expander: " msg ": " more)
  (mw:quit #f))

; Given a <formals>, returns a list of bound variables.

(define (mw:make-null-terminated x)
  (cond ((null? x) '())
	((pair? x)
	 (cons (car x) (mw:make-null-terminated (cdr x))))
	(else (list x))))

; Returns the length of the given list, or -1 if the argument
; is not a list.  Does not check for circular lists.

(define (mw:safe-length x)
  (define (loop x n)
    (cond ((null? x) n)
	  ((pair? x) (loop (cdr x) (+ n 1)))
	  (else -1)))
  (loop x 0))

(define (mw:every pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (or (null? l)
	       (and (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(or (null? l)
		    (and (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

; Given an association list, copies the association pairs.

(define (mw:syntax-copy alist)
  (map (lambda (x) (cons (car x) (cdr x)))
       alist))

;;;; Implementation-dependent parameters and preferences that determine
; how identifiers are represented in the output of the macro expander.
;
; The basic problem is that there are no reserved words, so the
; syntactic keywords of core Scheme that are used to express the
; output need to be represented by data that cannot appear in the
; input.  This file defines those data.

; The following definitions assume that identifiers of mixed case
; cannot appear in the input.

;(define mw:begin1  (string->symbol "Begin"))
;(define mw:define1 (string->symbol "Define"))
;(define mw:quote1  (string->symbol "Quote"))
;(define mw:lambda1 (string->symbol "Lambda"))
;(define mw:if1     (string->symbol "If"))
;(define mw:set!1   (string->symbol "Set!"))

(define mw:begin1  (string->symbol "begin"))
(define mw:define1 (string->symbol "define"))
(define mw:quote1  (string->symbol "quote"))
(define mw:lambda1 (string->symbol "lambda"))
(define mw:if1     (string->symbol "if"))
(define mw:set!1   (string->symbol "set!"))

; The following defines an implementation-dependent expression
; that evaluates to an undefined (not unspecified!) value, for
; use in expanding the (define x) syntax.

(define mw:undefined (list (string->symbol "Undefined")))

; A variable is renamed by suffixing a vertical bar followed by a unique
; integer.  In IEEE and R4RS Scheme, a vertical bar cannot appear as part
; of an identifier, but presumably this is enforced by the reader and not
; by the compiler.  Any other character that cannot appear as part of an
; identifier may be used instead of the vertical bar.

(define mw:suffix-character #\|)

;;;; Syntactic environments.

; A syntactic environment maps identifiers to denotations,
; where a denotation is one of
;
;    (special <special>)
;    (macro <rules> <env>)
;    (identifier <id>)
;
; and where <special> is one of
;
;    quote
;    lambda
;    if
;    set!
;    begin
;    define
;    define-syntax
;    let-syntax
;    letrec-syntax
;    syntax-rules
;
; and where <rules> is a compiled <transformer spec> (see R4RS),
; <env> is a syntactic environment, and <id> is an identifier.

(define mw:standard-syntax-environment
  '((quote         . (special quote))
    (lambda        . (special lambda))
    (if            . (special if))
    (set!          . (special set!))
    (begin         . (special begin))
    (define        . (special define))
    (let           . (special let))                ;; @@ added KAD
    (let*          . (special let*))               ;; @@    "
    (letrec        . (special letrec))             ;; @@    "
    (quasiquote    . (special quasiquote))         ;; @@    "
    (unquote       . (special unquote))            ;; @@    "
    (unquote-splicing . (special unquote-splicing)) ; @@    "
    (do            . (special do))                 ;; @@    "
    (case          . (special case))                 ;; LRB
    (define-syntax . (special define-syntax))
    (let-syntax    . (special let-syntax))
    (letrec-syntax . (special letrec-syntax))
    (syntax-rules  . (special syntax-rules))
    (...           . (identifier ...))
    (:::           . (identifier :::))))

; An unforgeable synonym for lambda, used to expand definitions.

(define mw:lambda0 (string->symbol " lambda "))

; The mw:global-syntax-environment will always be a nonempty
; association list since there is no way to remove the entry
; for mw:lambda0.  That entry is used as a header by destructive
; operations.

(define mw:global-syntax-environment
  (cons (cons mw:lambda0
	      (cdr (assq 'lambda mw:standard-syntax-environment)))
	(mw:syntax-copy mw:standard-syntax-environment)))

(define (mw:global-syntax-environment-set! env)
  (set-cdr! mw:global-syntax-environment env))

(define (mw:syntax-bind-globally! id denotation)
  (if (and (mw:identifier? denotation)
	   (eq? id (mw:identifier-name denotation)))
      (letrec ((remove-bindings-for-id
		(lambda (bindings)
		  (cond ((null? bindings) '())
			((eq? (caar bindings) id)
			 (remove-bindings-for-id (cdr bindings)))
			(else
			 (cons (car bindings)
			       (remove-bindings-for-id (cdr bindings))))))))
	(mw:global-syntax-environment-set!
	 (remove-bindings-for-id (cdr mw:global-syntax-environment))))
      (let ((x (assq id mw:global-syntax-environment)))
	(if x
	    (set-cdr! x denotation)
	    (mw:global-syntax-environment-set!
	     (cons (cons id denotation)
		   (cdr mw:global-syntax-environment)))))))

(define (mw:syntax-divert env1 env2)
  (append env2 env1))

(define (mw:syntax-extend env ids denotations)
  (mw:syntax-divert env (map cons ids denotations)))

(define (mw:syntax-lookup-raw env id)
  (let ((entry (assq id env)))
    (if entry
	(cdr entry)
	#f)))

(define (mw:syntax-lookup env id)
  (or (mw:syntax-lookup-raw env id)
      (mw:make-identifier-denotation id)))

(define (mw:syntax-assign! env id denotation)
  (let ((entry (assq id env)))
    (if entry
	(set-cdr! entry denotation)
	(mw:bug "Bug detected in mw:syntax-assign!" env id denotation))))

(define mw:denote-of-quote
  (mw:syntax-lookup mw:standard-syntax-environment 'quote))

(define mw:denote-of-lambda
  (mw:syntax-lookup mw:standard-syntax-environment 'lambda))

(define mw:denote-of-if
  (mw:syntax-lookup mw:standard-syntax-environment 'if))

(define mw:denote-of-set!
  (mw:syntax-lookup mw:standard-syntax-environment 'set!))

(define mw:denote-of-begin
  (mw:syntax-lookup mw:standard-syntax-environment 'begin))

(define mw:denote-of-define
  (mw:syntax-lookup mw:standard-syntax-environment 'define))

(define mw:denote-of-define-syntax
  (mw:syntax-lookup mw:standard-syntax-environment 'define-syntax))

(define mw:denote-of-let-syntax
  (mw:syntax-lookup mw:standard-syntax-environment 'let-syntax))

(define mw:denote-of-letrec-syntax
  (mw:syntax-lookup mw:standard-syntax-environment 'letrec-syntax))

(define mw:denote-of-syntax-rules
  (mw:syntax-lookup mw:standard-syntax-environment 'syntax-rules))

(define mw:denote-of-...
  (mw:syntax-lookup mw:standard-syntax-environment '...))

(define mw:denote-of-:::
  (mw:syntax-lookup mw:standard-syntax-environment ':::))

(define mw:denote-of-let
  (mw:syntax-lookup mw:standard-syntax-environment 'let))        ;; @@ KenD

(define mw:denote-of-let*
  (mw:syntax-lookup mw:standard-syntax-environment 'let*))       ;; @@ KenD

(define mw:denote-of-letrec
  (mw:syntax-lookup mw:standard-syntax-environment 'letrec))     ;; @@ KenD

(define mw:denote-of-quasiquote
  (mw:syntax-lookup mw:standard-syntax-environment 'quasiquote)) ;; @@ KenD

(define mw:denote-of-unquote
  (mw:syntax-lookup mw:standard-syntax-environment 'unquote))    ;; @@ KenD

(define mw:denote-of-unquote-splicing
  (mw:syntax-lookup mw:standard-syntax-environment 'unquote-splicing)) ;@@ KenD

(define mw:denote-of-do
  (mw:syntax-lookup mw:standard-syntax-environment 'do))        ;; @@ KenD

(define mw:denote-of-case
  (mw:syntax-lookup mw:standard-syntax-environment 'case))        ;; LRB

(define mw:denote-class car)

(define (mw:identifier? denotation)
  (eq? (mw:denote-class denotation) 'identifier))

(define (mw:make-identifier-denotation id)
  (list 'identifier id))

(define macwork:rules cadr)
(define macwork:env caddr)
(define mw:identifier-name cadr)

(define (mw:same-denotation? d1 d2)
  (or (eq? d1 d2)
      (and (mw:identifier? d1)
	   (mw:identifier? d2)
	   (eq? (mw:identifier-name d1)
		(mw:identifier-name d2)))))

; Renaming of variables.

; Given a datum, strips the suffixes from any symbols that appear within
; the datum, trying not to copy any more of the datum than necessary.
; Well, right now I'm just copying the datum, but I need to fix that!

(define (mw:strip x)
  (cond ((symbol? x)
	 (let ((chars (memv mw:suffix-character
			    (reverse (string->list
				      (symbol->string x))))))
	   (if chars
	       (string->symbol
		(list->string (reverse (cdr chars))))
	       x)))
	((pair? x)
	 (cons (mw:strip (car x))
	       (mw:strip (cdr x))))
	((vector? x)
	 (list->vector (map mw:strip (vector->list x))))
	(else x)))

; Given a list of identifiers, returns an alist that associates each
; identifier with a fresh identifier.

(define (mw:rename-vars vars)
  (set! mw:renaming-counter (+ mw:renaming-counter 1))
  (let ((suffix (string-append (string mw:suffix-character)
			       (number->string mw:renaming-counter))))
    (map (lambda (var)
	   (if (symbol? var)
	       (cons var
		     (string->symbol
		      (string-append (symbol->string var) suffix)))
	       (error "Illegal variable" var)))
	 vars)))

; Given a syntactic environment env to be extended, an alist returned
; by mw:rename-vars, and a syntactic environment env2, extends env by
; binding the fresh identifiers to the denotations of the original
; identifiers in env2.

(define (mw:syntax-alias env alist env2)
  (mw:syntax-divert
   env
   (map (lambda (name-pair)
	  (let ((old-name (car name-pair))
		(new-name (cdr name-pair)))
	    (cons new-name
		  (mw:syntax-lookup env2 old-name))))
	alist)))

; Given a syntactic environment and an alist returned by mw:rename-vars,
; extends the environment by binding the old identifiers to the fresh
; identifiers.

(define (mw:syntax-rename env alist)
  (mw:syntax-divert env
		    (map (lambda (old new)
			   (cons old (mw:make-identifier-denotation new)))
			 (map car alist)
			 (map cdr alist))))

; Given a <formals> and an alist returned by mw:rename-vars that contains
; a new name for each formal identifier in <formals>, renames the
; formal identifiers.

(define (mw:rename-formals formals alist)
  (cond ((null? formals) '())
	((pair? formals)
	 (cons (cdr (assq (car formals) alist))
	       (mw:rename-formals (cdr formals) alist)))
	(else (cdr (assq formals alist)))))

(define mw:renaming-counter 0)

;;;; Compiler for a <transformer spec>.

;;; The input is a <transformer spec> and a syntactic environment.
;;; Syntactic environments are described in another file.

(define mw:pattern-variable-flag (list 'v))
(define mw:ellipsis-pattern-flag (list 'e))
(define mw:ellipsis-template-flag mw:ellipsis-pattern-flag)

(define (mw:make-patternvar v rank)
  (vector mw:pattern-variable-flag v rank))
(define (mw:make-ellipsis-pattern P vars)
  (vector mw:ellipsis-pattern-flag P vars))
(define (mw:make-ellipsis-template T vars)
  (vector mw:ellipsis-template-flag T vars))

(define (mw:patternvar? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) mw:pattern-variable-flag)))

(define (mw:ellipsis-pattern? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) mw:ellipsis-pattern-flag)))

(define (mw:ellipsis-template? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) mw:ellipsis-template-flag)))

(define (mw:patternvar-name V) (vector-ref V 1))
(define (mw:patternvar-rank V) (vector-ref V 2))
(define (mw:ellipsis-pattern P) (vector-ref P 1))
(define (mw:ellipsis-pattern-vars P) (vector-ref P 2))
(define (mw:ellipsis-template T) (vector-ref T 1))
(define (mw:ellipsis-template-vars T) (vector-ref T 2))

(define (mw:pattern-variable v vars)
  (cond ((null? vars) #f)
	((eq? v (mw:patternvar-name (car vars)))
	 (car vars))
	(else (mw:pattern-variable v (cdr vars)))))

; Given a <transformer spec> and a syntactic environment,
; returns a macro denotation.
;
; A macro denotation is of the form
;
;    (macro (<rule> ...) env)
;
; where each <rule> has been compiled as described above.

(define (mw:compile-transformer-spec spec env)
  (if (and (> (mw:safe-length spec) 1)
	   (eq? (mw:syntax-lookup env (car spec))
		mw:denote-of-syntax-rules))
      (let ((literals (cadr spec))
	    (rules (cddr spec)))
	(if (or (not (list? literals))
		(not (mw:every (lambda (rule)
			      (and (= (mw:safe-length rule) 2)
				   (pair? (car rule))))
			      rules)))
	    (mw:error "Malformed syntax-rules" spec))
	(list 'macro
	      (map (lambda (rule)
		     (mw:compile-rule rule literals env))
		   rules)
	      env))
      (mw:error "Malformed syntax-rules" spec)))

(define (mw:compile-rule rule literals env)
  (mw:compile-pattern (cdr (car rule))
		     literals
		     env
		     (lambda (compiled-rule patternvars)
		       ; should check uniqueness of pattern variables here!!!!!
		       (cons compiled-rule
			     (mw:compile-template
			      (cadr rule)
			      patternvars
			      env)))))

(define (identity x) x)

(define (adjoin e l) (if (memq e l) l (cons e l)))

(define (union l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else (union (cdr l1) (adjoin (car l1) l2)))))

(define (remove-if-not p l)
  (cond ((null? l) l)
	((p (car l)) (cons (car l) (remove-if-not p (cdr l))))
	(else (remove-if-not p (cdr l)))))

(define (mw:compile-pattern P literals env k)
  (define (loop P vars rank k)
    (cond ((symbol? P)
	   (if (memq P literals)
	       (k P vars)
	       (let ((var (mw:make-patternvar P rank)))
		 (k var (cons var vars)))))
	  ((null? P) (k '() vars))
	  ((pair? P)
	   (if (and (pair? (cdr P))
		    (symbol? (cadr P))
		    (eq? (mw:syntax-lookup env (cadr P))
			 mw:denote-of-...))
	       (if (null? (cddr P))
		   (loop (car P)
			 '()
			 (+ rank 1)
			 (lambda (P vars1)
			   (k (mw:make-ellipsis-pattern P vars1)
			      (union vars1 vars))))
		   (mw:error "Malformed pattern" P))
	       (loop (car P)
		     vars
		     rank
		     (lambda (P1 vars)
		       (loop (cdr P)
			     vars
			     rank
			     (lambda (P2 vars)
			       (k (cons P1 P2) vars)))))))
	  ((vector? P)
	   (loop (vector->list P)
		 vars
		 rank
		 (lambda (P vars)
		   (k (vector P) vars))))
	  (else (k P vars))))
  (loop P '() 0 k))

(define (mw:compile-template T vars env)
  
  (define (loop T inserted referenced rank escaped? k)
    (cond ((symbol? T)
	   (let ((x (mw:pattern-variable T vars)))
	     (if x
		 (if (>= rank (mw:patternvar-rank x))
		     (k x inserted (cons x referenced))
		     (mw:error
		      "Too few ellipses follow pattern variable in template"
		      (mw:patternvar-name x)))
		 (k T (cons T inserted) referenced))))
	  ((null? T) (k '() inserted referenced))
	  ((pair? T)
	   (cond ((and (not escaped?)
		       (symbol? (car T))
		       (eq? (mw:syntax-lookup env (car T))
			    mw:denote-of-:::)
		       (pair? (cdr T))
		       (null? (cddr T)))
		  (loop (cadr T) inserted referenced rank #t k))
		 ((and (not escaped?)
		       (pair? (cdr T))
		       (symbol? (cadr T))
		       (eq? (mw:syntax-lookup env (cadr T))
			    mw:denote-of-...))
		  (loop1 T inserted referenced rank escaped? k))
		 (else
		  (loop (car T)
			inserted
			referenced
			rank
			escaped?
			(lambda (T1 inserted referenced)
			  (loop (cdr T)
				inserted
				referenced
				rank
				escaped?
				(lambda (T2 inserted referenced)
				  (k (cons T1 T2) inserted referenced))))))))
	  ((vector? T)
	   (loop (vector->list T)
		 inserted
		 referenced
		 rank
		 escaped?
		 (lambda (T inserted referenced)
		   (k (vector T) inserted referenced))))
	  (else (k T inserted referenced))))
  
  (define (loop1 T inserted referenced rank escaped? k)
    (loop (car T)
	  inserted
	  '()
	  (+ rank 1)
	  escaped?
	  (lambda (T1 inserted referenced1)
	    (loop (cddr T)
		  inserted
		  (append referenced1 referenced)
		  rank
		  escaped?
		  (lambda (T2 inserted referenced)
		    (k (cons (mw:make-ellipsis-template
			      T1
			      (remove-if-not (lambda (var)
					       (> (mw:patternvar-rank var)
						  rank))
					     referenced1))
			     T2)
		       inserted
		       referenced))))))
  
  (loop T
	'()
	'()
	0
	#f
	(lambda (T inserted referenced)
	  (list T inserted))))

; The pattern matcher.
;
; Given an input, a pattern, and two syntactic environments,
; returns a pattern variable environment (represented as an alist)
; if the input matches the pattern, otherwise returns #f.

(define mw:empty-pattern-variable-environment
  (list (mw:make-patternvar (string->symbol "") 0)))

(define (mw:match F P env-def env-use)
  
  (define (match F P answer rank)
    (cond ((null? P)
	   (and (null? F) answer))
	  ((pair? P)
	   (and (pair? F)
		(let ((answer (match (car F) (car P) answer rank)))
		  (and answer (match (cdr F) (cdr P) answer rank)))))
	  ((symbol? P)
	   (and (symbol? F)
		(mw:same-denotation? (mw:syntax-lookup env-def P)
				     (mw:syntax-lookup env-use F))
		answer))
	  ((mw:patternvar? P)
	   (cons (cons P F) answer))
	  ((mw:ellipsis-pattern? P)
	   (match1 F P answer (+ rank 1)))
	  ((vector? P)
	   (and (vector? F)
		(match (vector->list F) (vector-ref P 0) answer rank)))
	  (else (and (equal? F P) answer))))
  
  (define (match1 F P answer rank)
    (cond ((not (list? F)) #f)
	  ((null? F)
	   (append (map (lambda (var) (cons var '()))
			(mw:ellipsis-pattern-vars P))
		   answer))
	  (else
	   (let* ((P1 (mw:ellipsis-pattern P))
		  (answers (map (lambda (F) (match F P1 answer rank))
				F)))
	     (if (mw:every identity answers)
		 (append (map (lambda (var)
				(cons var
				      (map (lambda (answer)
					     (cdr (assq var answer)))
					   answers)))
			      (mw:ellipsis-pattern-vars P))
			 answer)
		 #f)))))
  
  (match F P mw:empty-pattern-variable-environment 0))

(define (mw:rewrite T alist)
  
  (define (rewrite T alist rank)
    (cond ((null? T) '())
	  ((pair? T)
	   ((if (mw:ellipsis-pattern? (car T))
		append
		cons)
	    (rewrite (car T) alist rank)
	    (rewrite (cdr T) alist rank)))
	  ((symbol? T) (cdr (assq T alist)))
	  ((mw:patternvar? T) (cdr (assq T alist)))
	  ((mw:ellipsis-template? T)
	   (rewrite1 T alist (+ rank 1)))
	  ((vector? T)
	   (list->vector (rewrite (vector-ref T 0) alist rank)))
	  (else T)))
  
  (define (rewrite1 T alist rank)
    (let* ((T1 (mw:ellipsis-template T))
	   (vars (mw:ellipsis-template-vars T))
	   (rows (map (lambda (var) (cdr (assq var alist)))
		      vars)))
      (map (lambda (alist) (rewrite T1 alist rank))
	   (make-columns vars rows alist))))
  
  (define (make-columns vars rows alist)
    (define (loop rows)
      (if (null? (car rows))
	  '()
	  (cons (append (map (lambda (var row)
			       (cons var (car row)))
			     vars
			     rows)
			alist)
		(loop (map cdr rows)))))
    (if (or (null? (cdr rows))
	    (apply = (map length rows)))
	(loop rows)
	(mw:error "Use of macro is not consistent with definition"
		 vars
		 rows)))
  
  (rewrite T alist 0))

; Given a use of a macro, the syntactic environment of the use,
; and a continuation that expects a transcribed expression and
; a new environment in which to continue expansion,
; does the right thing.

(define (mw:transcribe exp env-use k)
  (let* ((m (mw:syntax-lookup env-use (car exp)))
	 (rules (macwork:rules m))
	 (env-def (macwork:env m))
	 (F (cdr exp)))
    (define (loop rules)
      (if (null? rules)
	  (mw:error "Use of macro does not match definition" exp)
	  (let* ((rule (car rules))
		 (pattern (car rule))
		 (alist (mw:match F pattern env-def env-use)))
	    (if alist
		(let* ((template (cadr rule))
		       (inserted (caddr rule))
		       (alist2 (mw:rename-vars inserted))
		       (newexp (mw:rewrite template (append alist2 alist))))
		  (k newexp
		     (mw:syntax-alias env-use alist2 env-def)))
		(loop (cdr rules))))))
    (loop rules)))


(define (complete:expand expr)
  (system:macroexpand (macwork:macroexpand expr)))
(set! system:*macroexpand-hook* complete:expand)

(provide 'macros)
