;; LeeScheme/apropos.scm. Copyright (C) Lee Richard Boynton, 1993-2000.

(provide 'apropos)
(require 'sort)

(define substring-index system:substring-index)

(define (apropos-list . keywords)
  (define (apropos-helper symlist str)
    (define (apropos-helper-helper symlist result)
      (if (null? symlist)
	  result
	  (if (substring-index (symbol->string (car symlist)) str)
	      (apropos-helper-helper (cdr symlist)
				     (cons (car symlist) result))
	      (apropos-helper-helper (cdr symlist) result))))
    (apropos-helper-helper symlist '()))
  (let ((result (system:globals)) (kw keywords))
    (if (pair? keywords)
	(let loop ((sym (car kw)))	  
	  (let ((str (if (string? sym) sym (symbol->string sym))))
	    (set! result (apropos-helper result str)))
	  (set! kw (cdr kw))
	  (if (pair? kw) (loop (car kw)))))
    result))

(define (apropos . keywords)
  (define (apropos-sorter x y)
    (string<? (symbol->string x) (symbol->string y)))
  (newline)
  (let loop ((result (sort (apply apropos-list keywords) apropos-sorter)))
    (if (pair? result)
	(begin
	  (display (car result))
	  (newline)
	  (loop (cdr result)))))
  (newline))
    
