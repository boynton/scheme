;; LeeScheme/autoload.scm. Copyright (C) Lee Richard Boynton, 1993-2000.

(require 'macros)

(define-syntax autoload
  (syntax-rules (define)
    ((autoload fun)
     (define (fun . rest) (load 'fun) (apply fun rest)))
    ((autoload fun file)
     (define (fun . rest) (load 'file) (apply fun rest)))))

(autoload apropos)
(autoload apropos-list apropos)
(autoload pretty)
(autoload sort)

(provide 'autoload)
