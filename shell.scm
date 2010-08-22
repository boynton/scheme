;
; shell.scm - common shell commands.
;


(define (as-string obj)
  (cond
   ((string? obj) obj)
   ((symbol? obj) (symbol->string obj))
   ((number? obj) (number->string obj))
   (t " <non-string>")))

(define (shell-string-arg obj)
  (string-append " " (as-string obj)))

(define (ls . args)
  (let ((s (apply string-append "ls -F" (map shell-string-arg args))))
	(system:command s)))

(define (pwd)
  (print (system:pwd)))

(define (cd . path)
  (if (pair? path)
	  (system:chdir (as-string (car path)))
	  (system:chdir (system:home-directory))))

(define (cp . args)
  (system:command (apply string-append "cp" (map shell-string-arg args))))

(define (mv . args)
  (system:command (apply string-append "mv" (map shell-string-arg args))))

(define (rm . args)
  (system:command (apply string-append "rm" (map shell-string-arg args))))

(define (mkdir . args)
  (system:command (apply string-append "mkdir" (map shell-string-arg args))))

(define (rmdir . args)
  (system:command (apply string-append "rmdir" (map shell-string-arg args))))

(define (less . args)
  (system:command (apply string-append "less" (map shell-string-arg args))))

(define (emacs . args)
  (system:command (apply string-append "emacs" (map shell-string-arg args))))

(define e emacs)
(define l ls)
(define (date)
  (system:command "date"))

(define (grep . args)
  (system:command (apply string-append "grep" (map shell-string-arg args))))



