;
;config.scm - This optional file gets loaded just before the repl runs.
;

;(set! system:*search-path* (cons "~/music/scheme" system:*search-path*))

(require 'macros)
(require 'autoload)

(set! system:*prompt* "scheme> ")
(set! system:*result-prompt* "= ")

(set! system:*load-verbose* #t)
;(set! system:*show-compiled-definitions* #t)

(system:top-level-command! apropos)


