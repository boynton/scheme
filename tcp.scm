(provide 'tcp)

(define tcp-listen system:listen)
(define tcp-accept system:accept)
(define (read-line port)
  (cdr (system:read-line port)))
(define (close-socket-port sock) #t)
(define (write-line s port)
  (display s port)
  (newline port))
(define object->string system:object->string)
(define close-port system:socket-close)


