
(define *serial-port* (system:open-serial-output "COM2" 38400))

(define (note-on k v)
    (print 'ding)
    (system:write-fixnums *serial-port* 144 k v))
(define (note-off k)
    (system:write-fixnums *serial-port* 128 k 0))
(define (rest n)
    (system:pause n))


(note-on 32 100)
(rest 500)
(note-off 32)
(rest 500)

(note-on 32 100)
(rest 500)
(note-off 32)

(note-on 32 100)
(rest 500)
(note-off 32)

(close-output-port *serial-port*)
