(define sock (system:connect "freeside"))

(system:write-string "GET / HTTP/1.0\r\n\r\n" sock)

(let loop ((c (read-char sock)))
    (if (not (eof-object? c))
        (begin
            (write-char c)
            (loop (read-char sock)) )))

(close-input-port sock)
