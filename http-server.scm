;; Single-threaded, generic HTTP server for MIT Scheme.
;; Author: Stephen Brennan

(load "tcp-server.scm")

(define http-handler
  (lambda (port)
    (display (read-message port))
    (write-string "HTTP/1.0 404 Not found\n\nNot found." port)
    (flush-output port)
    (close-output-port port)))
