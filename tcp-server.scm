;; Single-threaded, generic TCP server for MIT Scheme.
;; Author: Stephen Brennan

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading Messages from Input Ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How messages should end.
(define end-message "\n\n")

;; Read character, append to string, and continue until the message ends with
;; the end-message sequence.
(define read-message-helper
  (lambda (input-port message)
    (if (string-suffix? end-message message)
        message
        (read-message-helper input-port
                             (string-append message
                                            (string (read-char input-port)))))))

;; Read a message (terminated by end-message) from an input port.
(define read-message
  (lambda (input-port)
    (read-message-helper input-port "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define handle-requests
  (lambda (server-socket handler)
    (handler (tcp-server-connection-accept server-socket #t #f))
    (handle-requests server-socket handler)))

(define run-server
  (lambda (socket handler)
    (let* ((server-socket (open-tcp-server-socket socket))
           (rval (handle-requests server-socket handler)))
      (close-tcp-server-socket server-socket)
      rval)))
