;; Single-threaded, generic HTTP server for MIT Scheme.
;; Author: Stephen Brennan

(load "tcp-server.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string-split-lim: Returns list of all non-empty strings in str in between
;; separator character sep.  Limits to lim strings.  If lim is 0, there is no
;; limit (you can also use string-split for this).  Example:
;; (string-split-lim #\space "fragment  another the_third and_more" 3)
;; => '("fragment" "another" "the_third and_more")
(define string-split-lim
  (lambda (sep str lim)
    (letrec
        ((string-split
          (lambda (start end lim)
            (cond
             ((>= start (string-length str)) '())
             ((zero? lim) (list (substring str start (string-length str))))
             ((eq? sep (string-ref str start))
              (string-split (+ start 1) (+ start 2) lim))
             ((>= end (string-length str)) (list (substring str start end)))
             ((eq? sep (string-ref str end))
              (cons (substring str start end) (string-split (+ end 1)
                                                            (+ end 2)
                                                            (- lim 1))))
             (else (string-split start (+ end 1) lim))))))
      (string-split 0 1 (- lim 1)))))

(define string-split
  (lambda (sep str)
    (string-split-lim sep str 0)))

;; parse-headers: Takes a list of header lines and returns a list of (header
;; value) pairs.
;; (parse-headers '("Host: google.com" "Cookie: chocolate chip"))
;; => '(("Host" "google.com") ("Cookie" "chocolate chip"))
(define parse-headers
  (lambda (line-list)
    (if (null? line-list)
        '()
        (cons (map string-trim (string-split-lim #\: (car line-list) 2))
              (parse-headers (cdr line-list))))))

;; Take a header pair and return a string representing it.
(define header->string
  (lambda (header-pair)
    (string-append (caar header-list) ": " (cadar header-list))))

;; Join a list of strings by a separator.
(define string-join
  (lambda (sep strs)
    (cond
     ((null? strs) "")
     ((null? (cdr strs)) (car strs))
     (else (string-append (car strs) sep (string-join sep (cdr strs)))))))

;; Take a list of headers and return a string of headers.
(define headers->string
  (lambda (header-list)
    (string-join "\n" (map header->string header-list))))

;; Get header value from list
(define get-header
  (lambda (hlist header)
    (cond
     ((null? hlist) 'not-found)
     ((string=? header (caar hlist)) (cadar hlist))
     (else (get-header (cdr hlist))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Request Functions
;;
;; A HTTP request object is a list:
;; (METHOD URL VERSION (HEADER VALUE) (HEADER VALUE) ... )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List of HTTP methods from HTTP/1.1 spec.
(define HTTP-METHODS '("OPTIONS" "GET" "HEAD" "POST"
                       "PUT" "DELETE" "TRACE" "CONNECT"))

;; verify-request-fields: Take a list of request fields (method url version) and
;; make sure they are valid.  Return them if valid, or raise error if false.
(define verify-request-fields
  (lambda (request-fields)
    (cond
     ((not (= 3 (length request-fields)))
      (error "Incorrect number of fields!"))
     ((not (member (car request-fields) HTTP-METHODS))
      (error "Unrecognized method."))
     (else request-fields))))

;; http-request-parse: Take an HTTP request and parse it into a http-request
;; object, which can be accessed with the req-* functions.
(define http-request-parse
  (lambda (message)
    (let ((lines (string-split #\newline message)))
      (append (verify-request-fields (string-split #\space (car lines)))
              (parse-headers (cdr lines))))))

;; Return the method of a http-request object.
(define req-method car)
;; Return the URL of a http-request object.
(define req-url cadr)
;; Return the HTTP version of a http-request object.
(define req-version caddr)
;; Return the header list of an http-request object.
(define req-headers cdddr)

;; Lookup a header in an http-request object.
(define req-header
  (lambda (request header)
      (get-header (req-headers request) header)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Response Functions
;;
;; A HTTP Response is a list:
;; (CODE (HEADER_PAIRS ...) BODY_FUNCTION)
;; BODY_FUNCTION is a function which takes an output port and writes the
;; response to the output port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the reason string for an HTTP response code.
(define http-response-reason
  (lambda (code)
    (cond
     ((= code 100) "Continue")
     ((= code 101) "Switching Protocols")
     ((= code 200) "OK")
     ((= code 201) "Created")
     ((= code 202) "Accepted")
     ((= code 203) "Non-Authoritative Information")
     ((= code 204) "No Content")
     ((= code 205) "Reset Content")
     ((= code 206) "Partial Content")
     ((= code 300) "Multiple Choices")
     ((= code 301) "Moved Permanently")
     ((= code 302) "Found")
     ((= code 303) "See Other")
     ((= code 304) "Not Modified")
     ((= code 305) "Use Proxy")
     ((= code 307) "Temporary Redirect")
     ((= code 400) "Bad Request")
     ((= code 401) "Unauthorized")
     ((= code 402) "Payment Required")
     ((= code 403) "Forbidden")
     ((= code 404) "Not Found")
     ((= code 405) "Method Not Allowed")
     ((= code 406) "Not Acceptable")
     ((= code 407) "Proxy Authentication Required")
     ((= code 408) "Request Time-out")
     ((= code 409) "Conflict")
     ((= code 410) "Gone")
     ((= code 411) "Length Required")
     ((= code 412) "Precondition Failed")
     ((= code 413) "Request Entity Too Large")
     ((= code 414) "Request-URI Too Large")
     ((= code 415) "Unsupported Media Type")
     ((= code 416) "Requested Range not satisfiable")
     ((= code 417) "Expectation Failed")
     ((= code 500) "Internal Server Error")
     ((= code 501) "Not Implemented")
     ((= code 502) "Bad Gateway")
     ((= code 503) "Service Unavailable")
     ((= code 504) "Gateway Time-out")
     ((= code 505) "HTTP Version not supported")
     (else "Reason Unknown"))))

;; Return a response body function that prints a string to the output port.
(define body-text
  (lambda (string)
    (lambda (port)
      (write-string string port))))

;; Write characters from in to out until in ends.
(define send-io
  (lambda (in out unspec)
    (let ((chr (read-char in)))
      (if (eof-object? chr)
          ""
          (send-io in out (write-char chr out))))))

;; Return a response body function that sends a file to output.
(define body-file
  (lambda (filename)
    (lambda (port)
      (call-with-input-file filename
        (lambda (input)
          (send-io input port #f))))))

;; Create a response
(define resp-new
  (lambda (code headers body)
    (list code headers body)))

;; Get response parts.
(define resp-code car)
(define resp-headers cadr)
(define resp-body caddr)

;; Add headers to a response.
(define resp-add-header
  (lambda (response header value)
    (http-response-new (resp-code response)
                       (cons (list header value) (resp-headers response))
                       (resp-body response))))

;; Get a header from a response.
(define resp-header
  (lambda (response header)
    (get-header (resp-headers response) header)))

;; Write a response to the port.
(define resp-write
  (lambda (response port)
    (write-string
     (string-append "HTTP/1.1 "
                    (number->string (resp-code response)) " "
                    (http-response-reason (resp-code response)) "\n"
                    (if (not (null? (resp-headers response)))
                        (string-append  (headers->string (resp-headers response))
                                        "\n\n")
                        "\n"))
     port)
    ((resp-body response) port)))

;; 404 response constant.  It's useful, I promise.
(define http-404 (resp-new 404 '() (body-text "Not found.")))
(define http-403 (resp-new 403 '() (body-text "Forbidden.")))

;; TCP Server Handler for HTTP.  Reads a single HTTP request from the input
;; port, parses it, and writes out this file as a response.
(define http-handler
  (lambda (request-handler)
    (lambda (port)
      (let* ((request (http-request-parse (read-message port)))
             (response (request-handler request)))
        (display request)
        (newline)
        (resp-write response port)
        (flush-output port)
        (close-output-port port)
        (display response)
        (newline)
        request))))
