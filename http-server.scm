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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Request Functions
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
    (letrec
        ((req-header
          (lambda (hlist)
            (cond
             ((null? hlist) 'not-found)
             ((string=? header (caar hlist)) (cadar hlist))
             (else (req-header (cdr hlist)))))))
      (req-header (req-headers request)))))

;; TCP Server Handler for HTTP.  Reads a single HTTP request from the input
;; port, parses it, and writes a 404 response.
(define http-handler
  (lambda (port)
    (let ((request (http-request-parse (read-message port))))
      (display request)
      (write-string "HTTP/1.0 404 Not found\n\nNot found." port)
      (flush-output port)
      (close-output-port port)
      request)))
