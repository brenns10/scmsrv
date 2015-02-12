;; Single-threaded, generic HTTP server for MIT Scheme.
;; Author: Stephen Brennan

(load "tcp-server.scm")

;; string-split: Splits a string on a separator.  That is, returns all non-empty
;; strings delimited by the separator. EG:
;; (string-split #\space "fragment another   the_third")
;; => '("frament" "another" "the_third")
(define string-split
  (lambda (sep str)
    ((lambda (f a b) (f f a b))
      (lambda (self start end)
        (cond
         ((>= start (string-length str)) '())
         ((eq? sep (string-ref str start)) (self self (+ start 1) (+ start 2)))
         ((>= end (string-length str)) (cons (substring str start end) '()))
         ((eq? sep (string-ref str end)) (cons (substring str start end) (self self (+ end 1) (+ end 2))))
         (else (self self start (+ end 1))))) 0 1)))

(define parse-headers
  (lambda (line-list)
    (if (null? line-list)
        '()
        (cons (map string-trim (string-split #\: (car line-list)))
              (parse-headers (cdr line-list))))))

(define HTTP-METHODS '("OPTIONS" "GET" "HEAD" "POST" "PUT" "DELETE" "TRACE" "CONNECT"))
(define verify-request-fields
  (lambda (request-fields)
    (cond
     ((not (= 3 (length request-fields)))
      (error "Incorrect number of fields!"))
     ((not (member (car request-fields) HTTP-METHODS))
      (error "Unrecognized method."))
     (else request-fields))))

;; http-request-parse: Take an HTTP request and parse it into a http-request
;; object, which can be accessed with the hr-* functions.
(define http-request-parse
  (lambda (message)
    (let ((lines (string-split #\newline message)))
      (append (verify-request-fields (string-split #\space (car lines)))
              (parse-headers (cdr lines))))))

(define hr-method car)
(define hr-url cadr)
(define hr-version caddr)
(define hr-headers cdddr)
(define hr-header
  (lambda (request header)
    ((lambda (f l) (f f l))
     (lambda (self hlist)
       (cond
        ((null? hlist) 'not-found)
        ((string=? header (caar hlist)) (cdar hlist))
        (else (self self (cdr hlist)))))
     (hr-headers request))))

(define http-handler
  (lambda (port)
    (let ((request (http-request-parse (read-message port))))
      (display request)
      (write-string "HTTP/1.0 404 Not found\n\nNot found." port)
      (flush-output port)
      (close-output-port port)
      request)))
