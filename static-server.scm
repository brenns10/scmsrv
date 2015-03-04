;; Static web server plugin to HTTP server.
;; Author: Stephen Brennan

(load "http-server.scm")

(define ensure-trailing/
  (lambda (s)
    (if (string-suffix? "/" s)
        s
        (substring s 0 (- (string-length s) 1)))))

(define ensure-no-starting/
  (lambda (s)
    (if (string-prefix? "/" s)
        (substring s 1 (string-length s))
        s)))

(define path-join
  (lambda (s1 s2)
    (string-append (ensure-trailing/ s1) (ensure-no-starting/ s2))))

(define index
  (lambda (f)
    (if (not (file-executable? f))
        http-403
        (resp-new 200 '() (body-text (string-join "\n" (map ->namestring (ensure-trailing/ f))))))))

(define static-handler
  (lambda (root)
    (lambda (request)
      (let ((f (path-join root (req-url request))))
        (cond
         ((not (file-exists? f)) http-404)
         ((file-directory? f) (index f))
         ((not (file-readable? f)) http-403)
         (else (resp-new 200 '() (body-file f))))))))


(define run-static-server
  (lambda (port root)
    (run-server port (http-handler (static-handler root)))))
