(require-library syslog)
(import syslog)

(define (handle! request)
  ;; (syslog prio/warning "~a" (make-request-output-port 2))
  (display "Hello, Apache!")
  (display "Hello, Apache logs!" (current-error-port))
  ok)
