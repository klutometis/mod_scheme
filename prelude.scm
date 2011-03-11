(include "srclib/environments/environments.scm")
(import extras ports srfi-13 environments defstruct foreigners)
(foreign-declare "#include <httpd.h>")
;;; test this against just sending it all along.

(defstruct
  request
  header-only
  ;; protocol
  ;; proto-num
  ;; hostname
  ;; request-time
  ;; status-line
  ;; status
  ;; method
  ;; method-number
  ;; chunked
  ;; content-type
  ;; handler
  ;; content-encoding
  ;; user
  ;; ap-auth-type
  ;; uri
  ;; filename
  ;; path-info
  ;; args
  )

(define-foreign-record-type
  (request_rec request_rec)
  #;((c-pointer "apr_pool_t") request-pool)
  (int header_only request-rec-header-only))

(define (make-request-output-port request*)
  (let ((buffer ""))
    (make-output-port
     (lambda (scribendum)
       (set! buffer (string-append/shared buffer scribendum)))
     (lambda () (set! buffer ""))
     (lambda () ((foreign-lambda int
                                 "ap_rputs"
                                 c-string
                                 (c-pointer "request_rec"))
                 buffer
                 request*)))))

(define-external
  (mod_scheme_handle (c-string file) (c-pointer request*))
  int
  (let ((environment
         (environment-copy #;(scheme-report-environment 5)
          (interaction-environment)
          #t)))
    (load file (lambda (expression) (eval expression environment)))
    (eval `(handle! (make-request ,(request-rec-header-only request*)))
          environment)))

(return-to-host)
