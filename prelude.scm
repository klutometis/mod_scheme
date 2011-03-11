(include "srclib/environments/environments.scm")
(import extras ports srfi-13 environments defstruct foreigners)
(foreign-declare "#include <httpd.h>")
(foreign-declare "#include <http_log.h>")
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

(define (puts string request-rec*)
  ((foreign-lambda int
                   "ap_rputs"
                   c-string
                   (c-pointer "request_rec"))
   string
   request-rec*))

(define (log-error request-rec* string)
  ((foreign-lambda void
                   "ap_log_rerror"
                   c-string
                   int
                   int
                   int
                   (c-pointer "request_rec")
                   c-string)
   ;; something more instructive, perhaps?
   (foreign-value "__FILE__" c-string)
   (foreign-value "__LINE__" int)
   (foreign-value "LOG_DEBUG" int)
   (foreign-value "OK" int)
   request-rec*
   string))

(define (make-request-output-port request-rec*)
  (let ((buffer ""))
    (make-output-port
     (lambda (scribendum)
       (set! buffer (string-append/shared buffer scribendum)))
     (lambda () (set! buffer ""))
     (lambda () (rputs buffer request-rec*)))))

(define-external
  (mod_scheme_handle (c-string file) (c-pointer request-rec*))
  int
  (let ((environment
         (environment-copy #;(scheme-report-environment 5)
          (interaction-environment)
          #t)))
    (condition-case
     (begin
       (load file (lambda (expression)
                    (eval expression environment)))
       (condition-case
        (eval `(handle! (make-request ,(request-rec-header-only request-rec*)))
              environment)
        (exn (exn)
             (let ((message
                    ((condition-property-accessor 'exn 'message)
                     exn))
                   (arguments
                    ((condition-property-accessor 'exn 'arguments)
                     exn))
                   (location
                    ((condition-property-accessor 'exn 'location)
                     exn)))
               (log-error request-rec*
                          (format "Message: ~A; Arguments: ~A; Location: ~A"
                                  message
                                  arguments
                                  location))
               (foreign-value "HTTP_INTERNAL_SERVER_ERROR" int)))))
     (exn (exn)
          (let ((message
                 ((condition-property-accessor 'exn 'message)
                  exn))
                (arguments
                 ((condition-property-accessor 'exn 'arguments)
                  exn))
                (location
                 ((condition-property-accessor 'exn 'location)
                  exn)))
            (log-error request-rec*
                       (format "Message: ~A; Arguments: ~A; Location: ~A"
                               message
                               arguments
                               location))
            (foreign-value "HTTP_INTERNAL_SERVER_ERROR" int))))))

(return-to-host)
