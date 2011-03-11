;;; TODO: Apache segfaults if we try to (use environments); compiling
;;; from source instead.
(include "srclib/environments/environments.scm")
(require-library srfi-13)
(import extras ports srfi-13 environments defstruct foreigners)
(import-for-syntax matchable)

(foreign-declare "#include <httpd.h>")
(foreign-declare "#include <http_log.h>")

(define ok
  (foreign-value "OK" int))
(define internal-server-error
  (foreign-value "HTTP_INTERNAL_SERVER_ERROR" int))
(define log-debug
  (foreign-value "LOG_DEBUG" int))

(define log-level (make-parameter log-debug))
(define handler-function (make-parameter 'handle!))

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
  (int header_only request-rec-header-only))

(define (rputs string request-rec*)
  ((foreign-lambda int
                   "ap_rputs"
                   c-string
                   (c-pointer "request_rec"))
   string
   request-rec*))

(define (rflush request-rec*)
  ((foreign-lambda int
                   "ap_rflush"
                   (c-pointer "request_rec"))
   request-rec*))

(define (make-request-output-port request-rec*)
  (make-output-port
   (lambda (scribendum)
     (rputs scribendum request-rec*))
   void
   (lambda () (rflush request-rec*))))

(define (log-error request-rec* format-string . args)
  ((foreign-lambda void
                   "ap_log_rerror"
                   c-string
                   int
                   int
                   int
                   (c-pointer "request_rec")
                   c-string)
   ;; something more informative, perhaps?
   (foreign-value "__FILE__" c-string)
   (foreign-value "__LINE__" int)
   (log-level)
   (foreign-value "OK" int)
   request-rec*
   (apply format (cons format-string args))))

(define (make-request-error-port request-rec*)
  (make-output-port
   (lambda (scribendum)
     (log-error request-rec* scribendum))
   void
   void))

(define-syntax server-error-on-condition
  (lambda (expression rename compare)
    (match-let (((_ request-rec* . forms) expression))
      `(condition-case
        (begin ,@forms)
        (exn (exn)
          (let ((message
                 ((condition-property-accessor 'exn 'message) exn))
                (arguments
                 ((condition-property-accessor 'exn 'arguments) exn))
                (location
                 ((condition-property-accessor 'exn 'location) exn)))
            (log-error request-rec*
                       "Message: ~A; arguments: ~A; location: ~A"
                       message
                       arguments
                       location)
            internal-server-error))))))

(define invoke-handler
  (case-lambda
   ((request-rec* environment)
    (invoke-handler request-rec* environment (handler-function)))
   ((request-rec* environment handler-function)
    (server-error-on-condition
     request-rec*
     (with-output-to-port
         (make-request-output-port request-rec*)
       (lambda ()
         (with-error-output-to-port
          (make-request-error-port request-rec*)
          (lambda ()
            (eval `(handle!
                    (make-request
                     ,(request-rec-header-only request-rec*)))
                  environment)))))))))

(define (eval-file request-rec* environment file)
  (server-error-on-condition
   request-rec*
   (load file (lambda (expression)
                (eval expression environment)))
   (invoke-handler request-rec* environment)))

(define-external
  (mod_scheme_handle (c-string file) (c-pointer request-rec*))
  int
  (let ((environment (environment-copy (interaction-environment) #t)))
    (eval-file request-rec* environment file)))

(return-to-host)
