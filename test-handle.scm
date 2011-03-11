;; (import scheme chicken)
(require-library environments)
(import environments)

(define (handle! request)
  (make-environment ounth)
  (request-header-only request))
