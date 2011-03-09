(use extras)
(foreign-declare "#include <httpd.h>")
(define-external
  (handle (c-pointer request_rec))
  int
  (foreign-value "OK" int)
  (random 3))
#;(return-to-host)

