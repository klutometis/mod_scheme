(foreign-declare "#include <httpd.h>")
(define-external
  (handle (c-pointer request_rec))
  int
  (foreign-value "DECLINED" int))
