/* 
**  mod_scheme.c -- Apache sample scheme module
**  [Autogenerated via ``apxs -n scheme -g'']
**
**  To play with this sample module first compile it into a
**  DSO file and install it into Apache's modules directory 
**  by running:
**
**    $ apxs -c -i mod_scheme.c
**
**  Then activate it in Apache's httpd.conf file for instance
**  for the URL /scheme in as follows:
**
**    #   httpd.conf
**    LoadModule scheme_module modules/mod_scheme.so
**    <Location /scheme>
**    SetHandler scheme
**    </Location>
**
**  Then after restarting Apache via
**
**    $ apachectl restart
**
**  you immediately can request the URL /scheme and watch for the
**  output of this module. This can be achieved for instance via:
**
**    $ lynx -mime_header http://localhost/scheme 
**
**  The output should be similar to the following one:
**
**    HTTP/1.1 200 OK
**    Date: Tue, 31 Mar 1998 14:42:22 GMT
**    Server: Apache/1.3.4 (Unix)
**    Connection: close
**    Content-Type: text/html
**  
**    The sample page from mod_scheme.c
*/ 

#include <stdlib.h>

#include <httpd.h>
#include <http_config.h>
#include <http_log.h>
#include <http_protocol.h>
#include <ap_config.h>
#include <apr_file_info.h>
#include <apr_file_io.h>

#include <chicken.h>

/* At some point, a suitable scheme record? Or maybe we can just
   provide accessors and mutators. */
extern int handle(request_rec *);

/* The sample content handler */
static int scheme_handler(request_rec *r) {
  if (strcmp(r->handler, "scheme")) {
    return DECLINED;
  }

  if (!(r->finfo.filetype == APR_REG ||
        r->finfo.filetype == APR_LNK)) {
    ap_log_rerror(APLOG_MARK,
                  APLOG_DEBUG,
                  OK,
                  r,
                  "Declining to open non-file %s",
                  r->filename);
    return DECLINED;
  }

  apr_file_t *file;

  if (apr_file_open(&file,
                    r->filename,
                    APR_READ | APR_XTHREAD,
                    APR_OS_DEFAULT,
                    r->pool)) {
    ap_log_rerror(APLOG_MARK,
                  APLOG_DEBUG,
                  OK,
                  r,
                  "Could not open %s",
                  r->filename);
    return HTTP_NOT_FOUND;
  }

  /* apr_file_close(file); */

  r->content_type = "text/html";      

  if (!r->header_only)
    ap_rprintf(r, "The sample page from mod_scheme (%d); return from scheme: %d.\n",
               rand(),
               mod_scheme_handle(r->filename, r));
  return OK;
}

static void scheme_register_hooks(apr_pool_t *p) {
  void C_toplevel(C_word x, C_word y, C_word z);
  /* C_word chicken = CHICKEN_run(CHICKEN_default_toplevel); */
  C_word chicken = CHICKEN_run(C_toplevel);
  ap_hook_handler(scheme_handler, NULL, NULL, APR_HOOK_MIDDLE);
}

/* Dispatch list for API hooks */
module AP_MODULE_DECLARE_DATA scheme_module = {
  STANDARD20_MODULE_STUFF, 
  NULL,                  /* create per-dir    config structures */
  NULL,                  /* merge  per-dir    config structures */
  NULL,                  /* create per-server config structures */
  NULL,                  /* merge  per-server config structures */
  NULL,                  /* table of config file commands       */
  scheme_register_hooks  /* register hooks                      */
};

