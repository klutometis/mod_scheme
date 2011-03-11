;;;; environments.scm - User-defined evaluation environments - felix
;
; Copyright (c) 2000-2003, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer.
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution.
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Send bugs, suggestions and ideas to:
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Steinweg 1A
; 37130 Gleichen, OT Weissenborn
; Germany

(module environments

  (;export
    make-environment
    environment-copy
    environment?
    interaction-environment?
    environment-empty?
    environment-extendable?
    environment-set-mutable!
    environment-mutable?
    environment-ref
    environment-set!
    environment-extend!
    environment-includes?
    environment-has-binding?
    environment-remove!
    environment-for-each
    environment-symbols)

  (import scheme chicken foreign)

;;;

;candidate for Unit eval?

(define (##sys#hash-table-delete! ht key)
  (let ((k (##sys#hash-symbol key (##sys#size ht))))
    (let next ((b (##sys#slot ht k)) (pb #f))
      (and (not (eq? '() b))
           (let ((nb (##sys#slot b 1)))
             (if (not (eq? key (##sys#slot (##sys#slot b 0) 0)))
                 (next nb b)
                 (begin
                   (if pb (##sys#setslot pb 1 nb)
                     (##sys#setslot ht k nb) )
                   #t ) ) ) ) ) ) )

(define (##sys#hash-table-empty? ht)
  (let ((len (##sys#size ht)))
    (do ((i 0 (fx+ i 1)))
        ((or (fx>= i len) (not (eq? '() (##sys#slot ht i))))
         (fx>= i len)) ) ) )

#| ;these read much easier
(define (##sys#hash-table-delete! ht key)
  (let ((k (##sys#hash-symbol key (vector-length ht))))
    (let next ((b (vector-ref ht k)) (pb #f))
      (and (not (null? b)))
           (let ((nb (cdr b)))
             (if (not (eq? key (vector-ref (car b) 0))) (next nb b)
                (begin
                  (if pb (set-cdr! pb nb)
                    (vector-set! ht k nb) )
                  #t ) ) ) ) ) )

(define (##sys#hash-table-empty? ht)
  (let ((len (vector-length ht)))
    (do ((i 0 (fx+ i 1)))
        ((or (fx>= i len) (not (null? (vector-ref ht i))))
         (fx>= i len)) ) ) )
|#

;;;

;;

(define-inline (->boolean obj)
  (and obj
       #t) )

;;

(define-constant DEFAULT-ENVIRONMENT-TABLE-SIZE 301)

;;

(define (error/type/list loc obj)
  (##sys#error-hook
    (foreign-value "C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR" int)
    loc obj) )

(define (error/undefined-symbol loc env sym)
  (error loc "symbol undefined in environment" sym env) )

(define (error/inextensible-environment loc env)
  (error loc "inextensible environment" env) )

#; ;UNUSED
(define (error/immutable-symbol loc env sym)
  (error loc "symbol immutable in environment" sym env) )

;;

(define-inline (%symbol-value sym)
  (##sys#slot sym 0) )

(define-inline (%symbol-bound? sym)
  (##sys#symbol-has-toplevel-binding? sym) )

(define-inline (%symbol-ref loc sym)
  (if (%symbol-bound? sym)
    (%symbol-value sym)
    (error/undefined-symbol loc env sym) ) )

(define-inline (%symbol-bind! sym val)
  (##sys#setslot sym 0 val) )

;;

(define-inline (%make-environment tbl #!optional extn?)
  (##sys#make-structure 'environment tbl extn?) )

(define-inline (%environment-table env)
  (##sys#slot env 1) )

(define-inline (%environment-extensible? env)
  (##sys#slot env 2) )

;;

(define-inline (%make-environment-table
                 #!optional (siz DEFAULT-ENVIRONMENT-TABLE-SIZE))
  (##sys#make-vector siz '()) )

(define-inline (%environment-table? et)
    ;enough to distinguish interaction-environment
    ;but not a general test
    ;the interaction-environment has no environment-table since
    ;it is represented by the system symbol-table
  (->boolean et) )

(define-inline (%environment-table-size et)
  (##sys#size et) )

(define-inline (%environment-table-ref et i)
  (##sys#slot et i) )

(define-inline (%environment-table-set! et i v)
  (##sys#setslot et i v) )

(define-inline (%environment-table-empty? et)
  (##sys#hash-table-empty? et) )

(define-inline (%environment-table-delete! et sym)
  (##sys#hash-table-delete! et sym) )

(define-inline (%environment-table-binding et sym extn?)
  (##sys#hash-table-location et sym extn?) )

;;

(define-inline (%environment-binding-ref eloc)
  (##sys#slot eloc 1) )

(define-inline (%environment-binding-bound? eloc)
  (not (eq? (##sys#slot eloc 1)
            (##sys#slot '##sys#arbitrary-unbound-symbol 0))) )

(define-inline (%environment-binding-bind! eloc val)
  (##sys#setslot eloc 1 val) )

(define-inline (%environment-binding-mutable? eloc)
  (##sys#slot eloc 2) )

(define-inline (%environment-binding-mutable-set! eloc flag)
  (##sys#setslot eloc 2 flag) )

;;

(define-inline (%get-environment-binding et sym)
  (%environment-table-binding et sym #t) )

(define-inline (%find-environment-binding et sym)
  (%environment-table-binding et sym #f) )

(define-inline (%checked-find-environment-binding loc env et sym)
  (or (%find-environment-binding et sym)
      (error/undefined-symbol loc env sym)) )

(define-inline (%checked-get-environment-binding loc env et sym)
  (or (%find-environment-binding et sym)
      (error/inextensible-environment 'environment-set! env)) )

;;

(define-inline (%environment-variable-exists? et sym)
  (->boolean (%find-environment-binding et sym)) )

(define-inline (%environment-variable-bound? et sym)
  (let ((eloc (%find-environment-binding et sym)))
    (and eloc
         (%environment-binding-bound? eloc) ) ) )

(define-inline (%update-environment-variable! et sym . v+m)
  (let ((eloc (%get-environment-binding et sym)))
    (unless (null? v+m)
      (%environment-binding-bind! eloc (car v+m))
      (unless (null? (cdr v+m))
        (%environment-binding-mutable-set! eloc (cadr v+m)) ) ) ) )

;;

(define-inline (%environment-variable-ref loc env et sym)
  (let ((eloc (%checked-find-environment-binding loc env et sym)))
    (if (%environment-binding-bound? eloc)
      (%environment-binding-ref eloc) ) ) )

(define-inline (%environment-variable-bind! loc env et sym val)
  (%environment-binding-bind!
    (%checked-get-environment-binding loc env et sym)
    val) )

(define-inline (%environment-variable-mutable? loc env et sym)
  (%environment-binding-mutable?
    (%checked-find-environment-binding loc env et sym)) )

(define-inline (%environment-variable-mutable-set! loc env et sym mtbl?)
  (%environment-binding-mutable-set!
    (%checked-find-environment-binding loc env et sym)
     mtbl?) )


#; ;UNUSED
(define-inline (%environment-binding-bind!/mutable? loc env eloc val sym)
  ;Created symbols are always mutable
  (if (%environment-binding-mutable? eloc)
    (%environment-binding-bind! eloc val)
    (error/immutable-symbol loc env sym)) )
;;

(define-inline (%make-environment-table-setter et #!optional syms)
  (lambda (sym)
    (when (or (not syms) (memq sym syms))
      (%environment-binding-bind!
        (%get-environment-binding et sym)
        (%symbol-value sym)) ) ) )

;;

;; The common environment processing form:
;; (with-environment-table* env
;;   (... process with environment-table et ...)
;;   (.. process interaction-environment ..))

(define-syntax (with-environment-table* f r c)
  (let ((env (cadr f))
        (env-expr (caddr f))
        (intenv-expr (if (null? (cdddr f)) `(,(r 'void)) (cadddr f))) )
    `(,(r 'let) ((et (,(r '%environment-table) ,env)))
      (,(r 'if) (,(r '%environment-table?) et) ,env-expr
        ,intenv-expr ) ) ) )

;;; Public API

;;

(define (make-environment #!optional extn?)
  (%make-environment (%make-environment-table) extn?) )

;;

(define (environment-copy env . args)
  (##sys#check-structure env 'environment 'environment-copy)
  (let ((has-extn? (pair? args)))
    (let-optionals* args ((extn? #f) (syms #f) (mtbl? extn?))
      (when syms (##sys#check-list syms 'environment-copy))
      (%make-environment
        (##sys#copy-env-table
          (with-environment-table* env
              ;environment object
            et
              ;interaction-environment
            (let ((et (%make-environment-table)))
                ;Use a filtered snapshot of the existing symbol table
                ;as the basis for the new environment table.
              (##sys#walk-namespace (%make-environment-table-setter et syms))
              et ) )
             has-extn? mtbl? syms)
        extn?) ) ) )

;;

(define environment? ##sys#environment?)

;;

(define (interaction-environment? obj)
  ;there can be only one
  (eq? obj (interaction-environment)) )

;;

(define (environment-extendable? env)
  (##sys#check-structure env 'environment 'environment-extendable?)
  (%environment-extensible? env) )

;;

(define (environment-empty? env)
  (##sys#check-structure env 'environment 'environment-extendable?)
  (with-environment-table* env
      ;environment object
    (and et ;should never happen
         (%environment-table-empty? et))
      ;interaction-environment
    #f) )

;;

(define (environment-mutable? env sym)
  (##sys#check-structure env 'environment 'environment-mutable?)
  (with-environment-table* env
      ;environment object
    (%environment-variable-mutable? 'environment-mutable? env et sym)
      ;interaction-environment
    #t) )

;;

(define (environment-set-mutable! env sym mtbl?)
  (##sys#check-structure env 'environment 'environment-set-mutable!)
  (##sys#check-symbol sym 'environment-set-mutable!)
  (with-environment-table* env
      ;environment object
    (%environment-variable-mutable-set! 'environment-set-mutable! env
      et sym mtbl?)
      ;interaction-environment
    (void)) )

;;

(define (environment-ref env sym)
  (##sys#check-structure env 'environment 'environment-ref)
  (##sys#check-symbol sym 'environment-ref)
  (with-environment-table* env
      ;environment object
    (%environment-variable-ref 'environment-ref env et sym)
      ;interaction-environment
    (%symbol-ref 'environment-ref sym) ) )

;;

(define (environment-set! env sym val)
  (##sys#check-structure env 'environment 'environment-set!)
  (##sys#check-symbol sym 'environment-set!)
  (with-environment-table* env
      ;environment object
    (%environment-variable-bind! 'environment-set! env et sym val)
      ;interaction-environment
    (%symbol-bind! sym val) ) )

;;

(define (environment-extend! env sym . v+m)
  (##sys#check-structure env 'environment 'environment-extend!)
  (##sys#check-symbol sym 'environment-extend!)
  (with-environment-table* env
      ;environment object
    (apply %update-environment-variable! et sym v+m)
      ;note that the variable is already existing in the
      ;interaction-environment since sym exists!
    (unless (null? v+m) (%symbol-bind! sym (car v+m)) ) ) )

;;

(define (environment-includes? env sym)
  (##sys#check-structure env 'environment 'environment-includes?)
  (##sys#check-symbol sym 'environment-includes?)
  (with-environment-table* env
      ;environment object
    (%environment-variable-exists? et sym)
      ;interaction-environment
    #t) )

;;

(define (environment-has-binding? env sym)
  (##sys#check-structure env 'environment 'environment-has-binding?)
  (##sys#check-symbol sym 'environment-has-binding?)
  (with-environment-table* env
      ;environment object
    (%environment-variable-bound? et sym)
      ;interaction-environment
    (%symbol-bound? sym) ) )

;;

(define (environment-remove! env syms #!optional (silent? #f) (inextd? #t))
  (##sys#check-structure env 'environment 'environment-remove!)
  (if (not (or (%environment-extensible? env) inextd?))
    (unless silent?
      (error 'environment-remove!
        "cannot remove bindings from an inextensible environment" env) )
    (let ((syms
            (cond
              ((symbol? syms) (list syms))
              ((pair? syms)   syms)
              (else
                (error/type/list 'environment-remove! syms)) ) ) )
      (with-environment-table* env
          ;environment object
        (let loop ((syms syms))
          (cond
            ((null? syms)
              (void) )
            ((not (pair? syms))
              (##sys#not-a-proper-list-error syms) )
            (else
              (let ((sym (car syms)))
                (unless (and (%environment-table-delete! et sym) (not silent?))
                  (error/undefined-symbol 'environment-remove! env sym) )
                (loop (cdr syms)) ) ) ) )
          ;interaction environment
        (error 'environment-remove!
          "cannot remove bindings from the interaction environment" env) ) ) ) )

;;

(define (environment-for-each env proc)
  (##sys#check-structure env 'environment 'environment-for-each)
  (with-environment-table* env
      ;environment object
    (##sys#hash-table-for-each proc et)
      ;interaction-environment
    (##sys#walk-namespace
      (lambda (sym)
        (proc sym (%symbol-value sym))))) )

;;

(define (environment-symbols env)
  (##sys#check-structure env 'environment 'environment-symbols)
  (##sys#environment-symbols env) )

) ;module environments
