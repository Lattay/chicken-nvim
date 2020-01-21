(include "code-gen/template-tools.scm")
(include "code-gen/templates.scm")

(module api-gen (gen-module)
(import scheme
        chicken.base
        chicken.process
        chicken.string)

(import srfi-69
        (prefix msgpack mp:)
        msgpack-rpc-client)

(import templates
        template-tools)


(define (remove-deprecated functions)
  (let loop ((rest functions) (acc '()))
    (if (null? rest)
        (reverse acc)
        (if (hash-table-ref/default (car rest) "deprecated_since" #f)
            (loop (cdr rest) acc)
            (loop (cdr rest) (cons (car rest) acc))))))

(define (gen-module)
  (let ((port (open-input-pipe "nvim --api-info")))
    (let* ((api-info (mp:unpack port))
           (version (hash-table-ref api-info "version"))
           ; (version.api_level (hash-table-ref api-info "version.api_level"))
           ; (version.api_compatible (hash-table-ref api-info "version.api_compatible"))
           (functions (hash-table-ref api-info "functions"))
           (ui-events (hash-table-ref api-info "ui_events"))
           (ui-options (hash-table-ref api-info "ui_options"))
           (types (hash-table-ref api-info "types"))
           (error-types (hash-table-ref api-info "error_types")))
      (let ((type-table (make-hash-table))
            (type-name-table (make-hash-table)))
        (let ((type-code 
                (apply append
                       (map (lambda (type)
                              (let* ((name (scheme-style (car type)))
                                     (prefix (hash-table-ref (cdr type) "prefix"))
                                     (id (hash-table-ref (cdr type) "id"))
                                     (props `((prefix . ,prefix) (id . ,id) (scheme-name . ,name))))
                                (hash-table-set! type-table (car type) (alist->hash-table props))
                                (hash-table-set! type-name-table name (car type))
                                (make-type name id)))
                            (hash-table->alist types))))
              (func-table (make-hash-table)))
          (for-each
            (lambda (fun)
              (let* ((base-name (hash-table-ref fun "name"))
                     (method (hash-table-ref/default fun "method" #f))
                     (parameters (vector->list (hash-table-ref fun "parameters")))
                     ; (return-type (hash-table-ref fun "return_type"))
                     (get-prefix
                       (lambda ()
                         (hash-table-ref
                           (hash-table-ref
                             type-table (vector-ref (car parameters) 0))
                           'prefix)))
                     (name (string->symbol (scheme-style
                                             (if method
                                                 (chop-prefix (get-prefix) base-name)
                                                 (chop-prefix "nvim_" base-name))))))
                (if (hash-table-exists? func-table name)
                    (let ((func (hash-table-ref func-table name)))
                      (if method
                          (if (equal? (cdr parameters) (hash-table-ref func 'parameters))
                              (hash-table-set! func 'obj-type `((,(vector-ref (car parameters) 0) . ,base-name)
                                                                . ,(hash-table-ref func 'obj-type)))
                              (error "I did not expected methods to have different signatures out of the first argument."))
                          (hash-table-set! func 'obj-type `(("Neovim" . ,base-name)
                                                            . ,(hash-table-ref func 'obj-type)))))
                    (hash-table-set! func-table name
                                     (alist->hash-table
                                       (if method
                                           `((parameters . ,(cdr parameters))
                                             (obj-type . ((,(vector-ref (car parameters) 0) . ,base-name))))
                                           `((parameters . ,parameters)
                                             (obj-type . (("Neovim" . ,base-name))))))))))
            (remove-deprecated (vector->list functions)))

          (values
            (hash-table-keys func-table)
            type-code
            (map
              (lambda (func)
                (let ((name (car func))
                      (function (cdr func)))
                  (let ((parameters (hash-table-ref function 'parameters))
                        (obj-types (hash-table-ref/default function 'obj-type #f)))
                    (if (= (length obj-types) 1)
                        (make-method type-table name (cdar obj-types) (caar obj-types) parameters)
                        (make-multi-method type-table name obj-types parameters)))))
              (hash-table->alist func-table))))))))
)

(import api-gen)
(import chicken.pretty-print)

(define-values (exported type-code func-code) (gen-module))
(display #<<EOS
; This file have been generated by code-gen/api-gen.scm.
; Do not modify it by hand.
;
; This file implement the binding to the Neovim RPC API

EOS
)
(pretty-print
  `(module nvim ,exported
           (include "src/preamble.scm")
           (include "src/nvim-error.scm")
           ,@type-code
           . ,func-code))
