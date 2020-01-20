(include "code-gen/template-tools.scm")
(include "code-gen/templates.scm")

(module api-gen (explore-api
                 gen-module)
(import scheme
        chicken.base
        chicken.process
        chicken.string)

(import srfi-69
        (prefix msgpack mp:)
        msgpack-rpc-client)

(import templates
        template-tools)

(define (explore-api)
  (let ((port (open-input-pipe "nvim --api-info")))
    (let* ((api-info (mp:unpack port))
           (version (hash-table-ref api-info "version"))
           ; (version.api_level (hash-table-ref api-info "version.api_level"))
           ; (version.api_compatible (hash-table-ref api-info "version.api_compatible"))
           (functions (hash-table-ref api-info "functions"))
           (ui_events (hash-table-ref api-info "ui_events"))
           (ui_options (hash-table-ref api-info "ui_options"))
           (types (hash-table-ref api-info "types"))
           (error-types (hash-table-ref api-info "error_types")))
      (display (untangle-msg version))
      (newline)
      (for-each (lambda (fn)
                  (let ((name (hash-table-ref fn "name"))
                        (keys (hash-table-keys fn)))
                    (display name)
                    (display ": ")
                    (write (untangle-msg (hash-table-keys fn)))
                    (newline)
                    (display "   ")
                    (display (hash-table-ref fn "parameters"))
                    (display (untangle-msg (hash-table-ref fn "parameters")))
                    (display " --> ")
                    (display (untangle-msg (hash-table-ref fn "return_type")))
                    (newline)))
                (vector->list functions)) ; (vector->list functions))
      (map (lambda (k)
             (write k)
             (newline)
             (write (untangle-msg (hash-table-ref error-types k)))
             (newline))
           (hash-table-keys error-types))
      (map (lambda (k)
             (write k)
             (newline)
             (newline)
             (write (untangle-msg (hash-table-ref types k)))
             (newline))
           (hash-table-keys types)))))

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
           (ui_events (hash-table-ref api-info "ui_events"))
           (ui_options (hash-table-ref api-info "ui_options"))
           (types (hash-table-ref api-info "types"))
           (error-types (hash-table-ref api-info "error_types")))
      (let (
            (type-table (make-hash-table))
            (type-name-table (make-hash-table)))
        ; type generation
        (append
          (apply append (map (lambda (type)
                 (let* ((name (scheme-style (car type)))
                        (prefix (hash-table-ref (cdr type) "prefix"))
                        (id (hash-table-ref (cdr type) "id"))
                        (props `((prefix . ,prefix) (id . ,id) (scheme-name . ,name))))
                   (hash-table-set! type-table (car type) (alist->hash-table props))
                   (hash-table-set! type-name-table name (car type))
                   (make-type name id)))
               (hash-table->alist types)))
          (map (lambda (fun)
                 (let ((base-name (hash-table-ref fun "name"))
                       (parameters (vector->list (hash-table-ref fun "parameters")))
                       (return-type (hash-table-ref fun "return_type"))
                       (method (hash-table-ref/default fun "method" #f)))
                   (make-method type-table base-name method parameters)))
               (remove-deprecated  (vector->list functions))))
        ))))
)

(import api-gen)
(import chicken.pretty-print)
(pretty-print
  '(import chicken.base chicken.type))
(pretty-print
  '(include "src/nvim-error.scm"))
(for-each pretty-print (gen-module))
