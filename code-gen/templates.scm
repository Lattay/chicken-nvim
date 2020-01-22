(module templates (make-type
                   make-method
                   make-multi-method)
(import scheme
        chicken.base
        chicken.string)
(import srfi-69)
(import template-tools)

(define (make-type name ext-id)
  (let ((type-name (string->symbol name))
        (short-name (string->symbol (substring name 0 3)))
        (constructor-name (make-symbol "make-" name))
        (predicate (make-symbol name "?"))
        (getter-client (make-symbol name "-client"))
        (getter-id (make-symbol name "-id")))
    `((export ,type-name)
      (export ,constructor-name)
      (export ,predicate)
      (export ,getter-client)
      (export ,getter-id)
      (define-record-type ,type-name
         (,constructor-name client res-id)
         ,predicate
         (client ,getter-client)
         (res-id ,getter-id))

       (define (,(make-symbol name "->extension") ,short-name)
         (assert (,predicate ,short-name))
         (make-extension ,ext-id (mp:pack/blob (,getter-id ,short-name))))

       (define (,(make-symbol "extension->" name) client ext)
         (assert (extension? ext))
         (if (= (extension-type ext) ,ext-id)
             (,constructor-name (neovim-client client) (mp:unpack/blob (extension-data ext)))
             #f)))))

(define (make-method type-table scheme-style-name base-name self-type parameters)
  `(define (,scheme-style-name self . ,(make-arg-list type-table parameters))
     (begin
       ,(validate-arg type-table self-type 'self)
       . ,(validate-args type-table parameters))
     (let ((client (,(getter-client self-type) self)))
       (let-values (((result status)
                     (mrpc:call! client ,base-name
                                 ,(make-arg-conversion type-table self-type 'self)
                                 . ,(make-args-conversion type-table parameters))))
         (if (equal? status 'success)
             result
             (error (nvim-error->string result)))))))

(define (make-multi-method type-table scheme-style-name obj-types parameters)
  `(define (,scheme-style-name self . ,(make-arg-list type-table parameters))
     (begin
       . ,(validate-args type-table parameters))
     (cond
       ,@(build-dispatching type-table obj-types parameters)
       (else
         (error "No method for instance." self)))))

(define (build-dispatching type-table obj-types parameters)
  (map
    (lambda (type-pair)
      (let ((type (car type-pair))
            (base-name (cdr type-pair)))
        `((,(type-name->predicate type-table type) self)
          (let ((client (,(getter-client type) self)))
            (let-values (((result status)
                          (mrpc:call! client ,base-name
                                      ,(make-arg-conversion type-table type 'self)
                                      . ,(make-args-conversion type-table parameters))))
              (if (equal? status 'success)
                  result
                  (error (nvim-error->string result)))))
          )))
    obj-types))

(define (type-name->predicate type-table type)
  (if (hash-table-exists? type-table type)
      (string->symbol
        (string-append
          (hash-table-ref (hash-table-ref type-table type) 'scheme-name)
          "?"))
      (cond
        ((substring=? type "ArrayOf")
         'vector?)
        (else
          (case (string->symbol type)
            ((Integer) 'fixnum?)
            ((Float) 'flonum?)
            ((String) 'string?)
            ((Character) 'character?)
            ((Boolean) 'boolean?)
            ((Dictionary) 'hash-table?)
            ((Neovim) 'neovim?)
            ((Object) 'true)
            (else (error (string-append "Unknown type: " type))))))))

(define (make-arg-list type-table parameters)
  (map (lambda (param) (string->symbol (vector-ref param 1))) parameters))

(define (validate-arg type-table type var-name)
  `(assert (and ,type (,(type-name->predicate type-table type) ,var-name))))

(define (validate-args type-table parameters)
  (map
    (lambda (v)
      (validate-arg type-table (vector-ref v 0) (vector-ref v 1)))
    parameters))

(define (make-arg-conversion type-table type var-name)
  (if (hash-table-exists? type-table type)
      (let ((convert-symbol (make-symbol
                              (hash-table-ref
                                (hash-table-ref type-table type)
                                'scheme-name)
                              "->extension")))
        `(,convert-symbol ,var-name))
      var-name))

(define (make-args-conversion type-table parameters)
  (map
    (lambda (param)
      (make-arg-conversion type-table (vector-ref param 0) (string->symbol (vector-ref param 1))))
    parameters))
)
