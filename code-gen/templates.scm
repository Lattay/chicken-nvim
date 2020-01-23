(module templates (make-type
                   make-method
                   make-polymorphic-conversions
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
        (getter-nvim (make-symbol name "-nvim"))
        (getter-id (make-symbol name "-id")))
    `((export ,type-name)
      (export ,constructor-name)
      (export ,predicate)
      (export ,getter-nvim)
      (export ,getter-id)
      (define-record-type ,type-name
         (,constructor-name client res-id)
         ,predicate
         (nvim ,getter-nvim)
         (res-id ,getter-id))

       (define (,(make-symbol name "->extension") ,short-name)
         (assert (,predicate ,short-name))
         (mp:make-extension ,ext-id (mp:pack/blob (,getter-id ,short-name))))

       (define (,(make-symbol "extension->" name) nvim ext)
         (assert (mp:extension? ext))
         (if (= (mp:extension-type ext) ,ext-id)
             (,constructor-name nvim (mp:unpack/blob (mp:extension-data ext)))
             #f)))))

(define (make-polymorphic-conversions type-table)
  `((export pack)
    (export unpack)
    (define (pack obj)
      (cond
        ,@(map
            (lambda (type-name)
              `((,(type-name->predicate type-table type-name) obj)
                (,(make-symbol (scheme-type type-table type-name) "->extension") obj)))
            (hash-table-keys type-table))
        (else
          obj)))
    (define (unpack nvim obj)
      (if (mp:extension? obj)
          (cond
            ,@(map
                (lambda (type-name)
                  `((= (mp:extension-type obj) ,(hash-table-ref (hash-table-ref type-table type-name) 'id))
                    (,(make-symbol
                        "extension->"
                        (scheme-type type-table type-name))
                      nvim
                      obj)))
                (hash-table-keys type-table))
            (else
              (error "Unknown extension" obj)))
          obj))))

(define (make-method type-table scheme-style-name base-name self-type parameters return-type)
  `(define (,scheme-style-name self . ,(make-arg-list type-table parameters))
     (begin
       ,(validate-arg type-table self-type 'self)
       . ,(validate-args type-table parameters))
     (let-values (((result status)
                   ,(let ((getter (getter-nvim type-table self-type)))
                      (if (equal? self-type "Neovim")
                          `(mrpc:call! (,getter self) ,base-name
                                       . ,(make-args-conversion type-table parameters))
                          `(mrpc:call! (,getter self) ,base-name
                                       ,(make-arg-conversion type-table self-type 'self)
                                       . ,(make-args-conversion type-table parameters))))))
       (if (equal? status 'success)
           ,(make-result-conversion type-table return-type 'result)
           (error (nvim-error->string result))))))

(define (make-multi-method type-table scheme-style-name obj-types parameters)
  `(define (,scheme-style-name self . ,(make-arg-list type-table parameters))
     (begin
       . ,(validate-args type-table parameters))
     (cond
       ,@(map
           (lambda (type-pair)
             (let ((self-type (car type-pair))
                   (base-name (cadr type-pair))
                   (return-type (cddr type-pair)))
               `((,(type-name->predicate type-table self-type) self)
                 (let ((client (,(getter-nvim type-table self-type) self)))
                   (let-values (((result status)
                                 ,(if (equal? self-type "Neovim")
                                      `(mrpc:call! client ,base-name
                                                   . ,(make-args-conversion type-table parameters))
                                      `(mrpc:call! client ,base-name
                                                   ,(make-arg-conversion type-table self-type 'self)
                                                   . ,(make-args-conversion type-table parameters)))))
                     (if (equal? status 'success)
                         ,(make-result-conversion type-table return-type 'result)
                         (error (nvim-error->string result))))))))
           obj-types)
       (else
         (error "No method for instance." self)))))


(define (type-name->predicate type-table type)
  (if (hash-table-exists? type-table type)
      (string->symbol
        (string-append
          (scheme-type type-table type)
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
      (validate-arg type-table (vector-ref v 0) (string->symbol (vector-ref v 1))))
    parameters))

(define (make-arg-conversion type-table type var-name)
  (cond
    ((equal? type "Object")
     `(pack ,var-name))
    ((hash-table-exists? type-table type)
     `(,(make-symbol
          (scheme-type type-table type)
          "->extension")
        ,var-name))
    (else
      var-name)))

(define (make-args-conversion type-table parameters)
  (map
    (lambda (param)
      (make-arg-conversion type-table (vector-ref param 0) (string->symbol (vector-ref param 1))))
    parameters))

(define (make-result-conversion type-table type var-name)
  (cond
    ((equal? type "Object")
     `(unpack self-nvim ,var-name))
    ((hash-table-exists? type-table type)
     `(,(make-symbol "extension->"
                     (scheme-type type-table type))
        (,(getter-nvim type-table type) self)
        ,var-name))
    (else
      var-name)))

(define (getter-nvim type-table type)
  (if (equal? type "Neovim")
      'identity
      (if (hash-table-exists? type-table type)
          (make-symbol (scheme-type type-table type) "-nvim")
          (error "This object do not carry nvim instance."))))

(define (scheme-type type-table type)
  (hash-table-ref (hash-table-ref type-table type) 'scheme-name))

(define (make-version-info version)
  `((define (nvim-version)
      ,(car version))
    (define (api-version)
      ,(cdr version))))
)
