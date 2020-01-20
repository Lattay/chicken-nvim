(module templates (make-type
                   make-method)
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
        (getter-id (make-symbol name "-id")))
    `(
       (define-record-type ,type-name
         (,constructor-name res-id)
         ,predicate
         (res-id ,getter-id))

       (define (,(make-symbol name "->extension") ,short-name)
         (assert (,predicate ,short-name))
         (make-extension ,ext-id (pack/blob (,getter-id ,short-name))))

       (define (,(make-symbol "extension->" name) ext extension)
         (assert (extension? ext))
         (if (= (extension-type ext) ,ext-id)
             (,constructor-name (unpack/blob (extension-data ext)))
             #f)))))

(define (make-method type-table base-name method parameters)
  (let* ((get-prefix
           (lambda ()
             (list (hash-table-ref (hash-table-ref type-table (vector-ref (car parameters) 0)) 'prefix))))
         (scheme-style-name
           (string->symbol (scheme-style
                             (if method
                                 (chop-prefix (get-prefix) base-name)
                                 base-name)))))
    `(define (,scheme-style-name ,@(make-arg-list type-table parameters))
       (begin . ,(validate-args type-table parameters))
       (let-values (((result status)
                     (mrpc-call! nvim-client ,base-name
                                 ,@(make-arg-conversion type-table parameters))))
         (if (equal? status 'success)
             result
             (error (nvim-error->string result)))))))

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
            ((Integer)
             'fixnum?)
            ((Float)
             'flonum?)
            ((String)
             'string?)
            ((Character)
             'character?)
            ((Boolean)
             'boolean?)
            ((Dictionary)
             'hash-table?)
            ((Object)
             'object?)
            (else
              (error (string-append "Unknown type: " type))))))))

(define (make-arg-list type-table parameters)
  (map (lambda (param) (string->symbol (vector-ref param 1))) parameters))

(define (validate-args type-table parameters)
  (map
    (lambda (param)
      (let ((type (vector-ref param 0))
            (var-name (vector-ref param 1)))
        `(assert (and (quote ,type) ,(type-name->predicate type-table type) ,var-name))))
    parameters))

(define (make-arg-conversion type-table parameters)
  (map
    (lambda (param)
      (let ((type (vector-ref param 0))
            (var-name (string->symbol (vector-ref param 1))))
        (if (hash-table-exists? type-table type)
            (let ((convert-symbol (make-symbol
                                    (hash-table-ref
                                      (hash-table-ref type-table type)
                                      'scheme-name)
                                    "->extension")))
              `(,convert-symbol ,var-name))
              var-name)))
    parameters))
)
