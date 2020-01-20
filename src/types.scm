(define (make-symbol . args)
  (string->symbol (apply string-append args)))

(define (make-type name num)
  (let ((type-name name)
        (short-name name)
        (constructor-name (make-symbol "make-" name))
        (predicate-name (make-symbol name "?"))
        (getter-num (make-symbol name "-num"))
        (to-extension (make-symbol name "->extension"))
        (from-extension (make-symbol "extension->" name))
        (id-num num)

  `(begin
     (define-record-type ,type-name
       (,constructor-name num)
       ,predicate-name
       (num ,getter-num))

     (define (,to-extension (: ,short-name ,type-name))
       (make-extension ,id-num (pack/blob (,getter-num ,short-name))))

     (define (,from-extension (: ext extension))
       (if (= (extension-type ext) id-num)
           (,constructor-name (unpack/blob (extension-data ext)))
           #f))))
