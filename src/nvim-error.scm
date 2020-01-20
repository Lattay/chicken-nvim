(define (nvim-error->string error-type->error-name err)
  (let ((type (vector-ref err 0))
        (msg (vector-ref err 1)))
    (string-append (error-type->error-name type) ": " result)))

(define (make-nvim-error type msg)
  (vector type msg))
