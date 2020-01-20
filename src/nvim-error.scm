(define (nvim-error->string err)
  (let ((type (vector-ref err 0))
        (msg (vector-ref err 1)))
    (string-append (vector-ref error-names type) ": " result)))

(define (make-nvim-error type msg)
  (vector type msg))
