(define (set-line! (: buf buffer) (: index fixnum) (: line string))
  (let-values (((result status)
                (mrpc-call! nvim-client "buffer_set_line" (buffer->ext buf) index line)))
    (if (equal? status 'success)
        result
        (error (nvim-error->string result)))))

(define (make-method . args)
  `(define (,scheme-style-name ,@arg-list)
     (let-values (((result status)
                   (mrpc-call! nvim-client ,name-as-string ,@arg-conversion)))
       (if (equal? status 'success)
           result
           (error (nvim-error->string result))))))


