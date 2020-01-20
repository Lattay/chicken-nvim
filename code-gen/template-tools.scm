(module template-tools (make-symbol
                        chop-prefix
                        scheme-style)

(import scheme
        chicken.base
        chicken.string)

(define (make-symbol . args)
  (string->symbol (apply string-append args)))

(define (scheme-case s)
  (string-translate s "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" "abcdefghijklmnopqrstuvwxyz-"))

(define (chop-prefix prefix-list name)
  (let loop ((rest prefix-list))
    (if (null? rest)
        (error (string-append "Shitty chop on " name))
        (if (substring=? name (car rest))
            (substring name (string-length (car rest)))
            (loop (cdr rest))))))

(define (make-predicate name)
  (if (substring=? name "is-")
      (string-append (substring name 3) "?")
      name))

(define (scheme-style name)
  (make-predicate (scheme-case name)))
)
