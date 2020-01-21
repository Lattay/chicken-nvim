(module template-tools (make-symbol
                        chop-prefix
                        getter-client
                        scheme-style)

(import scheme
        chicken.base
        chicken.string)

(define (make-symbol . args)
  (string->symbol (apply string-append args)))

(define (scheme-case s)
  (string-translate s "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" "abcdefghijklmnopqrstuvwxyz-"))

(define (chop-prefix prefix name)
  (if (substring=? name prefix)
      (substring name (string-length prefix))
      name))

(define (make-predicate name)
  (if (substring=? name "is-")
      (string-append (substring name 3) "?")
      name))

(define (scheme-style name)
  (make-predicate (scheme-case name)))

(define (getter-client type)
  (make-symbol (scheme-style type) "-client"))
)
