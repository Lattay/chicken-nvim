(import scheme chicken.base)
(import srfi-69
        msgpack
        (prefix msgpack-rpc-client mrpc:))

(define (true . args) #t)

(define-record-type neovim
  (make-neovim mrpc-client)
  neovim?
  (mrpc-client neovim-client))
