;; This file provide a dedicated interface for nvim plugin
;; launched from nvim
(module nvim-plugin (
                     *neovim*
                     *plugin-info*
                     make-version
                     init-plugin
                     update-info
                     method
                     run
                     )
  (import scheme chicken.base)
  (import (prefix nvim nv:))
  (import srfi-1)

  ;; create a version object ready to be sent to neovim
  (define (make-version major minor patch #!optional (prerelease #f) (commit #f))
    (let ((table (make-hash-table 3)))
      (hash-table-set! table "major" major)
      (hash-table-set! table "minor" minor)
      (hash-table-set! table "patch" patch)
      (when prerelease
        (hash-table-set! table "prerelease" prerelease))
      (when commit
        (hash-table-set! table "commit" commit))
      table))

  ;; client info declared to neovim
  (define *plugin-info* (make-parameter '()))

  ;; neovim instance
  (define *neovim* (make-parameter #f))

  ;; Connect to neovim on STDIO and initialize plugin info
  (define (init-plugin name
                       #!key (version (make-version 1 0 0))
                       (attributes (make-hash-table 0)))
    (when (not (hash-table? version))
      (error "version should be an object returned by make-version"))
    (when (not (hash-table? attributes))
      (error "attributes should be a string:string hash-table"))
    (when (not (string? name))
      (error "name should be method name string as declared to neovim"))
    (let ((neovim (nv:connect 'stdio))
      (*plugin-info* (list
                      name
                      version
                      "plugin"
                      (make-hash-table 0)
                      attributes))
      (update-info neovim)
      (*neovim* neovim)))

  ;; Update client info and tell neovim
  (define (update-info)
    (apply nv:set-client-info (cons (*neovim*) (*plugin-info*))))

  ;; Register a new method
  (define (method name proc #!optional (async #f) (nargs #f))
    (let ((attr (make-hash-table)))
      (hash-table-set! attr "async" async)
      (when nargs
        (if (or
              (and
                (number? nargs)
                (non-negative-integer? nargs))
              (and
                (list? nargs)
                (= (length nargs) 2)
                (non-negative-integer? (car nargs))
                (non-negative-integer? (cdr nargs))))
          (hash-table-set! attr "nargs" nargs)
          (error "nargs should be either a single non-negative integer or list of two non-negative integers.")))
    (hash-table-set! (fourth (*plugin-info*)))
    (nv:register-method! (*neovim*) name proc) 
    (update-info)))

  (define (run #!optional (timeout 0) (listen-timeout 0.1))
    (nv:listen! (*neovim*) listen-timeout)
    (run timeout listen-timeout))
)
