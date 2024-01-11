#lang br/quicklang

(provide (rename-out [b-module-begin #%module-begin]))
(define-macro (b-module-begin EXPR)
  #'(#%module-begin EXPR))

(define-for-syntax (debug x)
  (println (syntax->datum x)) x)

(define string string-append)
(define int    identity)
(define dec    identity)
(define name   identity)
(define expres identity)
(provide string int dec name expres)

(define (bilang-let x) (lambda (f) (f x)))
(provide (rename-out [bilang-let let]))

(define-macro-cases comma1
  [(comma1 (name N) X)    (debug #'(app (atom N) X))]
  [(comma1 F X)           (debug #'(app F X))])

(define-macro-cases app
  [(app (atom A RESTRICT ...) (braces BODY) ARG ...) (debug #'(app (lambda (A) BODY) ARG ...))]
  [(app (atom A RESTRICT ...) X ARG ...)             (debug #'(app (atom A RESTRICT ... X) ARG ...))]
  [(app (comma1 (name N) X) ARG ...)                 (debug #'(app (atom N) X ARG ...))]
  [(app (parens (name N)) ARG ...)                   (debug #'(app (atom N) ARG ...))]
  [(app (parens X) ARG ...)                          (debug #'(app X ARG ...))]
  [(app (apply1 F X) ARG ...)                        (debug #'(app F X ARG ...))]
  [(app (comma1 F X) ARG ...)                        (debug #'(app F X ARG ...))]
  [(app F X Y ...)                                   (debug #'(app (F X) Y ...))]
  [(app X)                                           (debug #'X)])

(define-macro apply1 #'app)

(provide apply1 comma1)

