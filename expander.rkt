#lang br/quicklang

(provide (rename-out [b-module-begin #%module-begin]))
(define-macro (b-module-begin EXPR)
  #'(#%module-begin EXPR))

(define-for-syntax (debug x)
  (println (syntax->datum x)) x)

(define-macro string #'string-append)
(define-macro int    #'identity)
(define-macro dec    #'identity)
(define-macro name   #'identity)
(define-macro comma  #'identity)
(define-macro parens #'identity)
(define-macro expres #'identity)
(provide string int dec name comma parens expres)

(define (bilang-let x) (lambda (f) (f x)))
(provide (rename-out [bilang-let let]))

(define-macro-cases apply3
  [(apply3 (expl2 (apply1 F (parens (name VARNAME))) (rad (anion)) VALUE) BODY) #'((F VALUE) (lambda (VARNAME) BODY))])

(define-macro-cases app
  [(app (parens (comma (name N))) (braces BODY)) (debug #'(lambda (N) BODY))]
  [(app (parens        (name N))  (braces BODY)) (debug #'(lambda (N) BODY))]
  [(app F X)                                     (debug #'(F X))])

(define-macro apply1 #'app)
(define-macro comma1 #'app)

(provide apply3 apply1 comma1)
