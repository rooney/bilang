#lang br
(require brag/support)

(define-lex-abbrevs
  (prime?     (:* #\'))
  (dash       (:+ #\-))
  (alphas     (:+ (:/ #\a #\z #\A #\Z)))
  (alnums?    (:* (:/ #\a #\z #\A #\Z #\0 #\9)))
  (alnums     (:+ (:/ #\a #\z #\A #\Z #\0 #\9)))
  (digits     (:+ (:/                 #\0 #\9)))
  (symbols    (:+ (char-set "+/-><=*\\~?!&|^#%$@_")))
  (spacetabs  (:+ (:or #\space #\tab)))
  (spacetabs? (:* (:or #\space #\tab)))
  (newline    (:seq spacetabs? (:or #\newline "\r\n")))
  (dent       (:seq (:+ newline) (:* #\tab)))
  (dentspace  (:seq dent #\space))
  (tickspace  (:seq #\` spacetabs))
  (tickdent   (:seq #\` dent))
  (integer    (:seq digits (:* (:seq #\_ digits))))
  (decimal    (:seq integer #\. integer))
  (identifier (:seq alphas alnums? (:* (:seq (char-set "+/-") alnums prime?))))
  (unit       (:seq alphas prime?))
  (op         (:seq symbols prime?))
  (question   (:seq (:+ #\?) prime?)))

(define-macro-cases indent/dedent/newline
  [(indent/dedent/newline)                       #'(indent/dedent/newline lexeme on-indent: _lexer)]
  [(indent/dedent/newline DENT-LEXEME)           #'(indent/dedent/newline DENT-LEXEME on-indent: _lexer)]
  [(indent/dedent/newline on-indent: NEXT-LEXER) #'(indent/dedent/newline lexeme on-indent: NEXT-LEXER)]
  [(indent/dedent/newline DENT-LEXEME on-indent: NEXT-LEXER)
   #'(unless-eof
      (let* ([lines (string-split (string-replace DENT-LEXEME "\r\n" "\n") "\n" #:trim? #f)]
             [newlines (sub1 (length lines))]
             [new-dent (string-length (last lines))]
             [old-dent (length _indents)])
        (if (> new-dent old-dent)
            (cons (token 'NEWLINE newlines)
                  (for/list ([_ (range old-dent new-dent)]) (push-indent! NEXT-LEXER)))
            (append (for/list ([_ (range old-dent new-dent -1)]) (pop-indent!))
                    (list (token 'NEWLINE newlines))))))])

(define (handle-eof)
  (for/list ([_ (range (length _indents))]) (pop-indent!)))

(define-macro (unless-eof ACTIONS ...)
  #'(if (equal? eof (peek-char input-port))
        (handle-eof)
        (begin ACTIONS ...)))

(define-macro (base-lexer RULES ...)
  #'(lexer RULES ...
           [dent     (indent/dedent/newline)]
           [(eof)    (handle-eof)]
           [any-char (token 'UNKNOWN-TOKEN lexeme)]))

(define-macro (main-lexer RULES ...)
  #'(base-lexer RULES ...
                ["{," (list (token 'LBRACE   #\{) (token 'IT))]
                [#\{        (token 'LBRACE   #\{)]
                [#\}        (token 'RBRACE   #\})]
                [#\(        (token 'LPAREN   #\()]
                [#\)        (token 'RPAREN   #\))]
                [#\[        (token 'LBRACKET #\[)]
                [#\]        (token 'RBRACKET #\])]
                [#\$        (token 'DOLLAR   #\$)]
                [#\+        (token 'PLUS     #\+)]
                [#\/        (token 'SLASH    #\/)]
                [#\:        (token 'COLON    #\:)]
                [#\,        (token 'COMMA    #\,)]
                [#\.        (token 'DOT      #\.)]
                [question   (token 'QUESTION   (string->symbol lexeme))]
                [dash       (token 'DASH       (string->symbol lexeme))]
                [op         (token 'OP         (string->symbol lexeme))]
                [identifier (token 'IDENTIFIER (string->symbol lexeme))]
                [integer    (token 'INTEGER    (string->number lexeme))]
                [decimal    (token 'DECIMAL    (string->number lexeme))]
                [#\"        (mode!-quote 'DQUOTE #\" stringer-I)]
                [#\'        (mode!-quote 'SQUOTE #\' stringer)]
                [#\`        (mode! grave-span        (token 'GRAVE))]
                [tickspace  (mode! grave-line        (token 'GRAVE))]
                [tickdent   (mode! grave-block (cons (token 'GRAVE)
                                                     (indent/dedent/newline (substring lexeme 1))))]
                [spacetabs  (unless-eof (token 'SPACE))]
                [dentspace  (unless-eof (stock 'WANT-TABS #\space end-pos -1))]))

(define (interper)
  (let ([prev-lexer _lexer]
        [braces 0])
    (main-lexer [#\{ (begin (update! braces add1)
                            (token 'LBRACE "{"))]
                [#\} (begin (if (> braces 0)
                                (update! braces sub1)
                                (mode! prev-lexer))
                            (token 'RBRACE "}"))])))

(define-macro (stringer (SPECIAL-CHARS ...) RULES ...)
  #'(base-lexer RULES ...
                [(:+ (:~ SPECIAL-CHARS ... #\newline)) (token 'STRING lexeme)]
                [any-char                              (token 'STRING lexeme)]))

(define-macro (stringer-I (SPECIAL-CHARS ...) RULES ...)
  #'(stringer (SPECIAL-CHARS ... #\`)
              RULES ...
              ["`{" (mode! (interper)
                           (list (token 'INTERP 0)
                                 (token 'LBRACE #\{)))]))

(define-macro (stringer-II (SPECIAL-CHARS ...) RULES ...)
  #'(let ([start-level (length _indents)])
      (stringer-I (SPECIAL-CHARS ...)
                  RULES ...
                  [(:seq spacetabs #\` dent)
                   (append (list (token 'STRING (car (string-split lexeme "`" #:trim? #f)))
                                 (token 'INTERP (- (length _indents) start-level)))
                           (indent/dedent/newline on-indent:(interper)))])))

(define (grave-block) (stringer-II ()))

(define (grave-line)
  (let ([prev-lexer _lexer])
    (stringer-II ()
                 [dent (mode! prev-lexer (indent/dedent/newline))])))

(define (grave-span)
  (let ([prev-lexer _lexer]
        [parens 0]
        [braces 0]
        [brackets 0])

    (define-macro (up COUNTER SYMBOL)
      #'(begin (update! COUNTER add1)
               (token SYMBOL)))

    (define-macro (down COUNTER SYMBOL)
      #'(begin (if (> COUNTER 0)
                   (update! COUNTER sub1)
                   (mode! prev-lexer))
               (token SYMBOL)))

    (define-macro (exit? SYMBOL)
      #'(if (> (+ parens braces brackets) 0)
            (token 'STRING lexeme)
            (mode! prev-lexer (token SYMBOL))))

    (stringer-II (#\( #\) #\{ #\} #\[ #\] #\space #\,)
                 [#\( (up parens 'LPAREN)]
                 [#\{ (up braces 'LBRACE)]
                 [#\[ (up brackets 'LBRACKET)]
                 [#\) (down parens 'RPAREN)]
                 [#\} (down braces 'RBRACE)]
                 [#\] (down brackets 'RBRACKET)]
                 [#\space (exit? 'SPACE)]
                 [#\,     (exit? 'COMMA)])))

(define-macro (mode!-quote XQUOTE CHAR LEXER)
  #'(let ([prev-lexer _lexer])
      (mode! (LEXER (CHAR)
                    [CHAR (mode! prev-lexer (token 'UNQUOTE))]
                    [dent (if (is-after XQUOTE)
                              (mode! (mode!-unquote XQUOTE prev-lexer)
                                     (indent/dedent/newline on-indent:(LEXER ())))
                              (indent/dedent/newline))]))
      (token XQUOTE)))

(define (mode!-unquote xquote lexer)
  (lambda (input-port)
    (define gen-token (lexer input-port))
    (mode! lexer (if (equal? gen-token (token xquote))
                     (token 'UNQUOTE)
                     gen-token))))

(define-macro (mode! LEXER EXPRS ...)
  #'(begin (set! _lexer LEXER)
           EXPRS ...))

(define-macro (update! COUNTER FN)
  #'(set! COUNTER (FN COUNTER)))

(define (debug msg x)
  (display msg)
  (display " ")
  (println x)
  x)

(define (token-type token)
  (cond
    [(srcloc-token? token) (token-type (srcloc-token-token token))]
    [(token-struct? token) (token-struct-type token)]))

(define (is-after types)
  (and (not (empty? _past-tokens))
       (member (token-type (car _past-tokens)) (enlist types))))

(define-macro (append-if COND ITEM)
  #'(if COND (list ITEM) empty))

(define (enlist x)
  (if (list? x) x (list x)))

(define _toque (list))
(define _past-tokens (list))
(define _indents (list))
(define _lexer (main-lexer))

(define (push-indent! lexer-next-level)
  (define token-INDENT (token 'INDENT (length _indents)))
  (push! _indents _lexer)
  (mode! lexer-next-level)
  token-INDENT)

(define (pop-indent!)
  (mode! (pop! _indents))
  (token 'DEDENT))

(define-macro (stock TYPE VALUE ANCHOR OFFSET)
  #'(srcloc-token (token TYPE VALUE)
                  (srcloc (srcloc-source lexeme-srcloc)
                          (position-line ANCHOR)
                          (+ (position-col ANCHOR) OFFSET)
                          (+ (position-offset ANCHOR) OFFSET)
                          (abs OFFSET))))

(define (produce-tokens input-port)
  (let*-values ([(eof?) (equal? eof (peek-char input-port))]
                [(line col pos) (port-next-location input-port)]
                [(tokens) (append (enlist (_lexer input-port))
                                  (append-if eof? (void)))]
                [(endline endcol endpos) (port-next-location input-port)])

    (define (stock token line col pos length)
      (srcloc-token token (srcloc (object-name input-port) line col pos length)))

    (define (to-srcloc-token t)
      (cond [(srcloc-token? t) t]
            [(token-struct? t) (if (equal? 'INDENT (token-struct-type t))
                                   (let* ([col (* 8 (token-struct-val t))]
                                          [pos (- endpos (/ (- endcol col) 8))])
                                     (stock (token 'INDENT #\tab) endline col pos 1))
                                   (stock t line col pos (- endpos pos)))]))
    (if (empty? tokens)
        (produce-tokens input-port)
        (map to-srcloc-token tokens))))

(define (bilang-lexer input-port)
  (cond [(empty? _toque) (set! _toque (produce-tokens input-port))])
  (push! _past-tokens (pop! _toque))
  (car _past-tokens))

(provide bilang-lexer)
