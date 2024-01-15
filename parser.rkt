#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3
       | exprB
@exprB : break2|break1|breakO|break
       | expr2
@expr2 : apply2
       | expl2
       | comma2|comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | ion
       | expr0
@expr0 : apply0
       | dot|slash
       | it
       | e
@exprl : expr1|comma1|commaO|comma
@exprL : exprl|break1|breakO|break
@exprI : exprl|commaI|applyI|explI

explI  :                 rad kwargs
       |  exprl /SPACE   rad kwargs?
       | (exprl /SPACE)? rad kwargs? spaceI
expl2  :                 rad kwargs
       |  exprL /SPACE   rad kwargs?
       | (exprL /SPACE)? rad kwargs? (space2 | /SPACE expr2)

commaO : @comma (group|dot|slash|symbol)+
breakO : @break (group|dot|slash|symbol)+
comma  : (commaO|comma1|expr1|atom) /SPACE? /COMMA
comma_ : (commaO|comma1|expr1|atom) /SPACE  /COMMA
break  : (breakO|break1)            /SPACE? /COMMA | exprB /NEWLINE /COMMA
break_ : (breakO|break1)            /SPACE  /COMMA | exprB /NEWLINE /COMMA
break1 : (@break|breakO) kwargs? space1 | @break_ (kv|exprO)
comma1 : (@comma|commaO) kwargs? space1 | @comma_ (kv|exprO)
commaI : (@comma|commaO) kwargs? spaceI | @comma_  kvI
comma2 : (@comma|commaO) kwargs? space2 | @comma_  kv2
break2 : (@break|breakO) kwargs? space2 | @break_  kv2
       |  exprB                              /feeds kv2
apply3 : (exprB|symbol) (/SPACE key /COLON)? /feeds expr3
apply2 :  exprO kwargs? space2
apply1 :  exprO kwargs? space1
applyI :  exprO kwargs? spaceI
applyO : anion group+
       | ion e group*
apply0 : expr0 (group|dot|slash|symbol)+
       | exprO string
       | symbol e
       | symbol? (int|dec) name
@e     :       int|dec|name
       |       string
       |       group

int    : INTEGER
dec    : DECIMAL
name   : IDENTIFIER PRIME?
symbol : (SYMBOL|DOLLAR|DASH|SLASH|QUESTION-MARK) PRIME?
rad    : symbol | anion | LPAREN RPAREN | LBRACE RBRACE
key    : @symbol | @name
dot    :    /DOT  (@name | /LPAREN @symbol /RPAREN)
slash  :    /SLASH @name
atom   : (@key COLON?)? anion | kv
kv     : @key /COLON        exprO
kvI    : @key /COLON /SPACE exprI
kv2    : @key /COLON /SPACE expr2
       | @key /COLON @indent
ion    :      /COLON @key
anion  :      /COLON
it     : /IT
str    : /QUOTE /INDENT  (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE /NEWLINE (STRING|interp|NEWLINE)*         /UNQUOTE
       | /QUOTE          (STRING|interp)*                 /UNQUOTE
string : GRAVE? @str
interp : INTERPOLATE (braces|indent)

parens   : /LPAREN (subexpr|atom) /RPAREN
braces   : /LBRACE  subexpr /RBRACE
brackets : /LBRACKET array? /RBRACKET
@group   :  parens | braces | brackets
@kwargs  : (/SPACE             kv)+
@space1  :  /SPACE (expr1     |kv)
@spaceI  :  /SPACE (applyI|kvI|kv)
@space2  :  /SPACE (apply2|kv2) | indent
@subexpr :  /SPACE? expr4 | symbol | indent /feeds
@array   :  /SPACE  exprI                        (/NEWLINE /SPACE? /COMMA  /SPACE exprI)*   /SPACE
         |          exprO (/SPACE exprO)*        (/NEWLINE                (/SPACE exprO)+)*
         | /INDENT /COMMA (/SPACE exprI|@indent) (/NEWLINE /COMMA (@indent|/SPACE exprI))*  /DEDENT /NEWLINE
indent   : /INDENT @expres /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
