#lang brag

expres : /NEWLINE? expr3
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

commaO : @comma (group|dot|slash|op)+
breakO : @break (group|dot|slash|op)+
comma  : (commaO|comma1|expr1|atom) /SPACE? /COMMA
comma_ : (commaO|comma1|expr1|atom) /SPACE  /COMMA
break  : (breakO|break1)            /SPACE? /COMMA | exprB /NEWLINE /COMMA
break_ : (breakO|break1)            /SPACE  /COMMA | exprB /NEWLINE /COMMA
break1 : (@break|breakO) kwargs? space1 | @break_ (kv|exprO)
comma1 : (@comma|commaO) kwargs? space1 | @comma_ (kv|exprO)
commaI : (@comma|commaO) kwargs? spaceI | @comma_  kvI
comma2 : (@comma|commaO) kwargs? space2 | @comma_  kv2
break2 : (@break|breakO) kwargs? space2 | @break_  kv2
       |  exprB                              /NEWLINE kv2
apply3 : (exprB|op) (/SPACE key /COLON)? /NEWLINE expr3
apply2 :  exprO kwargs? space2
apply1 :  exprO kwargs? space1
applyI :  exprO kwargs? spaceI
applyO :  anion group+
       |  ion e group*
apply0 :  expr0 (group|dot|slash|op)+
       | (exprO|id) string
       | op e
       | op? (int|dec) id
@e     :      int|dec|id|string
       |      group


int    : INTEGER
dec    : DECIMAL
id     : (DASH? DASH?|PLUS?) IDENTIFIER QUESTION? PRIME?
op     : (OP|DOLLAR|PLUS|SLASH|DASH|QUESTION) PRIME?
rad    : op | anion | LPAREN RPAREN | LBRACE RBRACE
key    : @op | @id
slash  : /SLASH @id
dot    : /DOT (@id | /LPAREN @op /RPAREN)
atom   : (@key COLON?)? anion | kv
kv     : @key /COLON        exprO
kvI    : @key /COLON /SPACE exprI
kv2    : @key /COLON /SPACE expr2
       | @key /COLON block
ion    :      /COLON @key
anion  :      /COLON
it     : /IT

newline : NEWLINE
string : /DQUOTE (si|sz)* /UNQUOTE
       | /DQUOTE @sb /NEWLINE /UNQUOTE
sb     : @indent (STRING|interp|newline|sb)* /DEDENT
interp : /INTERP (@braces|block)
@s1    :          STRING*
@s2    : @indent  STRING*        /DEDENT
@si    :          (STRING|interp)*
@sz    : @indent  (STRING|interp)* /DEDENT
@se    : /NEWLINE (STRING|interp)*

@group   : parens|braces|brackets
parens   : /LPAREN subexpr /RPAREN
braces   : /LBRACE subexpr /RBRACE
brackets : /LBRACKET array? /RBRACKET
@kwargs  : (/SPACE kv)+
@space1  :  /SPACE (kv|expr1)
@spaceI  :  /SPACE (kv|kvI|applyI)
@space2  :  /SPACE (   kv2|apply2) | block
@subexpr :  /SPACE? exprI          | block /NEWLINE
@array   :  /SPACE  exprI                      (/NEWLINE /COMMA  /SPACE exprI)*        /SPACE
         |          exprO (/SPACE exprO)*      (/NEWLINE  exprO (/SPACE exprO)*)*
         | @indent /COMMA (/SPACE exprI|block) (/NEWLINE /COMMA (/SPACE exprI|block))* /DEDENT /NEWLINE
@block   : @indent @expres /DEDENT
@indent  : /NEWLINE /INDENT

error : UNKNOWN-TOKEN
      | WANT-TABS
