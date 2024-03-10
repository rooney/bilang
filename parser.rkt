#lang brag

expres : /NEWLINE? expr3
@expr3 : apply3
       | exprB
@exprB : break|breakO|break1|breakL|break2
       | expr2
@expr2 : apply2|comma2|rad2
       | exprL
@exprL : applyL|commaL|radL
       | exprJ
@exprJ : comma
       | exprI
@exprI : commaO|comma1
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : quanta
       | exprO
@exprO : mono
       | expr0
@expr0 : apply0
       | dot
       | e
radL   :                                        rad kwargs
       |                        exprJ  /SPACE   rad kwargs?
       |                       (exprJ  /SPACE)? rad kwargs?  /SPACE (kvL|exprL)
rad2   :  (@break|breakO|break1)       /SPACE   rad kwargs? (/SPACE (kvL|exprL))?
       | ((@break|breakO|break1|exprJ) /SPACE)? rad kwargs? (/SPACE (kv2|apply2|comma2|rad2)|dent)

comma  : exprI /SPACE? /COMMA
break  : exprB /NEWLINE /COMMA                              | (break1|breakO) /SPACE? /COMMA
break1 : (@break|breakO) kwargs? /SPACE (kv|expr1)          | (break1|breakO) /SPACE /COMMA (kv|expr0)
comma1 : (@comma|commaO) kwargs? /SPACE (kv|expr1)          |           exprI /SPACE /COMMA (kv|expr0)
commaL : (@comma|commaO) kwargs? /SPACE (kvL|applyL)        |           exprI /SPACE /COMMA  kvL
breakL : (@break|breakO) kwargs? /SPACE (kvL|applyL)        | (break1|breakO) /SPACE /COMMA  kvL | exprB /NEWLINE kvL
break2 : (@break|breakO) kwargs? (/SPACE (kv2|apply2)|dent) | (break1|breakO) /SPACE /COMMA  kv2 | exprB /NEWLINE kv2
comma2 : (@comma|commaO) kwargs? (/SPACE (kv2|apply2)|dent) |           exprI /SPACE /COMMA  kv2
commaO :  @comma (dot|op) (dot|op|string|group)*
breakO :  @break (dot|op) (dot|op|string|group)*
apply3 : (exprB|rad) /NEWLINE expr3
apply2 :  exprQ kwargs? (/SPACE (kv2|apply2)|dent)
applyL :  exprQ kwargs? /SPACE (kvL|applyL)
apply1 :  exprQ kwargs? /SPACE (kv|expr1)
apply0 :  expr0 (string|group)+
       |  exprO (dot|op)+
       |  ion (op|e)
       |  op e
@e     :  slot
       |  unit
       |  int|dec|id|string|group
unit   : (int|dec) id
int    :  INTEGER
dec    :  DECIMAL
id     : (DOLLAR|DASH)? IDENTIFIER QUESTION? PRIME*
op     : (DOLLAR|DASH|SLASH|PLUS|OP|QUESTION) PRIME*
       |  LPAREN RPAREN
       |  LBRACE RBRACE
       |  DOT DOT
dot    : /DOT SLASH? id
       | /DOT /LPAREN @op /RPAREN
quanta : @dot /COLON exprQ
kv     : @key /COLON exprQ
kvL    : @key /COLON /SPACE exprL
kv2    : @key /COLON (/SPACE (apply2|comma2|rad2)|dent)?
ion    :      /COLON
rad    : @op | COLON
key    : @op | @id
mono   : /MONO

string : /SQUOTE STRING? /UNQUOTE
       | /DQUOTE (si|sz)* /UNQUOTE
       | /DQUOTE @sb /NEWLINE /UNQUOTE
sb     : @indent (STRING|interp|NEWLINE|sb)* /DEDENT
interp : /INTERP (@braces|dent)
@s1    :          STRING*
@s2    : @indent  STRING*          /DEDENT
@si    :          (STRING|interp)*
@sz    : @indent  (STRING|interp)* /DEDENT
@se    : /NEWLINE (STRING|interp)*

@group   : brackets|braces|parens
brackets : /LBRACKET array? /RBRACKET
braces   : /LBRACE subexpr /RBRACE
parens   : /LPAREN (subexpr|op) /RPAREN
slot     : /LPAREN @dot /COLON /RPAREN
@kwargs  : (/SPACE kv)+
@subexpr : (/SPACE? apply2|dent) /NEWLINE
         |  /SPACE? exprL /SPACE?
@array   :  /SPACE  exprL                     (/NEWLINE /COMMA  /SPACE exprL)* /SPACE
         |          exprQ (/SPACE expr0)*     (/NEWLINE  expr0 (/SPACE expr0)*)*
         | @indent /COMMA (/SPACE exprL|dent) (/NEWLINE /COMMA (/SPACE exprL|dent))* /DEDENT /NEWLINE
@dent    : @indent @expres /DEDENT
@indent  : /NEWLINE /INDENT

error : UNKNOWN-TOKEN
      | WANT-TABS
