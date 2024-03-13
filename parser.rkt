#lang brag

expres : newline? expr3
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
       | exprO
@exprO : prop
       | expr0
@expr0 : apply0
       | dot
       | it
       | e
radL   :                                        rad kwargs
       |                        exprJ  /SPACE   rad kwargs?
       |                       (exprJ  /SPACE)? rad kwargs?  /SPACE (kvL|exprL)
rad2   :  (@break|breakO|break1)       /SPACE   rad kwargs? (/SPACE (kvL|exprL))?
       | ((@break|breakO|break1|exprJ) /SPACE)? rad kwargs? (/SPACE (kv2|apply2|comma2|rad2)|dent)

commaO :  @comma        (dot|op) (dot|op|string|group)*
breakO :  @break        (dot|op) (dot|op|string|group)*
comma1 : (@comma|commaO) kwargs? /SPACE (kv|expr1)          |           exprI /SPACE /COMMA (kv|expr0)
break1 : (@break|breakO) kwargs? /SPACE (kv|expr1)          | (break1|breakO) /SPACE /COMMA (kv|expr0)
commaL : (@comma|commaO) kwargs? /SPACE (kvL|applyL)        |           exprI /SPACE /COMMA  kvL
breakL : (@break|breakO) kwargs? /SPACE (kvL|applyL)        | (break1|breakO) /SPACE /COMMA  kvL | exprB newline kvL
comma2 : (@comma|commaO) kwargs? (/SPACE (kv2|apply2)|dent) |           exprI /SPACE /COMMA  kv2
break2 : (@break|breakO) kwargs? (/SPACE (kv2|apply2)|dent) | (break1|breakO) /SPACE /COMMA  kv2 | exprB newline kv2
comma  :  exprI /SPACE? /COMMA
break  :  exprB newline /COMMA                              | (break1|breakO) /SPACE? /COMMA
apply3 : (exprB|rad) newline expr3
apply2 :  exprO kwargs? (/SPACE (kv2|apply2)|dent)
applyL :  exprO kwargs? /SPACE (kvL|applyL)
apply1 :  exprO kwargs? /SPACE (kv|expr1)
apply0 :  expr0 (dot|op|string|group)+
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
prop   : @dot /COLON exprO
kv     : @key /COLON exprO
kvL    : @key /COLON /SPACE exprL
kv2    : @key /COLON (/SPACE (apply2|comma2|rad2)|dent)?
ion    :      /COLON
rad    : @op | COLON
key    : @op | @id
it     : /IT
string : /QUOTE str* /UNQUOTE
       | /GRAVE str+
       | /QUOTE blockquo /NEWLINE NEWLINE* /UNQUOTE
       | /GRAVE blockquo /NEWLINE NEWLINE*
@str   : STRING|blockquo|NEWLINE
       | @inter|interp|interpd
       | LPAREN str RPAREN
       | LBRACE str RBRACE
       | LBRACKET str RBRACKET

@group   : brackets|braces|parens
brackets : /LBRACKET array? /RBRACKET
braces   : /LBRACE subexpr /RBRACE
parens   : /LPAREN (subexpr|op) /RPAREN
slot     : /LPAREN @dot /COLON /RPAREN
inter    : /INTERP /LBRACE newline /RBRACE
interp   : /INTERP /LBRACE subexpr /RBRACE
interpd  : /INTERP dent
@kwargs  : (/SPACE kv)+
@subexpr : (/SPACE? apply2|dent) newline
         |  /SPACE? exprL /SPACE?
@array   :  /SPACE  exprL                     (newline /COMMA  /SPACE exprL)* /SPACE
         |          expr0 (/SPACE expr0)*     (newline  expr0 (/SPACE expr0)*)*
         |  indent /COMMA (/SPACE exprL|dent) (newline /COMMA (/SPACE exprL|dent))* /DEDENT newline
@dent    :  indent @expres /DEDENT
blockquo :  indent str+ /DEDENT
@indent  :  newline /INDENT
@newline : /NEWLINE+

error : /UNKNOWN-TOKEN
      | /WANT-TABS
