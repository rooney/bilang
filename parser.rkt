#lang brag

expres : newline? expr3
@expr3 : apply3
       | exprB
@exprB : breakI|breakL|break2
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

radL   : rad kwargs
       | Js rk
       | Js? rk sLx
rad2   : bIs rk sLx?
       | (bIs|Js)? rk (/SPACE (kv2|x2)|dent)

commaO : @comma dodo
breakO : @break dodo
comma1 : ck s1 | exprI sc0
break1 : bk s1 | break1O sc0
commaL : ck sL | exprI scL
breakL : bk sL | break1O scL | Bn kvL
comma2 : ck s2 | exprI sc2
break2 : bk s2 | break1O sc2 | Bn kv2
comma  : exprI sqc
break  : exprB nc | break1O sqc
apply3 : (exprB|rad) newline expr3
apply2 : Ok s2
applyL : Ok sL
apply1 : Ok s1
apply0 : expr0 dogs+
       | ion (op|e)
       | op e

@e     : slot
       | unit
       | num|id|gs
unit   : num id
int    : INTEGER
dec    : DECIMAL
id     : prefix? IDENTIFIER QUESTION? PRIME*
op     : (prefix|SLASH|PLUS|OP|QUESTION) PRIME*
       | LPAREN RPAREN
       | LBRACE RBRACE
       | DOT DOT
dot    : /DOT SLASH? id
       | /DOT /LPAREN @op /RPAREN
prop   : @dot /COLON exprO
kv     : k exprO
kvL    : k sXL
kv2    : k (/SPACE x2|dent)?
ion    : /COLON
rad    : @op | COLON
key    : @op | @id
it     : /IT
string : /QUOTE str* /UNQUOTE
       | /GRAVE str+
	   | /QUOTE bnn /UNQUOTE
       | /GRAVE bnn
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
@array   :  /SPACE  exprL (nc sXL)* /SPACE
         |          expr0 s0* (newline expr0 s0*)*
         | indent /COMMA sXd (nc sXd)* /DEDENT newline
@dent    : indent @expres /DEDENT
blockquo : indent str+ /DEDENT
@indent  : newline /INDENT
@newline : /NEWLINE+

@rk   : rad kwargs?
@Ok   : exprO kwargs?
@bk   : (@break|breakO) kwargs?
@ck   : (@comma|commaO) kwargs?
@nc   : newline /COMMA
@sqc  : /SPACE? /COMMA
@sc   : /SPACE /COMMA
@s0   : /SPACE expr0
@s1   : /SPACE (kv|expr1)
@s2   : /SPACE (kv2|apply2)|dent
@sL   : /SPACE (kvL|applyL)
@sLx  : /SPACE (kvL|exprL)
@sXL  : /SPACE exprL
@sXd  : sXL|dent
@scL  : sc kvL
@sc2  : sc kv2
@sc0  : sc (kv|expr0)
@Js   : exprJ /SPACE
@bIs  : breakI /SPACE
@k    : @key /COLON
@bnn  : blockquo /NEWLINE NEWLINE*
@num  : int|dec
@gs   : group|string
@dop  : dot|op
@dogs : dop|gs
@dodo : dop dogs*
@Bn   : exprB newline
@x2   : apply2|comma2|rad2
@breakI  : @break|break1O
@break1O : break1|breakO
@prefix  : DOLLAR|DASH

error : /UNKNOWN-TOKEN
      | /WANT-TABS
