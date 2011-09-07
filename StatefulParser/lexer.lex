datatype token = VAL | FUN | LPAREN | RPAREN | ID | EQ | NUM | PLUS | SEMI
type lexresult = (token * int) option

fun eof() = NONE

%%

%structure InternalLexer
digit = [0-9];
int = {digit}+;
alpha = [a-zA-Z];
id = {alpha}({alpha} | {digit})*;

%%

val => (SOME(VAL, yypos));
fun => (SOME(FUN, yypos));
"(" => (SOME(LPAREN, yypos));
")" => (SOME(RPAREN, yypos));
"=" => (SOME(EQ, yypos));
"+" => (SOME(PLUS, yypos));
";" => (SOME(SEMI, yypos));
" " | \n | \t => (lex());
{int} => (SOME(NUM, yypos));
{id}  => (SOME(ID, yypos));
