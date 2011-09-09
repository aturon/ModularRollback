%name InternalLexer;

%defs (
  datatype token = VAL | FUN | LPAREN | RPAREN | ID | EQ | NUM | PLUS | SEMI | EOF
  type lex_result = token
  fun eof() = EOF
);

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;

val => (VAL);
fun => (FUN);
"(" => (LPAREN);
")" => (RPAREN);
"=" => (EQ);
"+" => (PLUS);
";" => (SEMI);
{int} => (NUM);
{id}  => (ID);
" " | \n | \t => (continue());
