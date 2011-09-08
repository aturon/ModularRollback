use "lexer.sml";

structure Parser = 
struct
  exception ParseError of int option
  local open Lexer in
  fun parse lex =
    let fun want tok = 
	    case lex()
	     of NONE => raise ParseError NONE
	      | SOME(tok',pos) => 
		if Lexer.eq(tok, tok') then ()
		else raise ParseError(SOME pos)
	fun wants [] = ()
	  | wants (tok::toks) = (want tok; wants toks)

	fun parseExp() = wants[NUM, PLUS, NUM]
	fun parseDecl() =
	    case lex()
	     of NONE => ()
	      | SOME(tok, pos) =>
		(case tok
		  of VAL => 
		     (wants [ID, EQ]; parseExp(); want SEMI; parseDecl())
		   | FUN => 
		     (wants [ID, LPAREN, ID, RPAREN, EQ]; parseExp(); want SEMI; parseDecl())
		   | _ => raise ParseError (SOME pos)
		(* end case *))
    in parseDecl() end
  end
end
