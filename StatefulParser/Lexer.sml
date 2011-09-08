use "lexer.lex.sml";

structure Lexer =
struct 
  datatype token = datatype InternalLexer.UserDeclarations.token
  val toks = [VAL, FUN, LPAREN, RPAREN, ID, EQ, NUM, PLUS, SEMI]
  fun tokToString VAL = "'val'"
    | tokToString FUN = "'fun'"
    | tokToString LPAREN = "'('"
    | tokToString RPAREN = "')'"
    | tokToString ID = "an identifier"
    | tokToString EQ = "'='"
    | tokToString NUM = "a number"
    | tokToString PLUS = "'+'"
    | tokToString SEMI = "';'"
  fun eq (t, t') = t = t'   

  fun make str = 
      let val sr = ref (SOME str)
	  fun get n = 
	      case !sr 
	       of NONE => ""
		| SOME(s) => (sr := NONE; s)
      in InternalLexer.makeLexer get end
end
(* :
sig
  type token
  val toks: token list
  val tokToString: token -> string
  val eq: (token * token) -> bool
  val make: string -> unit -> (token * int) option
end
*)
