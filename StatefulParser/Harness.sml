structure Main =
struct
  fun println str = (print str; print "\n")

  fun repeat x 0 = []
    | repeat x n = x :: repeat x (n-1)

  fun underline (from, to) = 
      (implode (repeat #" " (from - 1))) ^ 
      (implode (repeat #"^" (to - from)))

  structure P = BurkeFisher(Parser)

  fun tryParse string = 
      SOME(P.parse Lexer.lex (Lexer.mkStream string))
      handle P.ParseError span =>
        (println "";
	 println "Syntax error:";
	 println string;
	 println (underline span);
	 println "";
	 NONE)

  fun withRepair string = 
      case tryParse string 
       of SOME(P.REPAIR (t, span, t')) =>         
	  (println "";
	   println "Syntax error:";
	   println string;
	   println (underline span);
	   println ("Did you mean " ^ Lexer.tokToString t' ^ "?");
	   println "")
	| _ => println "Parse succeeded"
	 
end
