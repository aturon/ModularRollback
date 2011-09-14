structure Main =
struct
  fun println str = (print str; print "\n")

  fun repeat x 0 = []
    | repeat x n = x :: repeat x (n-1)

  fun underline (from, to) = 
      (implode (repeat #" " (from - 1))) ^ 
      (implode (repeat #"^" (to - from)))

  fun tryParse string = 
      SOME(Parser.parse Lexer.lex (Lexer.mkStream string))
      handle Parser.ParseError span =>
        (println "";
	 println "Syntax error:";
	 println string;
	 println (underline span);
	 println "";
	 NONE)

  structure P = BurkeFisher(Parser)

  fun withRepair string =       
      (case P.parse Lexer.lex (Lexer.mkStream string)
	of P.REPAIR (t, span, t') =>
	   (println "";
	    println "Syntax error:";
	    println string;
	    println (underline span);
	    println ("Did you mean " ^ Lexer.tokToString t' ^ "?");
	    println "")
	 | _ => println "Parse succeeded")
      handle P.ParseError span => println("Unrepairable error")

	 
end
