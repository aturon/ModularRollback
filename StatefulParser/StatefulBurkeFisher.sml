struct
  type checkPt = token -> result option

  fun tryToks cpt [] = NONE
    | tryToks cpt (tok::toks) = 
        case cpt tok 
	 of NONE        => tryToks cpt toks
	  | SOME result => 

  fun tryCpts [] => Unrepairable
    | tryCpts (cpt::cpts) = 
        case tryToks cpt P.toks
	 of NONE        => tryCpts cpts
	  | SOME repair => RepairableBy repair

  fun parse lex = 
      let val cpts = ref [] 
	  fun wrapLex lex strm = shift (fn k =>
					fn cpts => k (lex strm) (CP k :: cpts))
      in case reset (fn () => P.parse (wrapLex lex))
	  of SOME r => Accepted r
	   | NONE   => tryCpts !cpts

end
