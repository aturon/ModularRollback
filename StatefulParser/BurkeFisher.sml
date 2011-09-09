functor BurkeFisher (P: PARSER) =
struct
  open P (* we'll shadow result and parse *)

  (* instantiate Herman's pearl *)
  structure C = GreatEscape(type ans = P.result)

  type repair = token * span * token
  datatype result
    = REPAIR of repair
    | RESULT of P.result

  fun tryToks cpt [] = NONE
    | tryToks cpt (tok::toks) = 
      let val ((oldTok, span, strm'), k) = cpt
      in k (tok, span, strm');
	 SOME (oldTok, span, tok)	 
      end
      handle ParseError _ => tryToks cpt toks

  fun tryCpts [] = NONE
    | tryCpts (cpt::cpts) = 
        case tryToks cpt exampleToks
	 of NONE   => tryCpts cpts
	  | repair => repair

  fun parse lex strm = 
      let val cpts = ref []
	  fun push cpt = cpts := cpt :: !cpts
	  fun cptLex strm = 
	      let val lr = lex strm
	      in C.shift (fn k => (push (lr, k); k lr))
	      end
      in RESULT (C.reset (fn () => P.parse cptLex strm))
	 handle ParseError span => 
		case tryCpts (!cpts)
		 of SOME r => REPAIR r
		  | NONE   => raise ParseError span
      end

end
