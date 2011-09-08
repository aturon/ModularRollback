struct
  datatype    checkPt = CP of token -> result option reified
       and 'a reified = WRITER of (checkPt list) -> 'a * checkPt list

  type checkPt = token -> result option
  type 'a M = (checkPt list) -> 'a * checkPt list

  fun unit x = fn cpts => (x, cpts)

  (* this is doing monadic bind! *)
  fun wrapLex lex strm = shift (fn k =>
      fn cpts => k (lex strm) (CP k :: cpts))

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
      case reset (fn () => unit (P.parse (wrapLex lex))) []
       of (SOME r, _)  => Accepted r
	| (NONE, cpts) => tryCpts cpts

end
