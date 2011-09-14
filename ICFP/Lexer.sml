structure Lexer =
struct 
  datatype token = datatype InternalLexer.UserDeclarations.token
  val exampleToks = [VAL, FUN, LPAREN, RPAREN, ID, EQ, NUM, PLUS, SEMI]
  fun tokToString VAL = "'val'"
    | tokToString FUN = "'fun'"
    | tokToString LPAREN = "'('"
    | tokToString RPAREN = "')'"
    | tokToString ID = "an identifier"
    | tokToString EQ = "'='"
    | tokToString NUM = "a number"
    | tokToString PLUS = "'+'"
    | tokToString SEMI = "';'"
    | tokToString EOF = "<end of file>"
  fun eq (t, t') = t = t'

  type span = int * int
  type stream = InternalLexer.strm * AntlrStreamPos.sourcemap
  type lex_result = token * span * stream

  fun mkStream s = (InternalLexer.streamify (
		    let val first = ref true
		    in fn () => if !first then (first := false; s) else ""
		    end), 
		    AntlrStreamPos.mkSourcemap())

  fun lex (strm, sm) = 
      let val (tok, (p1, p2), strm') = InternalLexer.lex sm strm
      in (tok, 
	  (AntlrStreamPos.colNo sm p1,
	   AntlrStreamPos.colNo sm p2),
	  (strm',sm))
      end

end
