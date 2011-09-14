structure Parser: PARSER = 
struct
  open Lexer

  type span = (int * int)
  type result = unit
  type 'stream lexer = 'stream -> token * span * 'stream 
  exception ParseError of span

  fun max (s1, e1) (s2, e2) = 
      if (s2 > s1) then (s2, e2) else (s1, e1)

  infix <|>
  fun p <|> q = 
   fn s => p s handle ParseError span => 
           q s handle ParseError span' => 
	   raise ParseError (max span span')

  infix >>
  fun p >> q = fn s => q (p s)

  fun parse lex strm = 
      let fun want t s = 
	      let val (t', span, s') = lex s
	      in if eq (t, t') then s'
		 else raise ParseError span
	      end
	  val parseExp = want NUM >> want PLUS >> want NUM
	  val parseDecl =
      	      (want FUN >> want ID >> want LPAREN >> 
	       want ID >> want RPAREN >> want EQ >> parseExp >> want SEMI) <|>
              (want VAL >> want ID >> want EQ >> parseExp >> want SEMI)
      in (parseDecl strm; ()) end

end
