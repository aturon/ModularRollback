structure Parser = 
struct
(*
      val parseExp = want NUM >> want PLUS >> want NUM

      val parseDecl =
      	  (want FUN >> want ID >> want LPAREN >> want ID >> want RPAREN >> want EQ >> parseExp >> want SEMI) <|>
      	  (want VAL >> want ID >> want EQ >> parseExp >> want SEMI)
*)
  exception ParseError of int
  fun parse lex =
    let fun want tok 

	fun parseExp = 
      
end
