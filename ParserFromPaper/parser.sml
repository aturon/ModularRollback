use "escape.sml";

signature TOK = 
sig
    type tok
    val toks : tok list
    val eq : tok * tok -> bool
    val toString : tok -> string
end

structure Tok = 
struct
    datatype tok = VAL | FUN | LPAREN | RPAREN | ID | EQ | NUM | PLUS | SEMI
    val toks = [VAL, FUN, LPAREN, RPAREN, ID, EQ, NUM, PLUS, SEMI]

    fun toString VAL = "'val'"
      | toString FUN = "'fun'"
      | toString LPAREN = "'('"
      | toString RPAREN = "')'"
      | toString ID = "identifier"
      | toString EQ = "'='"
      | toString NUM = "number"
      | toString PLUS = "'+'"
      | toString SEMI = "';'"
    fun eq (t, t') = t = t' 
end

signature LEXER = 
sig
    type tok
    type tok_stream
    val lex : tok_stream 
	   -> (tok * tok_stream) option	    
    val pos : tok_stream -> int
end

signature PARSER = 
sig
    type tok
    type result

    functor ForLexer (L : LEXER where type tok = tok) :
    sig
	exception ParseError of L.tok_stream list
	val parse : L.tok_stream -> result
    end
end

structure Parser = 
struct

  type tok = Tok.tok
  type result = int

  functor ForLexer (L : LEXER where type tok = tok) = 
  struct
      exception ParseError of L.tok_stream list

      fun want t = 
       fn s => 
	  case L.lex s
	   of NONE => raise ParseError [s]
	    | SOME (t', s') => if Tok.eq (t, t') 
			       then s'
			       else raise ParseError [s']

      infix <|>
      fun p <|> q = 
	fn s => p s handle ParseError ss => 
		q s handle ParseError ss' => raise ParseError (ss @ ss')

      infix >>
      fun p >> q = 
       fn s => q (p s)

      open Tok

      val parseExp = want NUM >> want PLUS >> want NUM

      val parseDecl =
      	  (want FUN >> want ID >> want LPAREN >> want ID >> want RPAREN >> want EQ >> parseExp >> want SEMI) <|>
      	  (want VAL >> want ID >> want EQ >> parseExp >> want SEMI)

      fun parse s = L.pos (parseDecl s)
  end
end

structure SimpleLexer =
struct

    type tok = Tok.tok
    type tok_stream = Tok.tok list * int
    fun lex ([], _) = NONE
      | lex (t::ts, i) = SOME (t, (ts, i+1))
    fun pos (_, i) = i
		      
end

structure P = Parser.ForLexer (SimpleLexer)

signature WINDOW =
sig
    type 'a window
    val empty : 'a window
    val push  : 'a window -> 'a -> 'a window
    val list  : 'a window -> 'a list
end

structure Window : WINDOW = 
struct

    type 'a window = 'a list
    val empty = []
    fun push xs x = x :: xs
    fun list xs = xs
    
end

funsig WRAPPED_LEXER (C : CONTROL) (L : LEXER) = LEXER where type tok = L.tok

functor WrappedLexer(C : CONTROL) (L : LEXER) =
struct

    type tok = L.tok

    type checkpoint = L.tok -> C.ans
    datatype wrapper
      = CHECKPOINT of checkpoint Window.window
      | PASSTHRU 

    type tok_stream = L.tok_stream * wrapper

    fun lex (s, PASSTHRU) = 
	(case L.lex s
	  of NONE => NONE
	   | SOME (t, s') => SOME (t, (s', PASSTHRU)))

      | lex (s, CHECKPOINT w) = 
	(case L.lex s
	  of NONE => NONE
	   | SOME (t, s') => SOME (C.shift (fn k =>
	       k (t, (s', CHECKPOINT (Window.push w 
		            (fn t' => k (t', (s', PASSTHRU)))))))))

    fun pos (s, _) = L.pos s

    exception NotCheckpointing

    fun wrap s = (s, CHECKPOINT Window.empty)
    fun unwrap ((s, _) : tok_stream) = s
    fun checkpoints (_, CHECKPOINT w) = Window.list w
      | checkpoints (_, PASSTHRU) = raise NotCheckpointing

end

functor WL : WRAPPED_LEXER = WrappedLexer

functor BurkeFisher (T : TOK) 
                    (P : PARSER where 
                           type tok = T.tok) =
struct
  type tok = T.tok
  type repair = string
  type result = P.result * string option

  structure C = GreatEscape2(structure E = Aborting type ans = P.result)

  functor ForLexer (L : LEXER 
                    where type tok = tok) =
  struct
    exception ParseError of L.tok_stream list
    structure WL = WrappedLexer (C) (L)
    structure UP = P.ForLexer (WL)

    fun tryReps k []      = NONE
      | tryReps k (t::ts) = SOME (k t, t) 
          handle UP.ParseError _ => tryReps k ts

    fun tryCPs []      = NONE
      | tryCPs (k::ks) = case tryReps k T.toks
                          of NONE    => tryCPs ks
                           | SOME rt => SOME rt

    fun parse us = 
      (C.reset (fn () => 
         UP.parse (WL.wrap us)), NONE)
      handle UP.ParseError wss => 
        case tryCPs (List.concat 
                      (map WL.checkpoints wss)) 
         of NONE => 
              raise ParseError (map WL.unwrap wss)
          | SOME (r, t) => 
              (r, SOME (concat ["Did you mean ", 
                                T.toString t, 
                                "?\n"]))
  end
end

structure RP = BurkeFisher (Tok) (Parser)
structure RPP = RP.ForLexer (SimpleLexer)

open Tok
open RPP

val good = ([VAL, ID, EQ, NUM, PLUS, NUM, SEMI], 0)
val bad = ([VAL, ID, LPAREN, ID, RPAREN, EQ, NUM, PLUS, NUM, SEMI], 0)
