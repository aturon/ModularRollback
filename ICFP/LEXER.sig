signature LEXER = 
sig
  type stream
  type token
  type span = int * int
  type lex_result = token * span * stream

  val mkStream: string -> stream
  val lex: stream -> lex_result
end
