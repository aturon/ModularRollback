signature PARSER = 
sig
  type token
  val exampleToks: token list
  
  type span = (int * int)
  type 'stream lexer = 'stream -> token * span * 'stream
  type result
  exception ParseError of span
  val parse: 'stream lexer -> 'stream -> result
end
