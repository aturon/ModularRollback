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
