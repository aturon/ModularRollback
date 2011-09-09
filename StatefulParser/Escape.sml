(* escape.sml [version 4]
 *
 * Copyright (c) 2007 David Herman
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *)

(*
 * This file contains the companion source code for the paper:
 *
 * David Herman. Functional Pearl: The Great Escape. In International
 *   Conference on Functional Programming (ICFP), October 2007.
 *   http://www.ccs.neu.edu/~dherman/research/papers/icfp07-great-escape.pdf
 *
 * @inproceedings{ Herman07:escape,
 *     author = {David Herman},
 *     title = {{F}unctional {P}earl: {T}he {G}reat {E}scape},
 *     booktitle = {International Conference on Functional Programming (ICFP)},
 *     location = {Freiburg, Germany},
 *     year = 2007,
 *     month = oct,
 * }
 *
 * The source code in this file is released under a permissive BSD license.
 *
 * For usage information, see the declarations of structures NEx, GEx1, GEx2,
 * and GEx3 at the bottom of this file.
 *)

(*
 * HISTORY:
 * 27 Aug 2007 - public release
 * 26 Sep 2007 - added GreatEscape2 functor
 * 01 Oct 2007 - created functions reify and reflect, based on an
 *               observation by Kevin Millikin
 * 04 Jun 2008 - fixed broken comments (thanks Vesa Karvonen)
 *             - created ex6 to demonstrate handlers on both sides of
 *               the border
 *             - Aborting.abort creates a sensible empty continuation
 *               (thanks Zena Ariola)
 *)

(* Functional representation of continuations. *)
signature ESCAPE =
sig
    type void
    val abort : (unit -> unit) -> 'a
    val coerce : void -> 'a
    val escape : (('a -> void) -> 'a) -> 'a
end;

(* Continuations that do not abort when captured (Section 5.3). *)
structure NonAborting : ESCAPE =
struct
    datatype void = VOID of void
    fun coerce (VOID v) = coerce v
    open SMLofNJ.Cont
    fun abort thunk = throw (isolate thunk) ()
    fun escape (f : ('a -> void) -> 'a) =
        callcc
            (fn k => (f (fn x => throw k x)))
end

(* Continuations that abort when captured (Section 3.1). *)
structure Aborting : ESCAPE =
struct
    datatype void = VOID of void
    fun coerce (VOID v) = coerce v
    open SMLofNJ.Cont
    fun abort thunk =
        let
            fun thunk' () =
                (thunk (); OS.Process.exit OS.Process.success)
                handle _ => (TextIO.print "missing reset\n";
                             OS.Process.exit OS.Process.failure)
        in
            throw (isolate thunk') ()
        end
    fun escape f =
        callcc
            (fn k => abort (fn () => (f (fn x => throw k x); ())))
end;

(* Delimited continuations. *)
signature CONTROL =
sig
    type ans
    val shift : (('a -> ans) -> ans) -> 'a
    val reset : (unit -> ans) -> ans
end;


(* ========================================================================== *)

(* A naive implementation of delimited continuations. *)
functor Naive (structure E : ESCAPE
               type ans) : CONTROL =
struct
    open E
    exception MissingReset
    type ans = ans

    val mk : (ans -> void) ref = ref (fn _ => raise MissingReset)
    fun return x = coerce (!mk x)

    fun reset thunk =
        escape (fn k => let val m = !mk
                        in
                            mk := (fn r => (mk := m; k r));
                            return (thunk ())
                        end)

    fun shift f =
        escape (fn k => return (f (fn v => reset (fn () => coerce (k v)))))
end;


(* ========================================================================== *)

(* Delimited continuations implemented with The Great Escape.
 *
 * This code is copied directly from Figure 5 in the paper, almost verbatim.
 *)
functor GE (structure E : ESCAPE
            type ans) : CONTROL =
struct
    open E
    exception MissingReset
    type ans = ans

    datatype tunneled = SUCCESS of ans
                      | FAILURE of exn

    val mk : (tunneled -> void) ref = ref (fn _ => raise MissingReset)

    fun return x = coerce (!mk x)

    fun reset thunk =
        (case escape (fn k => let val m = !mk
                              in
                                  mk := (fn r => (mk := m; k r));
                                  return (SUCCESS (thunk ())
                                          handle x => FAILURE x)
                              end) of
             SUCCESS v => v
           | FAILURE x => raise x)

    fun shift f =
        escape (fn k => return (SUCCESS (f (fn v => reset (fn () => coerce (k v))))
                                handle x => FAILURE x))
end;


(* ========================================================================== *)

(* Delimited continuations implemented with The Great Escape.
 *
 * This version is based off of a refactoring I discovered when preparing
 * slides for my ICFP talk. I find it much clearer and easier to understand.
 * Note, however, that the correctness proof corresponds to version 1.
 *
 * I have also discovered a change for better space efficiency that I missed
 * in the paper. With an additional use of `abort' in the definition of
 * `shift' (see below), I conjecture we get the same asymptotic space
 * efficiency as the specification semantics.
 *
 * This additional `abort' would work fine in the GreatEscape functor as
 * well, but I leave that implementation as is for comparison.
 *)
functor GreatEscape2 (structure E : ESCAPE
                      type ans) : CONTROL =
struct
    open E
    exception MissingReset
    type ans = ans

    datatype tunneled = SUCCESS of ans
                      | FAILURE of exn

    val mk : (tunneled -> void) list ref = ref []

    fun push k =
        mk := k::(!mk)

    fun pop () =
        (case !mk of
             [] => raise MissingReset
           | k::ks => (mk := ks; k))

    fun reflect x =
        (case x of
             SUCCESS v => v
           | FAILURE exn => raise exn)

    fun reify thunk =
        (SUCCESS (thunk ()))
        handle x => FAILURE x

    fun reset thunk =
        reflect
        (escape (fn k => (push k;
                          let val result = reify thunk
                              val return = pop()
                          in
                              coerce (return result)
                          end)))

    fun shift f =
        escape (fn k =>
                   let val captured = fn v => reset (fn () => coerce (k v))
                   in
                       (* Note the additional abort. *)
                       abort (fn () =>
                                 let val result = SUCCESS (f captured)
                                                  handle x => FAILURE x
                                     val return = pop ()
                                 in
                                     coerce (return result)
                                 end)
                   end)

end;

(* The naive implementation of delimited continuations. *)
structure NImpl = Naive(structure E = NonAborting type ans = string);

(* The Great Escape, without proper space efficiency. *)
structure GImpl1 = GE(structure E = NonAborting type ans = string);

(* The Great Escape with proper space efficiency. *)
structure GImpl2 = GE(structure E = Aborting type ans = string);

(* The Great Escape with an easier-to-understand implementation. *)
structure GImpl3 = GreatEscape2(structure E = Aborting type ans = string);


functor GreatEscape(type ans) = GE(structure E = Aborting type ans = ans)
