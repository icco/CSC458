(**
 * Chutes and Ladders Player Interface
 *
 * @author Nat Welch
 * @author Mark Gius
 *)

(**
 * Player Class
 * 
 * abstract shouldDouble : int -> int -> bool
 * abstract shouldTake : int -> int -> bool
 *)
[<AbstractClass>]
type Player () =
   abstract shouldDouble : int -> int -> bool
   abstract shouldTake : int -> int -> bool

