
type term = (double * double)
type eqn = term list

(**
 * Given an equation and a value, solve, return dbl.
 *)
let solve ( eq : eqn ) ( v : double ) = 1.0

(**
 * Given an equation, diffentiate and return eq'
 *)
let dx ( eq : eqn ) =
   let rec loop acc = function
      | [] -> acc
      | ((f, e)::t) -> if e > 0.0 then (loop (((f*e), (e - 1.0)) :: acc) t); else acc
   loop [] eq

(**
 * x is the equation we want to estimate
 * y is our guess
 *
 * basic equation:
 *  new_guess = guess - f(guess) / f'(guess)
 * 
 * TODO: Figure out when to stop recursing...
 *)
let rec nr (x : eqn) (y : double) =
   (nr x (y - ((solve x y) / (solve (dx x) y))))


(* Tests... *)
ignore ( solve : eqn -> double -> double ) 
ignore ( dx : eqn -> eqn ) 
ignore ( nr : eqn -> double -> double ) 

