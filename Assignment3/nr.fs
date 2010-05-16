type term = (double * double)
type eqn = term list

(**
 * Given an equation and a value, solve, return dbl.
 *)
let solve eq v = 1.0

(**
 * Given an equation, diffentiate and return eq'
 *)
let dx ( eq : eqn ) = eq

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
   nr x (x - ((solve x y)/(solve (dx x) y))) 
   
