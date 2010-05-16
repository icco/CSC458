
type term = (double * double)
type poly = term list

(**
 * Given an equation and a value, solve, return dbl.
 *)
let solve ( eq : poly ) ( v : double ) = 1.0

(**
 * Given an equation, diffentiate and return eq'
 *)
let dx ( eq : poly ) =
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
let rec findZeros ((x : poly), (y : double)) =
   (findZeros (x, (y - ((solve x y) / (solve (dx x) y)))))

(* Tests... *)
ignore ( solve : poly -> double -> double )
ignore ( dx : poly -> poly )
ignore ( findZeros : poly * double -> double )

