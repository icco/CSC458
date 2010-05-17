(**
 * Assignment 3
 * @author Nathaniel "Nat" Welch
 *)

type event1 = bool
type event = int -> event1
type rv = event -> double // a random variable
type term = (double * double)
type poly = term list

(**
 * Given an equation and a value, solve, return a double.
 *)
let solve ( eq : poly ) ( v : double ) =
   List.fold (fun acc (f,e) -> (acc + (f * (v ** e)))) 0.0 eq

(**
 * Given an equation, diffentiate and return eq'
 *)
let dx ( eq : poly ) =
   let rec loop acc = function
      | [] -> acc
      | ((f, e)::t) -> if e > 0.0 then (loop (((f*e), (e - 1.0))::acc) t); else acc
   List.rev (loop [] eq)

(**
 * x is the equation we want to estimate
 * y is our guess
 *
 * basic equation:
 *  new_guess = guess - f(guess) / f'(guess)
 * 
 * TODO: Figure out when to stop recursing...
 *)
let rec findZeros_ ((x : poly), (y : double)) i =
   if i = 0 then y
   else
      findZeros_ (x, (y - ((solve x y) / (solve (dx x) y)))) (i-1)

let rec findZeros ((x : poly), (y : double)) =
   findZeros_ (x, y) 1000

(**
 * Take in a random variable and a probability for heads and return the
 * expected value for that rv.
 *)
let expectedVal ( r : rv ) ( d : double ) =
   1.0

(* Tests... *)
ignore ( solve : poly -> double -> double )
ignore ( dx : poly -> poly )
ignore ( findZeros : poly * double -> double )

ignore ( expectedVal : rv -> double -> double )

let test _ =
   printfn "--- Test solve: "
   printfn "equals 144 ? %.0f" (solve ((1.0,2.0)::[]) 12.0)
   printfn "equals 12  ? %.0f" (solve ((1.0,2.0)::(4.0,1.0)::[]) 2.0)
   printfn "equals 62  ? %.0f" (solve ((1.0,5.0)::(1.0,4.0)::(1.0,3.0)::(1.0,2.0)::(1.0,1.0)::[]) 2.0)
   printfn "--- Test dx: "
   printfn "equals 2x? %A" (dx ((1.0, 2.0)::[]))
   printfn "equals  1? %A" (dx ((1.0, 1.0)::[]))
   printfn "equals  0? %A" (dx ((4.0, 0.0)::[]))
   printfn "equals 3x2 + 2x + 4? %A" (dx ((1.0, 3.0)::(1.0, 2.0)::(4.0, 1.0)::[]))
   printfn "--- Test findZeros: "




test () ;;

