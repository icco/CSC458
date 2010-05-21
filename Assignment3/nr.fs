(**
 * Assignment 3
 * @author Nathaniel "Nat" Welch
 *)
module Ass3

type term = (double * double)
type poly = term list

// val findZeros : poly * double -> double

type event1 = bool
type event = int -> event1
type rv = event -> double

// val expectedVal : rv -> double -> double

// Define our exception type
exception StrEx of string

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
   List.rev (loop [] eq) // Order the variables the way we got them

(**
 * x is the equation we want to estimate
 * y is our guess
 *
 * basic equation:
 *  new_guess = guess - f(guess) / f'(guess)
 *)
let rec findZeros_ ((x : poly), (y : double)) i =
   let d = (dx x) // Take the derivative and store
   let ex = StrEx "This function has a derivative of zero."
   if i = 0 then y
   else
      if (d = []) then 
         raise ex
      else
         findZeros_ (x, (y - ((solve x y) / (solve d y)))) (i-1)

let rec findZeros ((x : poly), (y : double)) =
   findZeros_ (x, y) 1000

(**
 * Take in a random variable and a probability for heads and return the
 * expected value for that rv.
 * 
 * Assumes a discrete random variable
 *)
(* DOES NOT WORK...
let rec _expectedVal i ( r : rv ) ( d : double ) =
   if i = 1000 then 0.0
   else
      (d * (r i)) + (_expectedVal (i-1) (r) (d))

let expectedVal ( r : rv ) ( d : double ) =
   _expectedVal 100 r d
*)

(* Tests... *)
ignore ( solve : poly -> double -> double )
ignore ( dx : poly -> poly )
ignore ( findZeros : poly * double -> double )

//ignore ( expectedVal : rv -> double -> double )

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
   let f = (1.0, 2.0)::(-612.0, 0.0)::[]
   in
      printfn "x^2 - 612 : 24.73863375 ? %A" ( findZeros (f, 10.0) )
   let f = (1.0, 2.0)::(2.0, 5.0)::(-612.0, 0.0)::[]
   in
      printfn "x^2 + 2x^5 - 612 : 3.131419962 ? %A" ( findZeros (f, 10.0) );
      printfn "x^2 + 2x^5 - 612 : 3.131419962 ? %A" ( findZeros (f, 1.0) )
   let f = (6.0, 0.0)::[]
   in
      try
         printf "6 : exception ? " ;
         (printfn "6 : exception ? %A" ( findZeros (f, 10.0) ))
      with
         | exn ->
            printf " yes."
            //printf "Exception message: %s" exn.Message

// Uncomment to run tests
//test () ;;

