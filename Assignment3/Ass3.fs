(*
 * Assignment 3
 * @author Nathaniel "Nat" Welch
 * @author Jesse Tyler
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
 * Given an equation, differentiate and return eq'
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

(**--------- Part 2------------------*)

(**
* basic event, forces the model to go to the left
*)
let eAllTails (ts:int) = 
    false

(** fakes an event  used for testing*)
let fakeEvent (b:bool) =
    let retFun (i:int) =
        b
    retFun
        
(**
* trackEvent: event-> int -> event
* helper function to create an event where you can track what timestep you are
* on takes an event, updates the reference to the last timestep that the event
* was called.
*
* returns the event that it was given  
* a 'Trojan' event
*)
let trackEvent (ev:event) (lastTS:int ref) =
    let retTrackEvent (ts:int) =
        lastTS:= ts
        ev ts
    retTrackEvent

(**
* getChange: event ->int ->float
* returns the p and d values for the event
*)
let getChange (ev:event) (ts:int) (probH:float) =
    if((ev ts) = true) then
        probH
    else
        1.0 - probH

(**
* partialEV: event -> float -> int -> float
*
* takes in a given event, the probability of a head happening, the current
* timestep evaluates the event at the given timestep and produces the expected
* value of getting that value at that timestep
*)
let rec partialEV  (ev:event) (probH:float) (ts:int) = 
    let change = getChange ev ts probH
    match ts with 
    | 0 -> change
    | x -> change * (partialEV ev probH (ts - 1)) 


(**
* forceEvent: event -> int -> bool
*
* helper function that consumes an event and a timestep and returns a bool
* used to change the outcome of an event at a desired timestep. 
*)
let forceEvent (ev:event) (ts:int) = 
    let runForceEvent (nts:int) =
        let eval = (ev nts)
        if (ts = nts) then
            not eval
        else
            eval
    runForceEvent

(**
* expectedVal: rv-> float -> float
*
* tree traversal 
*
* Note: This has a few bugs... I don't know how to test this thing. So very
* confused on how this thing actually works
*)
let expectedVal (randv: rv) (probH:float) =
    let lastTS = ref(-1)
    let trojanEvent = trackEvent (eAllTails) (lastTS)

    let rec findExpectedVal (ev:event) =
        let valu = randv ev
        let ts = !lastTS
        let currEV = partialEV (ev) (probH) (ts)
        
        if((ev (ts-1))= true) then
            (valu * currEV)
        else
            let newTrojan = (trackEvent (forceEvent ev (ts-1)) lastTS )
            (valu * currEV) + (findExpectedVal newTrojan)

    findExpectedVal trojanEvent
    
(* Tests... *)
ignore ( solve : poly -> double -> double )
ignore ( dx : poly -> poly )
ignore ( findZeros : poly * double -> double )
ignore ( expectedVal : rv -> double -> double )
//ignore ( trackEvent: event -> int -> event1)

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

let test2 _ =
    let rts = ref -1
    printfn " "
    printfn " "
    printfn "----test trackEvent: "
    let d1 = (trackEvent (eAllTails) rts) 1
    printfn "ts = 1 ? %d" !rts
    let d2 = (trackEvent (eAllTails) rts) 55
    printfn "ts = 55 ? %d" !rts
    let d3 = (trackEvent (eAllTails) rts) -12
    printfn "ts = -12 ? %d" !rts
    let d4 = (trackEvent (eAllTails) rts) 10
    printfn "ts = 10 ? %d" !rts

    printfn "----test getChange"
    printfn "prob = .5"
    printfn "true  equals .5 ? %f" (getChange (fakeEvent true) 10 0.5)
    printfn "false  equals .5 ? %f" (getChange (fakeEvent false) 10 0.5)
    printfn "prob = .3"
    printfn "true  equals .3 ? %f" (getChange (fakeEvent true) 10 0.3)
    printfn "false  equals .7 ? %f" (getChange (fakeEvent false) 10 0.3)
    printfn "----test partialEV"
    printfn "event: eAllTails; ts = 0; prob = .5"
    printfn "expect .5 ? %f" (partialEV eAllTails 0.5 0)
    printfn "event: eAllTails; ts = 4; prob = .7 "
    printfn "expect .00243 ? %f" (partialEV eAllTails 0.7 4)
    
    
// Uncomment to run tests
//test () ;;
//test2 () ;;

