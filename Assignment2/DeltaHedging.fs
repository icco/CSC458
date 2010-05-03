#light

(**
 * @author Nathaniel "Nat" Welch
 *)

module DeltaHedging

(* required types *)
type event1 = bool
type event = int -> event1
type rv = event -> double
type rvseq = int -> rv
type option = double -> double

module DeltaHedging =
   let rec loopRand ( r : System.Random ) x =
      if x = 0 then
         r.Next(1)
      else
         let k = r.Next(1);
         in
            loopRand r ( x - 1 )

   let makeERandom () ( x : int ) = 
      let s = System.Random().Next()
      let r = System.Random(s)
      let mutable a = 0
      in
         ((loopRand r x) = 0)

   let makeERandP ( l : double ) =
      makeERandom ()

   let makeERandT ( l : double ) =
      makeERandom ()

(* required definitions *)
(*
(eAllHeads : event)
(eAllTails : event)
(eAlternating : event)
(makeERandom : unit -> event)
(makeERandP : double -> event)
(makeERandT : unit -> event)
(forceEParts : int -> bool array -> event -> event)
(doubleToRV : double -> rv)
(rvNCountHeads : int -> rv)
(rvNCountTails : int -> rv)
(rvNStock : double -> double -> double -> rvseq)
(rvPathD : rvseq)
(unaryLiftRV : (double -> double) -> rv -> rv)
(binaryLiftRV : (double -> double -> double) -> rv -> rv -> rv)
(putOptionPayoff : double -> option)
(callOptionPayoff : double -> option)
(tabulateN : (int -> 'a) -> int -> 'a list)
(mean : double list -> double)
(sampleVar : double list -> double)
(sampleHeads : int -> double)
(sampleHeadsMeanAndVariance : int -> int -> (double * double))
(experiment1 : (double * double) list)
*)
