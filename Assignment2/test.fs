(**
 * Tests for assignment 2
 * @author Nathaniel "Nat" Welch
 *)

open DeltaHedging

let equal a b = if (a = b) then printf "." else printf "e"
let not_eq a b = if ( not (a = b)) then printf "." else printf "e"

(* required definitions *)
(*
ignore (DeltaHedging.eAllHeads : event)
ignore (DeltaHedging.eAllTails : event)
ignore (DeltaHedging.eAlternating : event)
ignore (DeltaHedging.forceEParts : int -> bool array -> event -> event)
ignore (DeltaHedging.optionValue : stockmodel -> int -> option -> rvseq)
ignore (DeltaHedging.delta : rvseq -> rvseq -> rvseq)
ignore (DeltaHedging.illustration : stockmodel -> int -> option -> event -> ())
*)

printf "\nmakeERandom\t: "
ignore (DeltaHedging.makeERandom : unit -> event)
let eRandom1 = DeltaHedging.makeERandom ()
let eRandom2 = DeltaHedging.makeERandom ()
let t1 = eRandom1 57
let t2 = eRandom1 57
let t3 = eRandom2 37
let t4 = eRandom2 37
equal t1 t2
equal t3 t4
not_eq t1 t3

printf "\nmakeERandP\t: "
ignore (DeltaHedging.makeERandP : double -> event)
let eRandom3 = DeltaHedging.makeERandP 0.5
let eRandom4 = DeltaHedging.makeERandP 0.003
let t5 = eRandom1 57
let t6 = eRandom1 57
let t7 = eRandom2 57
let t8 = eRandom2 57
equal t5 t6
equal t7 t8
not_eq t5 t7
ignore (DeltaHedging.makeERandT : unit -> event)

ignore (DeltaHedging.rvNCountHeads : int -> rv)
ignore (DeltaHedging.rvNCountTails : int -> rv)
ignore (DeltaHedging.doubleToRV : double -> rv)

ignore (DeltaHedging.rvNStock : double -> double -> double -> rvseq)

ignore (DeltaHedging.rvPathD : rvseq)

ignore (DeltaHedging.unaryLiftRV : (double -> double) -> rv -> rv)
ignore (DeltaHedging.binaryLiftRV : (double -> double -> double) -> rv -> rv -> rv)

ignore (DeltaHedging.putOptionPayoff : double -> option)
ignore (DeltaHedging.callOptionPayoff : double -> option)

