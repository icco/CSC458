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
   ignore (DeltaHedging.makeERandom : unit -> event)
   ignore (DeltaHedging.makeERandP : double -> event)
   ignore (DeltaHedging.makeERandT : unit -> event)
   ignore (DeltaHedging.forceEParts : int -> bool array -> event -> event)
   ignore (DeltaHedging.doubleToRV : double -> rv)
   ignore (DeltaHedging.rvNCountHeads : int -> rv)
   ignore (DeltaHedging.rvNCountTails : int -> rv)
   ignore (DeltaHedging.rvNStock : double -> double -> double -> rvseq)
   ignore (DeltaHedging.rvPathD : rvseq)
   ignore (DeltaHedging.unaryLiftRV : (double -> double) -> rv -> rv)
   ignore (DeltaHedging.binaryLiftRV : (double -> double -> double) -> rv -> rv -> rv)
   ignore (DeltaHedging.putOptionPayoff : double -> option)
   ignore (DeltaHedging.callOptionPayoff : double -> option)
   ignore (DeltaHedging.optionValue : stockmodel -> int -> option -> rvseq)
   ignore (DeltaHedging.delta : rvseq -> rvseq -> rvseq)
   ignore (DeltaHedging.illustration : stockmodel -> int -> option -> event -> ())
   *)

printf "\nmakeERandom\t: "
let eRandom1 = DeltaHedging.makeERandom ()
let eRandom2 = DeltaHedging.makeERandom ()
let t1 = eRandom1 57
let t2 = eRandom1 57
let t3 = eRandom2 57
let t4 = eRandom2 57
equal t1 t2
equal t3 t4
not_eq t3 t1

printf "\nmakeERandP\t: "


