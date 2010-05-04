(**
 * Tests for assignment 2
 * @author Nathaniel "Nat" Welch
 *)

open DeltaHedging

let equal a b = if (a = b) then printf "." else printf "e"
let not_eq a b = if ( not (a = b)) then printf "." else printf "e"

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


