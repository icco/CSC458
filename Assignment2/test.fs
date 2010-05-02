(**
 * Tests for assignment 2
 * @author Nathaniel "Nat" Welch
 *)

open DeltaHedging

let eRandom1 = makeERandom ()
let eRandom2 = makeERandom ()
let t1 = eRandom1 57
let t2 = eRandom1 57
let t3 = eRandom2 57
let t4 = eRandom2 57
(t1, t2) ||> printfn "Equal? %A %A"
