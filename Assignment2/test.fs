(**
 * Tests for assignment 2
 * @author Nathaniel "Nat" Welch
 *)

open DeltaHedging

let eRandom1 = DeltaHedging.makeERandom ()
let eRandom2 = DeltaHedging.makeERandom ()
let t1 = eRandom1 57
let t2 = eRandom1 57
let t3 = eRandom2 57
let t4 = eRandom2 57
(t1, t2) ||> printfn "Equal? %A %A"
(t3, t4) ||> printfn "Equal? %A %A"
