(**
 * @author Nathaniel "Nat" Welch
 *)

(* Tests... *)
open Xunit
open FsxUnit.Syntax
open System

open DeltaHedging

[<Fact>]
let test_makeERandom () =
   ignore (makeERandom : unit -> event)
   let eRandom1 = makeERandom ()
   let eRandom2 = makeERandom ()
   eRandom1 57 |> should equal ( eRandom1 57 )
   eRandom2 57 |> should equal ( eRandom2 57 )

