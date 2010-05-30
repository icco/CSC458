(** 
 * Final Assignment
 * 
 * @author Nathaniel "Nat" Welch
 *)

module EquityModel

let genList x = []

let generateData _ = Array.ofList ( genList 1000 )

let rec evaluateList = function
   | [] -> false
   | ((x:double)::xs) -> true

let evaluateData x = evaluateList (List.ofArray x)
