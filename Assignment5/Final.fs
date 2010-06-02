(** 
 * Final Assignment
 * 
 * @author Nathaniel "Nat" Welch
 * @author Mark Gius
 *)

module Final

let r = System.Random()
let roll _ = double (r.Next(-2, 4))

let scale ( x : double ) = x + ((roll()) * r.NextDouble())

let rec genList x = 
   if x = 0 then (100.0::[])
   else
      let y = genList (x-1)
      (scale (List.head y))::y

let generateData _ = Array.ofList ( genList 1000 )

let rec evaluateList = function
   | [] -> false
   | ((x:double)::xs) -> true

let evaluateData x = evaluateList (List.ofArray x)
