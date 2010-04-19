(*
*)

let r = System.Random()

(**
 * takes x and returns a list of Heads or tails. 
 * tails = 0, heads = 1
 *)
let ra _ = r.Next(0,2)
let eventFun x = List.init x ra

(**
 * A global named m?
 *)
let m = 10.0

let randomVariable x = if x = 1 then m else 1.0/m

(**
 * event -> double
let randomVariable x = 
   let k = List.map (fun x -> ( if x = 1 then m else 1.0/m )) (eventFun x)
   in
      List.fold (fun l x -> (l * x)) 1.0 k
 *)

let equity a = 
