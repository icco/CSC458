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

let rec genList acc x =
   if x = 0 then acc 
   else
      genList ((scale (List.head acc))::acc) (x - 1)

let generateData _ =  Array.ofList ( List.rev ( genList [100.0] 1000 ) )

let geometricMean (data : double list) = 
   (List.fold (fun acc x -> acc * x) 1.0 data) ** 
      (1.0 / (double (List.length data) - 1.0))

let stddev (data : double list) =
   let mean = List.average data
   let sum = (List.fold (fun acc x -> acc + (mean - x) ** 2.0)) 0.0 data
   sqrt (sum / (double (List.length data)))

//let stddevmeantest data =
//   let dataList = List.ofArray data

let rec evaluateList = function
   | [] -> true // Hey why not?
   | ((x:double)::xs) -> false

let evaluateData x = evaluateList ( List.ofArray x )
