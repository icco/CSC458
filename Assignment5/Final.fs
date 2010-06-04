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

let tooMuchGrowth data = 
   ( (data.[999] / data.[0]) > 7 )

let stddev (data : double list) =
   let mean = List.average data
   let sum = (List.fold (fun acc x -> acc + (mean - x) ** 2.0)) 0.0 data
   sqrt (sum / (double (List.length data)))

let rec evaluateList d = 
   let r = []::(tooMuchGrowth d)
   let count = List.fold (fun acc x -> if x then acc + 1; else acc) 0 r
   count > ((List.length r) / 2)

let evaluateData x = evaluateList ( List.ofArray x )

