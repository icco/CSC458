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

let tooMuchGrowth ( data : double list ) = 
   ( (data.[999] / data.[0]) > 7.0 )

let stddev ( data : double list ) =
   let mean = List.average data
   let sum = (List.fold (fun acc x -> acc + (mean - x) ** 2.0)) 0.0 data
   sqrt (sum / (double (List.length data)))

let differences (data : double list) =
   let listEnd = List.tail data
   (* The following line of code should be punishable by death *)
   let listBegin = List.rev (List.tail (List.rev data))
   List.map2 (fun x y -> x / y) listEnd listBegin

let equalWithTolerance tolerance a b =
   (b - a) < tolerance

(* This test uses precalculated data based on the stocks specified in
   lab13 plus a few others.  The average return and std dev of returns are
   calculated, and then the std dev and mean of the returns and std devs are 
   taken.  If a set of data has similar std dev and mean, it's probably a stock
 *)
let stddevmeantest data =
   let dataDifferences = differences (List.ofArray data)
   let returnsMean = 0.99994492132
   let returnsStdDev = 0.00056242307
   let stdDevsMean = 0.02580240517
   let stdDevsStdDev = 0.00998161951
   let dataStdDev = stddev dataDifferences
   let dataReturn = geometricMean dataDifferences
   (equalWithTolerance (returnsStdDev * 2.0) returnsMean dataReturn) &&
      (equalWithTolerance (stdDevsStdDev * 2.0) stdDevsMean dataStdDev)

let rec evaluateList d = 
   let r = ( tooMuchGrowth d )::[]
   let count = List.fold (fun acc x -> if x then acc + 1; else acc) 0 r
   count > ((List.length r) / 2)

let evaluateData x = evaluateList ( List.ofArray x )

