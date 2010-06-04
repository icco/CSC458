(** 
 * Final Assignment
 * 
 * @author Nathaniel "Nat" Welch
 * @author Mark Gius
 *)

module Final

(** throughout the code, we need some randomness *)
let r = System.Random()

(** a die that returns between -2.0 and 4.0 *)
let roll _ = double (r.Next(-2, 4))

(** given yesterdays fake data, create todays *)
let scale ( x : double ) = x + ((roll()) * r.NextDouble())

(**
 * This does the real work for generating our fake data using tail recursion
 * and the above scale function.
 *)
let rec genList acc x =
   if x = 0 then acc 
   else
      genList ((scale (List.head acc))::acc) (x - 1)

(** return our data, and seriously, who uses Arrays? Bah. *)
let generateData _ =  
   let badData = List.rev ( genList [100.0] 1000 )
   (* Massage the data to have the same number of significant digits as 
      Yahoo's data *)
   Array.ofList (List.map (fun x -> (System.Math.Floor (x * 100.0)) / 100.0) 
                          badData)

(**
 * geometric mean, pretty straight forward. see wikipedia article if this
 * confuses you.
 *)
let geometricMean (data : double list) = 
   (List.fold (fun acc x -> acc * x) 1.0 data) ** 
      (1.0 / (double (List.length data) - 1.0))

(** 
 * I have trouble beleiving stocks can grow more than one order of magnitude in
 * three years.
 *)
let tooMuchGrowth ( data : double list ) = 
   ( (data.[999] / data.[0]) < 10.0 )

(** Standard Deviation Calc. *)
let stddev ( data : double list ) =
   let mean = List.average data
   let sum = (List.fold (fun acc x -> acc + (mean - x) ** 2.0)) 0.0 data
   sqrt (sum / (double (List.length data)))

(** Given an array of stocks, returns an array of their daily diffs. *)
let differences (data : double list) =
   let listEnd = List.tail data
   (* The following line of code should be punishable by death *)
   let listBegin = List.rev (List.tail (List.rev data))
   List.map2 (fun x y -> x / y) listEnd listBegin

(** compare with a boundary. *)
let equalWithTolerance tolerance a b =
   abs (b - a) < tolerance

(**
 * This test uses precalculated data based on the stocks specified in lab13
 * plus a few others. The average return and std dev of returns are calculated,
 * and then the std dev and mean of the returns and std devs are taken. If a
 * set of data has similar std dev and mean, it's probably a stock
 *)
let stddevmeantest data =
   let dataDifferences = differences data
   let returnsMean = 0.99994492132
   let returnsStdDev = 0.00056242307
   let stdDevsMean = 0.02580240517
   let stdDevsStdDev = 0.00998161951
   let dataStdDev = stddev dataDifferences
   let dataReturn = geometricMean dataDifferences
   (equalWithTolerance (returnsStdDev * 2.0) returnsMean dataReturn) &&
      (equalWithTolerance (stdDevsStdDev * 2.0) stdDevsMean dataStdDev)

(**
 * the real work for the below function.
 *)
let rec evaluateList d = 
   let tests = ( tooMuchGrowth d )::( stddevmeantest d )::[]
   let count = List.fold (fun acc x -> if x then acc + 1; else acc) 0 tests
   count > ((List.length tests) / 2)

(**
 * Do we think your data is a real stock? true if yes, false if no...
 *)
let evaluateData x = evaluateList ( List.ofArray x )

