
open Final

ignore ( generateData : unit -> double array)
ignore ( evaluateData : double array -> bool)

generateData () |> printfn "%A"

evaluateData ( generateData () ) |> printfn "%A"

let differencesTest () =
   let aList = [1.0;2.0]
   differences aList |> printfn "%A should be [2.0]"

differencesTest ()
