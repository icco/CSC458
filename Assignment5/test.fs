
open Final

ignore ( generateData : unit -> double array)
ignore ( evaluateData : double array -> bool)

generateData () |> printfn "%A"

evaluateData ( generateData () ) |> printfn "%A"
