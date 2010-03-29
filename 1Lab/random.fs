
let f _ =
   let x = System.Random().NextDouble()
   in
      if x < 0.05 then 2
      elif (x <= 0.1 && x > 0.05) then 0
      elif (x <= 0.35 && x > 0.1) then 3
      else 1

printf "%d" (f());;
   
