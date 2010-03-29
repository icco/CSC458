
let compound x = (x * (1.0 + 0.05) ** 50.0)

let rand _ =
   let x = System.Random().NextDouble()
   in
      if x < 0.05 then 2
      elif (x <= 0.1 && x > 0.05) then 0
      elif (x <= 0.35 && x > 0.1) then 3
      else 1

let rec manysamples x = 
   if x = 0 then
      0
   else
      (rand ()) + (manysamples (x - 1))

let rec manysamples2 f x = if x = 0 then 0 else (rand ()) + (f (x - 1))

printf "Compund: %f\n" (compound 2.0);;
printf "Random: %d\n" (rand ());;
printf "Many Samples: %d\n" (manysamples 5);;
printf "Many Samples Generalized: %d\n" (manysamples2 (rand) 5);;

