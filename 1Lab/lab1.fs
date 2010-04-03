
let compound x = (x * (1.0 + 0.05) ** 50.0)

let r = System.Random()

let rand _ =
   let x = r.NextDouble()
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

// takes a unit -> int function
let rec manysamples2 f x = if x = 0 then 0 else (manysamples2 f (x - 1)) + (f ())

let rec newrandr a y = 
   match a with
   | [] -> 0
   | (x::xs) -> if y < x then 0 else (1 + (newrandr xs y))

let newrand a () = newrandr (Array.toList a) (r.NextDouble())

printf "Compund: %f\n" (compound 2.0) ;;
printf "Random: %d\n" (rand ()) ;;
printf "Many Samples: %d\n" (manysamples 5) ;;
printf "Many Samples Generalized: %d\n" (manysamples2 (rand) 5) ;;
printf "Random Generalized: %d\n" (manysamples2 (newrand [| 0.3; 0.9 |]) 5) ;;

printf "\nTest a' mer-bober:\n" ;;

for i in 1..10000 do
   printf "%d, %d\n" i (manysamples2 (newrand [| 0.3; 0.9 |]) 100)
