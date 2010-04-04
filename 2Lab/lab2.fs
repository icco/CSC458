let r = System.Random() ;;

// f1(x) = 3x^2 + 14
let f1 (x:double) = (3.0 * (x**2.0)) + 14.0

let differ f x = 
   let a1 = f (x - 0.05)
   let a2 = f (x + 0.05)
   in
      (a2 - a1) / 0.1

let oneStep (x:double) = 
   let one = (x * 0.01)
   in
      if r.NextDouble() > 0.5 then x + one else x - one

let rec step f x n = if n = 0 then x else (f (step f x (n - 1)))

let nSteps (x:double) n = step (oneStep) x n

printf "Should be 26: %.0f\n" (f1 2.0) ;;

printf "Should be 4: %.0f\n" (differ (fun x -> x**2.0) 2.0) ;;
printf "Should be 29: %.0f\n" (differ (fun x -> (3.0 * (x**2.0)) + (17.0 * x) + 12.0) 2.0) ;;
printf "Should be 224: %.0f\n" (differ (fun x -> (x ** 5.0) + (3.0 * (x ** 4.0)) + (2.0 * (x ** 3.0)) + (x ** 2.0) + (20.0 * x) + 10.0 ) 2.0) ;;

printf "Should be 2.02 | 1.98: %.2f\n" (oneStep 2.0) ;;
printf "Should be 2.02 | 1.98: %.2f\n" (oneStep 2.0) ;;

printf "Should be 2: %.2f\n" (nSteps 2.0 0) ;;
printf "Should be 2.02 | 1.98: %.2f\n" (nSteps 2.0 1) ;;
printf "Should be (1.96, 2.04): %.2f\n" (nSteps 2.0 2) ;;

printf "\nTrails:\n" ;;

for i in 1..1000 do
   printf "%f, %f\n" (nSteps 1.0 20) (nSteps 1.0 40)
