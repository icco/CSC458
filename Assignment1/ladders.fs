(**
 * Chutes and Ladders 
 * @author Nat Welch
 * @author Mark Gius
 *)

let r = System.Random() 

// I landed on position pos, where should I be?
let pos p = 
    match p with
        | 1 -> 38
        | 4 -> 14
        | 9 -> 31
        | 16 -> 6
        | 21 -> 42
        | 28 -> 84
        | 36 -> 44
        | 47 -> 26
        | 49 -> 11
        | 51 -> 67
        | 56 -> 53
        | 62 -> 19
        | 64 -> 60
        | 71 -> 91
        | 80 -> 100
        | 87 -> 24
        | 93 -> 73
        | 96 -> 75
        | 98 -> 78
        | _ -> p

let roll_dice _ = r.Next(1, 6)

let rec game p1 p2 = 
   if p1 >= 100 then
      true
   elif p2 >= 100 then
      false
   else
      game (pos ((roll_dice ()) + p1)) (pos ((roll_dice ()) + p2))

let runBaseGame () = printf "%b" (game 0 0 )

runBaseGame () ;;
