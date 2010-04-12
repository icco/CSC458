(**
 * Chutes and Ladders 
 *
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

let roll _ = r.Next (1,7)

(**
 * Player Class
 * 
 * abstract shouldDouble : int -> int -> bool
 * abstract shouldTake : int -> int -> bool
 *)
[<AbstractClass>]
type Player () =
   abstract shouldDouble : int -> int -> bool
   abstract shouldTake : int -> int -> bool


(**
 * This player always doubles if he is ahead, and always takes a double,
 * regardless of his position
 * @author mgius
 *)
type RecklessPlayer () =
   inherit Player ()
   override this.shouldDouble myPos hisPos = (myPos > hisPos)
   override this.shouldTake myPos hisPos = true

(**
 * Like the Reckless player, but only takes when he is ahead.
 * @author nwelch 
 *)
type ThinkingPlayer () =
   inherit Player ()
   override this.shouldDouble myPos hisPos = (myPos > hisPos)
   override this.shouldTake myPos hisPos = (myPos > hisPos)

(**
 * The game function....
 *)
let rec realRunGame gameState ( p_one : Player ) ( p_two : Player ) =
   let (whoseTurn, doubles, cubeOwner, pos1, pos2) = gameState
   let move = roll ()

   let newDoubles = (
      if (cubeOwner = -1 || cubeOwner % 2 = whoseTurn % 2) && (p_one.shouldDouble pos1 pos2) then 
         if p_two.shouldTake pos2 pos1 then (doubles + 1) else (doubles - 1)
      else
         doubles
   )

   let newOwner = (
      if (cubeOwner = -1 || cubeOwner % 2 = whoseTurn % 2) && (p_one.shouldDouble pos1 pos2) then 
         if p_two.shouldTake pos2 pos1 then (whoseTurn + 1) else (cubeOwner)
      else
         cubeOwner
   )

   let playerMod =
      match whoseTurn % 2 with
         | 0 ->  1.0
         | 1 -> -1.0
         | _ ->  0.0

   match newDoubles, pos (pos1 + (roll ())) with
      | newDoubles, _ when newDoubles < doubles ->
         // This is an indicator that playertwo rejected the bet
         (pos1, pos2) ||> printfn "Rejected: %d %d"
         2.0 ** float doubles * playerMod
      | _, newPos when newPos >= 100 ->
         (pos1, pos2) ||> printfn "Victory: %d %d"
         2.0 ** float newDoubles * playerMod
      | _, newPos ->
         (pos1, pos2) ||> printfn "%d %d"
         realRunGame (whoseTurn + 1, newDoubles, newOwner, pos2, newPos)
                  p_two p_one

let runGame = realRunGame (0,0,-1,0,0)

runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "\t%.0f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "\t%.0f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "\t%.0f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "\t%.0f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "\t%.0f"

