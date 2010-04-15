(**
 * Chutes and Ladders Players
 *
 * @author Nat Welch
 * @author Mark Gius
 *)
module MyPlayers
open Player

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
 * Tries to prolong the game.
 *)
type EnergeticPlayer () =
   inherit Player ()
   override this.shouldDouble myPos hisPos = false
   override this.shouldTake myPos hisPos = true

type Welch () =
   inherit Player ()
   let f p =
      let a = p + 3
      in
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

   override this.shouldDouble myPos hisPos = (f myPos) > (f hisPos)
   override this.shouldTake myPos hisPos = true

type Gius () =
    inherit Player()
    let giusBoard pos =
        match pos with
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
            | _ -> pos

    // Averages the positions that can be achived in the next 2 turns
    let folder doubleArray = List.fold (fun acc x -> acc + x)
    let score1 pos =
        float (List.fold (fun acc x -> acc + x) 0
                   [for j in [for i in 1..6 -> giusBoard (pos + i)]
                       -> List.fold (fun acc2 x2 -> acc2 + x2) 0
                             [for h in 1..6 -> giusBoard (j + j)] ]) / 6.0

    override this.shouldDouble myPos hisPos =
// printfn "DEBUG: I'm at %d, he's at %d, %f vs %f"
// myPos hisPos (score1 myPos) (score1 hisPos)
        score1 myPos > score1 hisPos
    override this.shouldTake myPos hisPos =
        true

(**
 * Like the Reckless player, but only takes when he is ahead.
 * @author nwelch 
 *)
type ThinkingPlayer () =
   inherit Player ()
   override this.shouldDouble myPos hisPos = (myPos > hisPos)
   override this.shouldTake myPos hisPos = (myPos > hisPos)
