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
        
(**
 * Player Class
 * 
 * abstract shouldDouble : int -> int -> bool
 * abstract shouldTake : int -> int -> bool
 *)
[<AbstractClass>]
type Player () =
    let mutable _pos = 0

    member this.pos
        with get () =
            _pos
        and set newValue =
            _pos <- newValue

    abstract shouldDouble : int -> int -> bool
    abstract shouldTake : int -> int -> bool


(**
 * This player always doubles if he is ahead, and always takes a double,
 * regardless of his position
 * @author mgius
 *)
type RecklessPlayer () =
    inherit Player ()
    override this.shouldDouble myPos hisPos =
        myPos > hisPos

    override this.shouldTake myPos hisPos =
        true

(**
 * Like the Reckless player, but only takes when he is ahead.
 * @author nwelch 
 *)
type ThinkingPlayer
    inherit Player ()
    override this.shouldDouble myPos hisPos =
        myPos > hisPos

    override this.shouldTake myPos hisPos =
        myPos > hisPos

(**
 * The game function....
 * 
 * Replaced most of my code with mgius'.
 *)
let rec game ( p_one:Player ) ( p_two:Player ) whoseTurn bet cubeOwner =
   let take = p_one.shouldTake p_one.pos p_two.pos ? doubles + 1 : doubles - 1
   let newDoubles = (p_one.shouldDouble p_one.pos p_two.pos) ? take : doubles

   let playerMod =
      match whoseTurn % 2 with
         | 0 -> 1.0
         | 1 -> -1.0
         | _ -> 0.0 // impossible
      
   match newDoubles, gameBoard (p_one.pos + (roll ())) with
       | newDoubles, _ when newDoubles < doubles ->
           // This is an indicator that one of the players rejected the bet
           2.0 ** float doubles * playerMod
       | _, newPos when newPos > 100 ->
           2.0 ** float newDoubles * playerMod
       | _, newPos ->
           p_one.pos <- p_one.pos + newPos
           game p_two p_one (whoseTurn + 1) newDoubles cubeOwner

let runGame (p_one:Player ) (p_two:Player ) = 
   game p_one p_two 0 0 -1

runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "%f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "%f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "%f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "%f"
runGame (ThinkingPlayer ()) (RecklessPlayer ()) |> printfn "%f"

