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
type Player() =
    let mutable _pos = 0

    member this.pos
        with get() =
            _pos
        and set(newValue) =
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

let rec game (player_one:Player ) (player_two:Player ) whoseTurn bet cubeOwner =
    match gameBoard (playerOne.pos + (roll ())) with
        | newPos when newPos > 100 ->
            whoseTurn % 2 + 1 |> printfn "Player %d has won!"
        | newPos ->
            playerOne.pos <- playerOne.pos + newPos
            realRunGame playerTwo playerOne (whoseTurn + 1) bet cubeOwner

let runBaseGame () = printf "%b" (game 0 0 )

runBaseGame () ;;

