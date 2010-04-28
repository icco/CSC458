module Interp

open AST
open Values
open Xunit
open FsxUnit.Syntax

exception Unimplemented of string
exception RuntimeError of string

// Match either IntV or DoubleV as double
let (|AsDouble|) input =
    match input with
        | IntV i -> (double i)
        | DoubleV d -> d
        | _ -> raise (RuntimeError (sprintf "expected number, given: %A" input))

// lift a binary primitive in (num num -> num) to one on fVals
let numToNumPrim (p1,p2) args =
    match args with
        | [IntV i1; IntV i2] -> IntV (p1 i1 i2)
        | [(AsDouble d1);(AsDouble d2)] -> DoubleV (p2 d1 d2)
        | _ -> raise (RuntimeError (sprintf "expected two arguments, given: %A" args))

// given a prim, produce the corresponding function on fVals
let primFun p =
    match p with
        | PlusP -> numToNumPrim ((+),(+))
        | MinusP -> numToNumPrim ((-),(-))
        | _ -> raise (Unimplemented "haven't implemented this primitive yet")

// evaluate an expression in the given environment
let rec eval (env : env) exp =
    let recur = eval env
    match exp with
        | IntExp n -> IntV n
        | DoubleExp d -> DoubleV d
        | PrimExp (p,args) -> primFun p (List.map recur args)
        | _ -> raise (Unimplemented (sprintf "other operations not implemented yet: %A" exp))

// the empty environment
let (emptyEnv : env) = Map.ofList[]

// eval with the empty environment
let evalMT = eval emptyEnv


// TEST CASES (INCOMPLETE!)
[<Fact>]
let valTests () =
    should equal (DoubleV 3.4) (evalMT (DoubleExp 3.4))
    should equal (IntV 124) (evalMT (IntExp 124))
    should (throw_exception<Unimplemented>) (fun () -> (ignore (evalMT (StringExp "abc"))))

[<Fact>]
let primTests () =
    should equal (IntV 9) (evalMT (PrimExp(PlusP,[IntExp 4;IntExp 5])))
    should equal (DoubleV 9.0) (evalMT (PrimExp(PlusP,[DoubleExp 4.0;IntExp 5])))
    should equal (IntV 15) (evalMT (PrimExp(PlusP,[PrimExp(PlusP,[IntExp 4;IntExp 5]);IntExp 6])))
    should (throw_exception<RuntimeError>)
      (fun () -> ignore (evalMT (PrimExp(PlusP,[IntExp 4;IntExp 5;IntExp 6]))))

// HORRIBLE HACK; add top-level non-function things to the tuple below.
//  take them out to see what goes wrong.
[<EntryPoint>]
let entry args =
    ignore (evalMT,emptyEnv)
    0
