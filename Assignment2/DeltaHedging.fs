(**
 * Assignment 2
 * @author Nathaniel "Nat" Welch
 *)

module DeltaHedging

open System
open System.Collections.Generic

(* required types *)
type event1 = bool
type event = int -> event1
type rv = event -> double
type rvseq = int -> rv
type option = double -> double

module DeltaHedging =
   let s = System.Random()

   let makeERandom () ( x : int ) = 
      let r = System.Random()
      let dict = new Dictionary<int, event1> ()
      in
         if (dict.ContainsKey x) then
            dict.[x]
         else
            dict.Add(x, ( if (r.Next(1) = 0) then true else false ));
            dict.[x]

   let makeERandP ( l : double ) ( x : int ) =
      let r = System.Random(s.Next())
      let dict = new Dictionary<int, event1> ()
      in
         if (dict.ContainsKey x) then
            dict.[x]
         else
            dict.Add(x, ( if r.NextDouble() > l then true else false ));
            dict.[x]


   let makeERandT () ( x : int ) =
      let r = System.Random(s.Next())
      let dict = new Dictionary<int, event1> ()
      in
         if (dict.Count > 1) then
            if (dict.ContainsKey x) then
               dict.[x]
            else
               dict.Add(x, ( if r.NextDouble() < 0.75 then true else false ));
               dict.[x]
         else
            dict.Add(x, ( if r.NextDouble() > 0.5 then true else false ));
            dict.[x]

