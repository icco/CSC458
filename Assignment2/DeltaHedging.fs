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

   let rec rvNCount ( w : bool ) ( x : int ) ( f : event ) =
      if x = 0 then
         0.0
      else
         if ((f x) = w) then
            1.0 + rvNCount w (x-1) (f)
         else
            0.0 + rvNCount w (x-1) (f)

   let rvNCountHeads ( x : int ) =
      rvNCount true

   let rvNCountTails ( x : int ) =
      rvNCount false

   let doubleToRV ( l : double ) ( x : int ) ( f : event ) =
      l

