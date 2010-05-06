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
   let r = System.Random()

   let makeERandom () ( x : int ) = 
      let dict = new Dictionary<int, event1> ()
      in
         if (dict.ContainsKey x) then
            dict.[x]
         else
            dict.Add(x, ( if (r.Next(1) = 0) then true else false ));
            dict.[x]

   let makeERandP ( l : double ) ( x : int ) =
      let dict = new Dictionary<int, event1> ()
      in
         if (dict.ContainsKey x) then
            dict.[x]
         else
            dict.Add(x, ( if r.NextDouble() > l then true else false ));
            dict.[x]

   let makeERandT () ( x : int ) =
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

   let rec rvNCount ( w : event1 ) ( x : int ) ( f : event ) =
      if x = 0 then
         double ( 0 )
      else
         if ((f x) = w) then
            1.0 + rvNCount w (x-1) f
         else
            0.0 + rvNCount w (x-1) f

   let rvNCountHeads = ( rvNCount true )
   let rvNCountTails = ( rvNCount false )
   let doubleToRV ( l : double ) x = l

   let rec rvNStock  ( u : double ) ( d : double ) ( init : double ) ( x : int ) ( f : event ) =
      if x = 0 then
         init
      else
         if (f x) then
            u * ( rvNStock u d init (x-1) f )
         else
            d * ( rvNStock u d init (x-1) f )

   let rec rvPathD ( x : int ) ( f : event ) =
      if x = 0 then 0.0
      else
         let newx = ( ( x - ( x % 3 ) ) - 3 )
         if (f x) then
            10.0 + ( rvPathD newx f )
         else
            ( rvPathD newx f )

   let rec unaryLiftRV f rf ev = f ( rf ev )
   let rec binaryLiftRV f rf1 rf2 ev = f ( rf1 ev ) ( rf2 ev )





