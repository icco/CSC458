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
type stockmodel = (double * double * double * double) // u, d, 1+r, and S_0

module DeltaHedging =
   let r = System.Random()
   
   let eAllHeads x = true
   let eAllTails x = false
   let eAlternating x = if (( x % 2 ) = 0 ) then true else false

   let forceEParts x b ev ts =
      if (( ts > x + (Array.length b)) || ( ts < x )) then (ev ts)
      else
         b.[ts - x]

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

   let putOptionPayoff  ( strike : double ) ( stock : double ) = max 0.0 (strike - stock)
   let callOptionPayoff ( strike : double ) ( stock : double ) = max 0.0 (stock - strike)

   (* n choose k, based off of wikipedia article *)
   let rec n_k n k =
      if k = 0 then 1
      else
         if n = 0 then 0
         else
            (n_k (n-1) (k-1)) + (n_k (n-1) k)

   (* yay, this is wrong... *)
   let optionValue ( sm : stockmodel ) ( tp : int ) o i (ev : event) =
      let (u, d, r, init) = sm
      let cval = rvNStock u d init i ev
      let p = (r - d) / (u - d)
      let q = 1.0 - p
      let mutable sum = 0.0
      for n = 1 to (tp - i) do
         sum = sum + ((p ** (double i)) * (q ** (double (n - 1))) * (double (n_k n i)) * (o (double i)))
      ((1.0 / r) ** (double tp)) * sum

   (* ran out of time... *)
   let delta ( r : rvseq ) ( r2 : rvseq ) = r

   (* ran out of time... *)
   let illustration ( s : stockmodel ) ( k : int ) ( o : option ) ( e : event ) = ()

