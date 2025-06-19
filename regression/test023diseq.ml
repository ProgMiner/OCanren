module L = List
         
open GT
open Printf
open OCanren
open OCanren.Std

let test x y = ocanren
  { (x, y) =/= (Nat.s (Nat.s x), Nat.s (Nat.s y)) & trace_diseq
  & x =/= Nat.s y & trace_diseq
  }

let _ =
  L.iter (fun (q, r) -> printf "q=%s, r=%s\n" q r) @@
  Stream.take ~n:(-1) @@
  ocanrun (q, r : ^Nat.nat) {test q r} -> (show(Nat.logic) q, show(Nat.logic) r)
