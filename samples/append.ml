module L = List

open GT
open Printf
open OCanren
open OCanren.Std

let _ =
  run q (fun zs -> ocanren {
    fresh ys in ys == 4::5::6::ys & List.appendo [1; 2; 3] ys zs
  }) (fun r -> r#reify (List.reify Nat.prj_exn))
    |> Stream.map (show(List.logic) (fun x -> string_of_int @@ Nat.to_int x))
    |> Stream.take ~n:(-1)
    |> L.iter (fun zs -> printf "zs=%s\n" zs)

(*
(* hangs on first *)
let _ =
  run q (fun zs -> ocanren {
    fresh xs, ys in xs == 1::2::3::xs & ys == 4::5::6::ys & List.appendo xs ys zs
  }) (fun r -> r#reify (List.reify Nat.prj_exn))
    |> Stream.map (show(List.logic) (fun x -> string_of_int @@ Nat.to_int x))
    |> Stream.take ~n:(-1)
    |> L.iter (fun zs -> printf "zs=%s\n" zs)
*)

(* produces infinite number of answers *)
let _ =
  run qr (fun xs ys -> ocanren {
    fresh zs in zs == 1::2::zs & List.appendo xs ys zs
  }) (fun r1 r2 -> r1#reify (List.reify Nat.prj_exn), r2#reify (List.reify Nat.prj_exn))
    |> Stream.map begin fun (xs, ys) ->
      show(List.logic) (fun x -> string_of_int @@ Nat.to_int x) xs,
      show(List.logic) (fun x -> string_of_int @@ Nat.to_int x) ys
    end
    |> Stream.take ~n:5
    |> L.iter (fun (xs, ys) -> printf "xs=%s, ys=%s\n" xs ys)
