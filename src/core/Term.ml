(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * OCanren.
 * Copyright (C) 2015-2025
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Printf

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

type t = Obj.t

module Var =
  struct

    type term   = t
    type env    = int
    type scope  = int
    type anchor = int ref

    let tabling_env = -1

    let unused_index = -1

    let non_local_scope = -6

    let new_scope =
      let scope = ref 0 in
      fun () -> (incr scope; !scope)

    let global_anchor = ref (-8)

    type t =
      { anchor        : anchor;
        env           : env;
        index         : int;
        mutable subst : term option;
        scope         : scope;
        constraints   : term list
      }

    let make ~env ~scope index = {
      env         = env;
      anchor      = global_anchor;
      subst       = None;
      constraints = [];
      index;
      scope;
    }

    let is_wildcard { index } = index = -42

    let make_wc ~env ~scope = make ~env ~scope (-42)

    let dummy =
      let env   = 0 in
      let scope = 0 in
      make ~env ~scope 0

    let var_tag, var_size =
      let dummy = Obj.repr dummy in
      Obj.tag dummy, Obj.size dummy

    let is_valid_anchor anchor = anchor == global_anchor

    let has_var_structure tx sx x =
      if tx = var_tag && sx = var_size then
        let anchor = (Obj.obj x).anchor in
        (Obj.is_block @@ Obj.repr anchor) && is_valid_anchor anchor
      else false

    let reify r { index ; constraints } = index, List.map r constraints

    let equal x y =
      (x.index = y.index) && (x.env = y.env)

    let compare x y =
      if x.index <> y.index then x.index - y.index else x.env - y.env

    let hash x = Hashtbl.hash (x.env, x.index)

    let describe ppf { index } = Format.fprintf ppf "_.%d" index

    let pp ppt ppf x = match x.constraints with
    | [] -> describe ppf x
    | cs ->
      let open Format in
      let ppcs = pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") ppt in
      fprintf ppf "%a{=/= %a}" describe x ppcs cs
  end

module VarSet = Set.Make(Var)
module VarTbl = Hashtbl.Make(Var)

module VarMap =
  struct

    include Map.Make(Var)

    let iteri f m =
      let i = ref 0 in
      iter (fun k v -> f !i k v; incr i) m
  end

module Mu =
  struct

    type anchor = int ref
    type term = t

    let global_anchor = ref (-8)

    type t = {
      anchor: anchor;
      var: Var.t;
      body: term;
    }

    let make var body = { anchor = global_anchor ; var ; body }

    let dummy = make Var.dummy (Obj.repr 0)

    let mu_tag, mu_size =
      let dummy = Obj.repr dummy in
      Obj.tag dummy, Obj.size dummy

    let is_valid_anchor anchor = anchor == global_anchor

    let has_mu_structure tx sx x =
      if tx = mu_tag && sx = mu_size then
        let anchor = (Obj.obj x).anchor in
        (Obj.is_block @@ Obj.repr anchor) && is_valid_anchor anchor
      else false
  end

type value = Obj.t

type shape =
| Var of Var.t
| Val of int
| Con of int * int * (int -> t)
| Mu of Mu.t

let repr = Obj.repr
let obj = Obj.obj

let is_box t =
  t <= Obj.last_non_constant_constructor_tag &&
  t >= Obj.first_non_constant_constructor_tag

let is_int = (=) Obj.int_tag
let is_str = (=) Obj.string_tag
let is_dbl = (=) Obj.double_tag

let is_val t = is_int t || is_str t || is_dbl t

let check_val t =
  if not @@ is_val t then invalid_arg @@ sprintf "OCanren fatal: invalid value tag (%d)" t

let[@inline] shape x =
  let tx = Obj.tag x in
  if is_box tx then
    let sx = Obj.size x in
    if Var.has_var_structure tx sx x then Var (obj x)
    else if Mu.has_mu_structure tx sx x then Mu (obj x)
    else Con (tx, sx, Obj.field x)
  else begin
    check_val tx ;
    Val tx
  end

let make_con tx sx xi =
  let x = Obj.new_block tx sx in
  for i = 0 to sx - 1 do
    Obj.set_field x i (xi i)
  done ;
  x

let rec pp ppf x =
  let open Format in
  match shape x with
  | Var x -> Var.pp pp ppf x
  | Val tx ->
    if is_int tx then fprintf ppf "int<%d>" @@ obj x
    else if is_str tx then fprintf ppf "string<%s>" @@ obj x
    else if is_dbl tx then fprintf ppf "double<%e>" @@ obj x
    else assert false
  | Con (tx, sx, xi) ->
    let rec inner i =
      if i < sx then begin
        if i > 0 then fprintf ppf ", " ;
        pp ppf (xi i) ;
        inner (i + 1)
      end
    in
    fprintf ppf "boxed %d <" tx ;
    inner 0 ;
    fprintf ppf ">"
  | Mu x -> fprintf ppf "mu %a <%a>" Var.describe x.Mu.var pp x.Mu.body

let show x = Format.asprintf "%a" pp x

let rec equal x y =
  match shape x, shape y with
  | Var x, Var y -> Var.equal x y && List.equal equal x.Var.constraints y.Var.constraints
  | Val _, Val _ -> x = y
  | Con (tx, sx, xi), Con (ty, sy, yi) when tx = ty && sx = sy ->
    let rec inner i =
      if i < sx then equal (xi i) (yi i) && inner (i + 1)
      else true
    in
    inner 0
  | Mu x, Mu y -> Var.equal x.Mu.var y.Mu.var && equal x.Mu.body y.Mu.body
  | _ -> false

let compare' = compare

let compare x y =
  match shape x, shape y with
  | Var x, Var y ->
    let res = Var.compare x y in
    if res <> 0 then res
    else List.compare compare x.Var.constraints y.Var.constraints
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Val _, Val _ -> compare' x y
  | Val _, _ -> -1
  | _, Val _ -> 1
  | Con (tx, sx, xi), Con (ty, sy, yi) ->
    if tx <> ty then compare' tx ty
    else if sx <> sy then compare' sx sy
    else
      let rec inner i =
        if i < sx then
          let res = compare (xi i) (yi i) in
          if res <> 0 then res else inner (i + 1)
        else 0
      in
      inner 0
  | Con _, _ -> -1
  | _, Con _ -> 1
  | Mu x, Mu y ->
    let res = Var.compare x.Mu.var y.Mu.var in
    if res <> 0 then res else compare x.Mu.body y.Mu.body

let rec hash x =
  match shape x with
  | Var x ->
    let cs = List.map hash x.Var.constraints in
    Hashtbl.hash (Var.hash x, cs)
  | Val _ -> Hashtbl.hash x
  | Con (tx, sx, xi) ->
    let rec inner i =
      if i < sx then (hash @@ xi i)::(inner @@ i + 1)
      else []
    in

    Hashtbl.hash (tx, inner 0)
  | Mu x -> Hashtbl.hash (Var.hash x.Mu.var, hash x.Mu.body)

let map_head f x =
  match shape x with
  | Con (tx, sx, xi) -> make_con tx sx @@ fun i -> f @@ xi i
  | _ -> x

let unsafe_map ~fvar ~fval =
  let rec hlp bvs =
    let rec hlp_bvs x =
      match shape x with
      | Var x' -> if VarSet.mem x' bvs then x else fvar x'
      | Val _ -> fval x
      | Con (tx, sx, xi) -> make_con tx sx @@ fun i -> hlp_bvs @@ xi i
      | Mu x -> repr @@ Mu.make x.Mu.var @@ hlp (VarSet.add x.Mu.var bvs) x.Mu.body
    in
    hlp_bvs
  in
  hlp VarSet.empty

let iter ~fvar ~fval =
  let rec hlp bvs =
    let rec hlp_bvs x =
      match shape x with
      | Var x -> if not @@ VarSet.mem x bvs then fvar x
      | Val _ -> fval x
      | Con (_, sx, xi) ->
        for i = 0 to sx - 1 do
          hlp_bvs @@ xi i
        done
      | Mu x -> hlp (VarSet.add x.Mu.var bvs) x.Mu.body
    in
    hlp_bvs
  in
  hlp VarSet.empty

let fold ~fvar ~fval ~init =
  let rec hlp bvs =
    let rec hlp_bvs acc x =
      match shape x with
      | Var x -> if VarSet.mem x bvs then acc else fvar acc x
      | Val _ -> fval acc x
      | Con (tx, sx, xi) ->
        let rec inner i acc =
          if i < sx then inner (i + 1) @@ hlp_bvs acc @@ xi i
          else acc
        in
        inner 0 acc
      | Mu x -> hlp (VarSet.add x.Mu.var bvs) acc x.Mu.body
    in
    hlp_bvs
  in
  hlp VarSet.empty init

module Flat =
  struct

    let[@inline] shape x =
      match shape x with
      | Mu x -> invalid_arg
        @@ Format.asprintf "OCanren fatal: mu-binders aren't allowed here (%a)" pp (repr x)
      | x -> x

    let map ~fvar ~fval =
      let rec hlp x =
        match shape x with
        | Var x -> fvar x
        | Val _ -> fval x
        | Con (tx, sx, xi) -> make_con tx sx @@ fun i -> hlp @@ xi i
        | Mu _ -> assert false
      in
      hlp

    let iter ~fvar ~fval =
      let rec hlp x =
        match shape x with
        | Var x -> fvar x
        | Val _ -> fval x
        | Con (_, sx, xi) ->
          for i = 0 to sx - 1 do
            hlp @@ xi i
          done
        | Mu _ -> assert false
      in
      hlp

    let fold ~fvar ~fval ~init =
      let rec hlp acc x =
        match shape x with
        | Var x -> fvar acc x
        | Val _ -> fval acc x
        | Con (_, sx, xi) ->
          let rec inner i acc =
            if i < sx then inner (i + 1) @@ hlp acc @@ xi i
            else acc
          in
          inner 0 acc
        | Mu _ -> assert false
      in
      hlp init

    exception Different_shape of int * int

    type label = L | R

    let fold2 ~fvar ~fval ~fk ~init =
      let rec hlp acc x y =
        match shape x, shape y with
        | Var x, Var y -> fvar acc x y
        | Var x, _ -> fk acc L x y
        | _, Var y -> fk acc R y x
        | Val tx, Val ty ->
          if tx = ty then fval acc x y
          else raise @@ Different_shape (tx, ty)
        | Val tx, Con (ty, _, _) | Con (tx, _, _), Val ty -> raise @@ Different_shape (tx, ty)
        | Con (tx, sx, xi), Con (ty, sy, yi) ->
          if tx = ty && sx = sy then
            let rec inner i acc =
              if i < sx then inner (i + 1) @@ hlp acc (xi i) (yi i)
              else acc
            in
            inner 0 acc
          else raise @@ Different_shape (tx, ty)
        | Mu _, _ | _, Mu _ -> assert false
      in
      hlp init

  module Int64_IO : sig

      val input : in_channel -> int64
      val output : out_channel -> int64 -> unit
  end = struct

      let buf = Bytes.create 8

      let output chan x =
        Bytes.set_int64_le buf 0 x ;
        output_bytes chan buf

      let input chan =
        really_input chan buf 0 8 ;
        Bytes.get_int64_le buf 0
  end

  let output_int64 = Int64_IO.output
  let input_int64 = Int64_IO.input

  (*
   * Variable:     [0 : byte] [index  : int64  ]
   * Int value:    [1 : byte] [value  : int64  ]
   * Float value:  [2 : byte] [value  : float64]
   * String value: [3 : byte] [length : int64  ] [bytes...]
   * Functor:      [4 : byte] [tag    : int64  ] [arity : int64] [subterms...]
   *)
  let rec marshal chan x =
    match shape x with
    | Var x ->
      output_byte chan 0 ;
      output_int64 chan @@ Int64.of_int x.Var.index
    | Val tx ->
      if is_int tx then begin
        output_byte chan 1 ;
        output_int64 chan @@ Int64.of_int @@ obj x
      end else if is_dbl tx then begin
        output_byte chan 2 ;
        output_int64 chan @@ Int64.bits_of_float @@ obj x
      end else if is_str tx then begin
        output_byte chan 3 ;
        let value : string = obj x in
        output_int64 chan @@ Int64.of_int @@ String.length value ;
        output_string chan value
      end else
        assert false
    | Con (tx, sx, xi) ->
        output_byte chan 4 ;
        output_int64 chan @@ Int64.of_int tx ;
        output_int64 chan @@ Int64.of_int sx ;
        for i = 0 to sx - 1 do
          marshal chan @@ xi i
        done

  let rec unmarshal ~env ~scope chan =
    match input_byte chan with
    | 0 -> repr @@ Var.make ~env ~scope @@ Int64.to_int @@ input_int64 chan
    | 1 -> repr (Int64.to_int @@ input_int64 chan : int)
    | 2 -> repr (Int64.float_of_bits @@ input_int64 chan : float)
    | 3 ->
        let length = Int64.to_int @@ input_int64 chan in
        Obj.repr (Stdlib.Option.get @@ In_channel.really_input_string chan length : string)
    | 4 ->
        let tx = Int64.to_int @@ input_int64 chan in
        let sx = Int64.to_int @@ input_int64 chan in
        let res = Obj.new_block tx sx in
        for i = 0 to sx - 1 do
          Obj.set_field res i @@ unmarshal ~env ~scope chan
        done ;
        res
    | x -> invalid_arg @@ Printf.sprintf "Term.unmarshal: incorrect data (%d)" x
  end
