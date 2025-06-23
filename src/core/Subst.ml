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

IFDEF STATS THEN
type stat = {mutable walk_count : int}

let stat = {walk_count = 0}

let walk_counter () = stat.walk_count
let walk_incr () = stat.walk_count <- stat.walk_count + 1
END

(* to avoid clash with Std.List (i.e. logic list) *)
module List = Stdlib.List

module Binding =
  struct

    type t =
      { var   : Term.Var.t
      ; term  : Term.t
      }

    let equal {var=v; term=t} {var=u; term=p} =
      (Term.Var.equal v u) || (Term.equal t p)

    let compare {var=v; term=t} {var=u; term=p} =
      let res = Term.Var.compare v u in
      if res <> 0 then res else Term.compare t p

    let hash {var; term} = Hashtbl.hash (Term.Var.hash var, Term.hash term)

    let pp ppf {var; term} =
      Format.fprintf ppf "{ var.idx = %d; term=%a }" var.Term.Var.index Term.pp term
  end

let varmap_of_bindings : Binding.t list -> Term.t Term.VarMap.t =
  Stdlib.List.fold_left (fun (acc: _ Term.VarMap.t) Binding.{var;term} ->
    assert (not (Term.VarMap.mem var acc));
    Term.VarMap.add var term acc
  )
  Term.VarMap.empty

type t = Term.t Term.VarMap.t

let empty = Term.VarMap.empty

let of_map m = m

let split s = Term.VarMap.fold (fun var term xs -> Binding.{ var ; term }::xs) s []

let pp ppf s =
  let open Format in
  fprintf ppf "{subst| " ;
  Term.VarMap.iter (fun x t -> fprintf ppf "%a |- %a; " Term.pp (Term.repr x) Term.pp t) s ;
  fprintf ppf "|subst}"

(* returns representing variable and optional term *)
let walk env subst =

  (* walk var *)
  let rec walkv v =
    let () = IFDEF STATS THEN walk_incr () ELSE () END in
    Env.check_exn env v ;

    match v.Term.Var.subst with
    | Some term -> walkt v term
    | None ->
        try walkt v (Term.VarMap.find v subst)
        with Not_found -> v, None

  (* walk term *)
  and walkt v t =
    let () = IFDEF STATS THEN walk_incr () ELSE () END in

    match Env.shape_flat env t with
    | Var v -> walkv v
    | _ -> v, Some t
  in

  walkv

(* [var] must be free in [subst], [term] must be either a different variable or a constructor *)
let extend ~scope env subst var term =
  (* It is safe to modify variables destructively if the case of scopes match.
   * There are two cases:
   * 1) If we do unification just after a conde, then the scope is already incremented and nothing goes into
   *    the fresh variables.
   * 2) If we do unification after a fresh, then in case of failure it doesn't matter if
   *    the variable is be distructively substituted: we will not look on it in future.
   *)
  if scope = var.Term.Var.scope && scope <> Term.Var.non_local_scope then begin
    var.subst <- Some term ;
    subst
  end else
    Term.VarMap.add var term subst

exception Unification_failed
exception Occurs_check

let occurs_check env subst roots =
  let vis = Term.VarTbl.create 16 in

  let rec hlp x =
    match walk env subst x with
    | _, None -> ()
    | x, Some term ->
      begin match Term.VarTbl.find vis x with
      | false -> raise Occurs_check
      | true -> ()
      | exception Not_found ->
        Term.VarTbl.add vis x false ;
        Term.Flat.iter ~fvar:hlp ~fval:(fun _ -> ()) term ;
        Term.VarTbl.replace vis x true
      end
  in

  List.iter hlp roots

let unify ?(scope=Term.Var.non_local_scope) env subst x y =
  (* The idea is to do the unification and collect the unification prefix during the process *)
  let extend var term (prefix, subst) =
    Binding.{ var ; term }::prefix, extend ~scope env subst var term
  in

  let rec check_vis x y = function
  | [] -> false
  | (x', y')::vis ->
    if x == x' && y == y' || x == y' && y == x' then true
    else check_vis x y vis
  in

  let rec helper x y vis acc = Term.Flat.fold2 x y ~init:acc
    ~fvar:begin fun ((_, subst) as acc) x y ->
      let x, t1 = walk env subst x in
      let y, t2 = walk env subst y in

      if Term.Var.equal x y then acc
      else match t1, t2 with
      | Some x, Some y ->
        if x == y || check_vis x y vis then acc
        else helper x y ((x, y)::vis) acc
      | _, Some y -> extend x y acc
      | _ -> extend y (Term.repr x) acc
    end
    ~fval:begin fun acc x y ->
      if x = y then acc
      else raise Unification_failed
    end
    ~fk:begin fun ((_, subst) as acc) _ x y ->
      match walk env subst x with
      | x, None -> extend x y acc
      | _, Some x ->
        if x == y || check_vis x y vis then acc
        else helper x y ((x, y)::vis) acc
    end
  in

  let x, y = Term.(repr x, repr y) in
  match helper x y [] ([], subst) with
  | exception Term.Flat.Different_shape _ | exception Unification_failed -> None
  | prefix, subst ->
    let roots () = List.map (fun Binding.{ var } -> var) prefix in
    match if Runconf.do_occurs_check () then occurs_check env subst @@ roots () with
    | exception Occurs_check -> None
    | () -> Some (prefix, subst)

let unify_map env subst map =
  let vars, terms = Term.VarMap.fold (fun v t (vs, ts) -> Term.repr v :: vs, t::ts) map ([], []) in
  unify env subst vars terms

let merge_disjoint env = Term.VarMap.union @@ fun _ _ ->
  invalid_arg "OCanren fatal (Subst.merge_disjoint): substitutions intersect"

let subsumed env subst = Term.VarMap.for_all @@ fun var term ->
  match unify env subst var term with
  | Some ([], _) -> true
  | _            -> false

let image env subst =
  let vis = Term.VarTbl.create 16 in
  let rec hlp x = match walk env subst x with
  | x, None -> Term.repr x
  | x, Some t ->
    if Term.VarTbl.mem vis x then begin
      Term.VarTbl.replace vis x true ;
      Term.repr x
    end else begin
      Term.VarTbl.add vis x false ;
      let t = Term.Flat.map ~fvar:hlp ~fval:Term.repr t in
      let t = if Term.VarTbl.find vis x then Term.repr @@ Term.Mu.make x t else t in
      Term.VarTbl.remove vis x ;
      t
    end
  in

  hlp

let apply env subst = Term.Flat.map ~fvar:(image env subst) ~fval:Term.repr

let freevars env subst x = Env.freevars env @@ apply env subst x

module Answer =
  struct

    type t = Term.t
  end

let reify env subst x = apply env subst (Term.repr x)
