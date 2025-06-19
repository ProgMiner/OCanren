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

(* [term] must not be a logic variable *)
type root = {
  depth: int;
  term: Term.t option;
}

type node =
| RootNode of root
| LinkNode of Term.Var.t

let pp_node ppf = let open Format in function
| RootNode { depth ; term = Some term } -> fprintf ppf "Root{ depth = %d; term = %a }" depth Term.pp term
| RootNode { depth } -> fprintf ppf "Root{ depth = %d }" depth
| LinkNode var -> fprintf ppf "Link(%a)" Term.pp @@ Term.repr var

(* mutability is intended to use only for path compression *)
type t = node Term.VarMap.t ref

let empty = ref Term.VarMap.empty

let split s =
  let hlp var node xs =
    let term = match node with
    | RootNode { term } -> term
    | LinkNode var' -> Some (Term.repr var')
    in

    match term with
    | Some term -> Binding.{ var ; term }::xs
    | None -> xs
  in

  Term.VarMap.fold hlp !s []

let pp ppf s =
  let open Format in
  fprintf ppf "{subst| " ;
  Term.VarMap.iter (fun x n -> fprintf ppf "%a |- %a; " Term.pp (Term.repr x) pp_node n) !s ;
  fprintf ppf "|subst}"

let find env subst =
  let rec hlp v =
    let () = IFDEF STATS THEN walk_incr () ELSE () END in
    Env.check_exn env v ;

    match v.Term.Var.subst with
    | Some term' as term ->
      begin match Env.var env term' with
      | Some u -> hlp u (* path compression here is useless *)
      | None ->
        (* omit depth heuristic here in favor of [Var.subst] optimization *)
        v, { depth = 0 ; term }
      end
    | None ->
      match Term.VarMap.find v !subst with
      | exception Not_found -> v, { depth = 0 ; term = None }
      | RootNode r -> v, r
      | LinkNode u ->
        let u, _ as res = hlp u in
        subst := Term.VarMap.add v (LinkNode u) !subst ;
        res
  in

  hlp

let union env subst v u =
  let v, r1 = find env subst v in
  let u, r2 = find env subst u in

  let subst = !subst in
  if r1.depth > r2.depth then
    let subst =
      if r1.term = None && r2.term <> None
      then Term.VarMap.add v (RootNode { r1 with term = r2.term }) subst
      else subst
    in
    ref @@ Term.VarMap.add u (LinkNode v) subst
  else if r1.depth < r2.depth then
    let subst =
      if r2.term = None && r1.term <> None
      then Term.VarMap.add u (RootNode { r2 with term = r1.term }) subst
      else subst
    in
    ref @@ Term.VarMap.add v (LinkNode u) subst
  else
    let term = match r1.term with Some _ as t -> t | None -> r2.term in
    let r1 = { depth = r1.depth + 1 ; term } in
    let subst = Term.VarMap.add v (RootNode r1) subst in
    ref @@ Term.VarMap.add u (LinkNode v) subst

(* [var] must be free in [subst], [term] must not be a logic variable *)
let bind env subst var term =
  let var, root = find env subst var in
  let term = Some term in

  ref @@ Term.VarMap.add var (RootNode { root with term }) !subst

type lterm = Var of Term.Var.t | Value of Term.t

let walk env subst v =
  match find env subst v with
  | _, { term = Some term } -> Value term
  | v, _ -> Var v

(* same as [Term.map] but performs [walk] on the road *)
let map ~fvar ~fval env subst x =
  let rec deepfvar v =
    Env.check_exn env v;
    match walk env subst v with
    | Var v   -> fvar v
    | Value x -> Term.map x ~fval ~fvar:deepfvar
  in
  Term.map x ~fval ~fvar:deepfvar

(* same as [Term.iter] but performs [walk] on the road *)
let iter ~fvar ~fval env subst x =
  let rec deepfvar v =
    Env.check_exn env v;
    match walk env subst v with
    | Var v   -> fvar v
    | Value x -> Term.iter x ~fval ~fvar:deepfvar
  in
  Term.iter x ~fval ~fvar:deepfvar

exception Occurs_check

let rec occurs env subst var term = iter env subst term ~fval:(fun _ -> ())
  ~fvar:(fun v -> if Term.Var.equal v var then raise Occurs_check)

(* [var] must be free in [subst], [term] must not be the same variable *)
let extend ~scope env subst var term =
  if Runconf.do_occurs_check () then occurs env subst var term ;

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
  end else match Env.var env term with
  | Some term -> union env subst var term
  | None -> bind env subst var term

let of_map env m =
  let hlp var term subst = extend ~scope:Term.Var.non_local_scope env subst var term in
  Term.VarMap.fold hlp m empty

exception Unification_failed

let unify ?(subsume=false) ?(scope=Term.Var.non_local_scope) env subst x y =
  (* The idea is to do the unification and collect the unification prefix during the process *)
  let extend var term (prefix, subst) =
    let subst = extend ~scope env subst var term in
    Binding.{ var ; term }::prefix, subst
  in

  let rec helper x y acc = Term.fold2 x y ~init:acc
    ~fvar:begin fun ((_, subst) as acc) x y ->
      match walk env subst x, walk env subst y with
      | Var x, Var y ->
        if Term.Var.equal x y then acc
        else extend x (Term.repr y) acc
      | Var x, Value y -> extend x y acc
      | Value x, Var y -> extend y x acc
      | Value x, Value y  -> helper x y acc
    end
    ~fval:begin fun acc x y ->
      if x = y then acc
      else raise Unification_failed
    end
    ~fk:begin fun ((_, subst) as acc) l v y ->
      if subsume && l = Term.R
      then raise Unification_failed
      else match walk env subst v with
      | Var v   -> extend v y acc
      | Value x -> helper x y acc
    end
  in

  try
    let x, y = Term.(repr x, repr y) in
    Some (helper x y ([], subst))
  with Term.Different_shape _ | Unification_failed | Occurs_check -> None

let unify_map env subst map =
  let vars, terms = Term.VarMap.fold (fun v t (vs, ts) -> Term.repr v :: vs, t::ts) map ([], []) in
  unify env subst vars terms

let merge_disjoint s1 s2 =
  let hlp _ _ = invalid_arg "OCanren fatal (Subst.merge_disjoint): substitutions intersect" in
  ref @@ Term.VarMap.union hlp !s1 !s2

let subsumed env s1 s2 =
  let hlp var term = match unify env s1 var term with
  | Some ([], _) -> true
  | _            -> false
  in
  Term.VarMap.for_all hlp !s2

let apply env subst x = Obj.magic @@ map env subst (Term.repr x) ~fvar:Term.repr ~fval:Term.repr

let freevars env subst x = Env.freevars env @@ apply env subst x

module Answer =
  struct

    type t = Term.t

    let subsumed env x y =
      match unify ~subsume:true env empty y x with
      | Some _ -> true
      | None   -> false
  end

let reify = apply
