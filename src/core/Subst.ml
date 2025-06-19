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

let is_var t = Term.unterm t ~fvar:(fun _ -> true)
  ~fval:(fun _ _ -> false) ~fcon:(fun _ _ _ -> false) ~fmu:(fun _ -> false)

(* term head is a constructor with variables as arguments *)
let is_term_head t =
  Term.unterm t ~fvar:(fun _ -> false) ~fval:(fun _ _ -> true) ~fmu:(fun _ -> false)
    ~fcon:begin fun _ sx xi ->
      let rec inner i =
        if i < sx
        then is_var (xi i) && inner (i + 1)
        else true
      in

      inner 0
    end

(* every right hand side must be either a variable or a term head *)
type t = Term.t Term.VarMap.t

let empty = Term.VarMap.empty

let of_map m =
  (* TODO(ProgMiner): currently in Disequality we may have complex right hand sides but they
     mustn't occur in a substitution to prevent infinite looping. For now, I just forbid them but
     in future we need to decide how to avoid them. See "regression/test023diseq.ml" to example *)
  let hlp _ t =
    if not (is_var t || is_term_head t) then begin
      failwith "OCanren fatal: not a term head nor a variable in right hand side"
    end
  in
  Term.VarMap.iter hlp m ;
  m

let split s = Term.VarMap.fold (fun var term xs -> Binding.{ var ; term }::xs) s []

let pp ppf s =
  let open Format in
  fprintf ppf "{subst| " ;
  Term.VarMap.iter (fun x t -> fprintf ppf "%a |- %a; " Term.pp (Term.repr x) Term.pp t) s ;
  fprintf ppf "|subst}"

(* returns representing variable and optional term head *)
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

    Env.unterm_flat env t ~fvar:walkv
      ~fval:(fun _ _ -> v, Some t)
      ~fcon:(fun _ _ _ -> v, Some t)
  in

  walkv

(* [var] must be free in [subst], [term] must be either a different variable or a term head *)
let extend ~scope env subst var term =
  (* TODO(ProgMiner): implement occurs check in other place *)
  (* if Runconf.do_occurs_check () then occurs env subst var term; *)

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

(* [var] must be free in [subst], [term] mustn't be a variable or a mu-binder.
 * [inject] "x = f(t1, ..., tn)" replaces terms "ti" with fresh variables "yi",
 * then injects "yi = ti" in [subst], then extends [subst] with "x = f(y1, ..., yn)"
 *)
let inject ~scope env subst =
  let extend = extend ~scope env in
  let subst = ref subst in

  let rec inject var term =
    let non_var term =
      let var = Env.fresh ~scope env in
      inject var term ;
      Term.repr var
    in

    let hlp term = Env.unterm_flat env term ~fvar:Term.repr
      ~fval:(fun _ _ -> non_var term) ~fcon:(fun _ _ _ -> non_var term)
    in

    let term = Term.map_head hlp term in
    subst := extend !subst var term
  in

  fun var term ->
    inject var term ;
    !subst

(* we must distinguish between regular extending and "prefire" to hold substitution prefix
 * in consistent state, e.g. for substitution [x -> 1, y -> 1] when we unify "x = y"
 * we mustn't add [x -> y] in prefix since actual substitution doesn't extending
 *)
type union_extend = Extend | Prefire

let union env subst x y =
  let x, t1 = walk env subst x in
  let y, t2 = walk env subst y in

  if Term.Var.equal x y then None, None
  else
    let x, y = match t2 with
    | Some _ -> y, x
    | None -> x, y
    in

    let ext, ts = match t1, t2 with
    | Some t1, Some t2 -> Prefire, Some (t1, t2)
    | _ -> Extend, None
    in

    Some (ext, y, (Term.repr x)), ts

exception Unification_failed

let unify ?(scope=Term.Var.non_local_scope) env subst x y =
  let extend = extend ~scope env in

  (* The idea is to do the unification and collect the unification prefix during the process *)
  let extend_prefix prefix var term = Binding.{ var ; term }::prefix in

  let rec helper x y acc = Term.Flat.fold2 x y ~init:acc
    ~fvar:begin fun ((prefix, subst) as acc) x y ->
      let ext, ts = union env subst x y in

      let acc = match ext with
      | None -> acc
      | Some (Extend, x, y) -> extend_prefix prefix x y, extend subst x y
      | Some (Prefire, x, y) -> prefix, extend subst x y
      in

      match ts with
      | None -> acc
      | Some (x, y) -> helper x y acc
    end
    ~fval:begin fun acc _ x y ->
      if x = y then acc
      else raise Unification_failed
    end
    ~fk:begin fun ((prefix, subst) as acc) _ x y ->
      match walk env subst x with
      | x, None -> extend_prefix prefix x y, inject ~scope env subst x y
      | _, Some x -> helper x y acc
    end
  in

  try
    let x, y = Term.(repr x, repr y) in
    Some (helper x y ([], subst))
  with Term.Flat.Different_shape _ | Unification_failed -> None

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
  let[@inline] unvar x =
    let non_var () = invalid_arg
      @@ Format.asprintf "OCanren fatal: not a term head in substitution right hand side %a"
        pp subst
    in
    Env.unterm_flat env x ~fvar:Fun.id ~fval:(fun _ _ -> non_var ()) ~fcon:(fun _ _ _ -> non_var ())
  in

  let vis = Term.VarTbl.create 16 in
  let rec hlp x = match walk env subst x with
  | x, None -> Term.repr x
  | x, Some t ->
    if Term.VarTbl.mem vis x then begin
      Term.VarTbl.replace vis x true ;
      Term.repr x
    end else begin
      Term.VarTbl.add vis x false ;
      let t = Term.map_head (fun x -> hlp (unvar x)) t in
      let t = if Term.VarTbl.find vis x then Term.repr @@ Term.Mu.make x t else t in
      Term.VarTbl.remove vis x ;
      t
    end
  in

  hlp

let apply env subst = Term.Flat.map ~fvar:(image env subst) ~fval:(fun _ -> Term.repr)

let freevars env subst x = Env.freevars env @@ apply env subst x

module Answer =
  struct

    type t = Term.t
  end

let reify env subst x = apply env subst (Term.repr x)
