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

module Binding :
  sig

    type t =
      { var   : Term.Var.t
      ; term  : Term.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int

    val pp : Format.formatter -> t -> unit
  end

val varmap_of_bindings : Binding.t list -> Term.t Term.VarMap.t

type t

val empty : t

(* right hand sides must be term heads *)
val of_map : Env.t -> Term.t Term.VarMap.t -> t

val split : t -> Binding.t list

val pp : Format.formatter -> t -> unit

(* [unify ~scope env subst x y] performs unification of two flat terms [x] and [y] in [subst].
 *   Unification is a process of finding substituion [s] s.t. [s(x) = s(y)].
 *   Returns [None] if two terms are not unifiable.
 *   Otherwise it returns a pair of diff and new substituion.
 *   Diff is a list of pairs (var, term) that were added to the original substituion.
 *   Current algorithm doesn't forbid recursive terms.
 *)
val unify : ?scope:Term.Var.scope -> Env.t -> t -> 'a -> 'a -> (Binding.t list * t) option

val unify_map : Env.t -> t -> Term.t Term.VarMap.t -> (Binding.t list * t) option

val merge_disjoint : t -> t -> t

(* [subsumed env s1 s2] checks that [s1] is subsumed by [s2] (i.e. [s2] is more general than [s1]).
 *   Subsumption relation forms a partial order on the set of substitutions.
 *)
val subsumed : Env.t -> t -> t -> bool

(* [apply env subst x] applies [subst] to flat term [x] (without mu-binders),
 *   i.e. replaces every variable to image in [subst];
 *   resulting in term with possible mu-binders inside
 *)
val apply : Env.t -> t -> Term.t -> Term.t

(* [freevars env subst x] - returns all free-variables of term [x] *)
val freevars : Env.t -> t -> Term.t -> Term.VarSet.t

module Answer :
  sig

    type t = Term.t
  end

(* see [apply] *)
val reify : Env.t -> t -> 'a -> Answer.t

IFDEF STATS THEN
(** Walk counter *)
val walk_counter : unit -> int
END
