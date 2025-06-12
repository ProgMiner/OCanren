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

(* [Term] encapsulates unsafe operations on untyped OCaml's values extended with the logic variables *)

(* [t] type of untyped OCaml term *)
type t

(* [Var] logic variables and operations on them *)
module Var :
  sig

    type term = t
    type env = int
    type scope
    type anchor

    type t =
      { anchor        : anchor;
        env           : env;
        index         : int;
        mutable subst : term option;
        scope         : scope;
        constraints   : term list
      }

    val tabling_env : env

    val non_local_scope : scope
    val new_scope : unit -> scope

    val make : env:env -> scope:scope -> int -> t

    val reify : (term -> 'b) -> t -> int * 'b list

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int

    val describe : Format.formatter -> t -> unit
  end

module VarSet : Set.S with type elt = Var.t

module VarTbl : Hashtbl.S with type key = Var.t

module VarMap :
  sig

    include Map.S with type key = Var.t

    val iteri: (int -> key -> 'a -> unit) -> 'a t -> unit
  end

(* [Mu] recursive term binders *)
module Mu :
  sig

    type term = t
    type anchor

    type t = {
      anchor: anchor;
      var: Var.t;
      body: term;
    }

    val make : Var.t -> term -> t
  end

type value

val repr : 'a -> t

(* [unterm ~fvar ~fval ~fcon ~fmu x] matches term using
 * [fvar x] for variable [x], [fval tag x] for primitive [x],
 * [fcon tag size arg x] for constructor [x] of form "tag(arg_1, ..., arg_size)"
 * and [fmu x] for mu-binder [x]
 *)
val[@inline] unterm
  : fvar:(Var.t -> 'a)
 -> fval:(int -> value -> 'a)
 -> fcon:(int -> int -> (int -> t) -> 'a)
 -> fmu:(Mu.t -> 'a)
 -> t -> 'a

val show : t -> string
val pp : Format.formatter -> t -> unit

val equal   : t -> t -> bool
val compare : t -> t -> int
val hash    : t -> int

(* [map_head f x] maps top-level constructor arguments of OCaml's value extended with logic variables;
 *   leaves other forms of term unchanged
 *)
val map_head : (t -> t) -> t -> t

(* [unsafe_map ~fvar ~fval x] maps OCaml's value extended with logic variables and mu-binders;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
 *   bound variables are skipped.
 *   It is unsafe to use for substitution due to possible variables capturing!
 * TODO(ProgMiner): rename into [map] after refactoring
 *)
val unsafe_map : fvar:(Var.t -> t) -> fval:(int -> value -> t) -> t -> t

(* [iter ~fvar ~fval x] iterates over OCaml's value extended with logic variables and mu-binders;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
 *   bound variables are skipped
 *)
val iter : fvar:(Var.t -> unit) -> fval:(int -> value -> unit) -> t -> unit

(* [fold ~fvar ~fval ~init x] fold over OCaml's value extended with logic variables and mu-binders;
 *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
 *   bound variables are skipped
 *)
val fold : fvar:('a -> Var.t -> 'a) -> fval:('a -> int -> value -> 'a) -> init:'a -> t -> 'a

(* [Flat] module operates with flat terms without mu-binders *)
module Flat :
  sig

    val[@inline] unterm
      : fvar:(Var.t -> 'a)
     -> fval:(int -> value -> 'a)
     -> fcon:(int -> int -> (int -> t) -> 'a)
     -> t -> 'a

    val map : fvar:(Var.t -> t) -> fval:(int -> value -> t) -> t -> t
    val iter : fvar:(Var.t -> unit) -> fval:(int -> value -> unit) -> t -> unit
    val fold : fvar:('a -> Var.t -> 'a) -> fval:('a -> int -> value -> 'a) -> init:'a -> t -> 'a

    exception Different_shape of int * int
    type label = L | R

    (* [fold2 ~fvar ~fval ~fk ~init x y] folds two OCaml's value extended with logic variables simultaneously;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
     *   if it finds logic variable in one term but regular value in another term in same place, it calls [fk];
     *   if two terms cannot be traversed simultaneously raises exception [Different_shape (tx, ty)],
     *   where [tx] and [ty] are Ocaml tags of disparate values
     *)
    val fold2
      : fvar:('a -> Var.t -> Var.t -> 'a)
     -> fval:('a -> int -> value -> value -> 'a)
     -> fk:('a -> label -> Var.t -> t -> 'a)
     -> init:'a -> t -> t -> 'a
  end
