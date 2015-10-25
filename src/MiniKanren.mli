(*
 * MiniKanren: miniKanren primitives implementation.
 * Copyright (C) 2015
 * Dmitri Boulytchev, Dmitry Kosarev, St.Petersburg State University
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

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

(** State (needed to perform calculations) *)
module State :
  sig
    (** State type *)
    type t

    (** Printing helper *)
    val show : t -> string
  end

(** Step converts a state into a lazy stream of states *)
type step = State.t -> State.t MKStream.t

(** Minikanren integers *)
type int = GT.int

(** Minikanren strings *)
type string = GT.string

(** Minikanren lists *)
type 'a list = 'a GT.list

(** {2 Printing functions} *)

(** Printing helper for minikanren lists (requires an environment) *)
val show_list : State.t -> (State.t -> 'a -> string) -> 'a list -> string

(** Printing helper for minikanren ints (requires an environment) *)
val show_int : State.t -> int -> string

(** Printing helper for minikanren ints (requires an environment) *)
val show_string : State.t -> string -> string

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a step from a functions, which takes a fresh
    logical variable *)
val call_fresh : ('a -> State.t -> 'b) -> State.t -> 'b

(** [x === y] creates a step, which performs a unifications of
    [x] and [y] *)
val (===) : 'a -> 'a -> step

(** [conj s1 s2] creates a step, which is a conjunction of its arguments *)
val conj : step -> step -> step

(** [disj s1 s2] creates a step, which is a disjunction of its arguments *)
val disj : step -> step -> step

(** {2 Top-level running primitives} *)

(** [run s] runs a state transformer [s] (not necessarily a step) in
    initial state *)
val run : (State.t -> 'a) -> 'a

(** [refine s x] refines a logical variable [x] (created with [fresh]) w.r.t.
    state [s] *)
val refine : State.t -> 'a -> 'a
