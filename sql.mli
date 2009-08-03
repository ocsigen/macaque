(* macaque : sql.mli
    MaCaQue : Macros for Caml Queries
    Copyright (C) 2009 Gabriel Scherer, Jérôme Vouillon

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library; see the file LICENSE.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.
*)

type true_t
type false_t

type +'a t

type 'a result_parser = string array * int ref -> 'a

(** access functions *)
val get : < t : 't; gettable : true_t; nullable : false_t; .. > t -> 't
val getn : < t : 't; gettable : true_t; nullable : true_t; .. > t -> 't option

(** parse function *)
val parse : 'a t -> 'a t result_parser

(** untyped access *)
type untyped
val untyped : 'a t -> untyped t

type +'a view
val untyped_view : 'a view -> untyped view

(** unsafe constructors *)
type +'a unsafe
val unsafe : 'a -> 'a unsafe

val force_gettable :
  < t : 't; nullable : 'nul; numeric : 'num; gettable : _ > t unsafe ->
  < t : 't; nullable : 'nul; numeric : 'num; gettable : true_t > t

val field : < t : 'a; nullable : false_t; .. > t -> string list unsafe -> ('a -> 'b t) unsafe -> 'b t
val row :
  string unsafe ->
  'row view ->
  < t : 'row; numeric : false_t; nullable : false_t; gettable : false_t > t
val tuple :
  (string * untyped t) list unsafe ->
  'tup result_parser unsafe ->
  < t : 'tup; numeric : false_t; nullable : false_t; gettable : false_t > t

(** select and view building *)
type 'a result

type from = (string * untyped view) list
type where = untyped t list

val view : 'a result -> from -> where -> 'a view
val simple_select : < t : 'a; .. > t -> 'a result

(** group by and accumulators *)
type grouped_row
val grouped_row : grouped_row

type 'a group
type 'a accum
val accum : 'a t -> 'a accum
val group_of_accum : 'a accum -> 'a group

val group : < t : 'group_const; .. > t -> < t : 'res; .. > t -> 'res result

(** final query building *)
type 'a query

val select : 'a view -> 'a list query
val insert : 'a view -> 'a view -> unit query
val delete : 'a view -> string unsafe -> < t : bool; .. > t list -> unit query
val update :
  'a view ->
  string unsafe ->
  'b t ->
  (< t : 'a; .. > t -> 'b t) unsafe ->
  < t : bool; .. > t list ->
  unit query

(** query printing *)
val sql_of_query : 'a query -> string
val sql_of_view : 'a view -> string

(** handle result from PGOCaml call *)
val handle_query_results : 'a query -> string array unsafe list -> 'a


(** standard data types (usable from user code) *)
module Data : sig
  val int : int -> < t : int; numeric : true_t; nullable : false_t; gettable : true_t > t
  val bool : bool -> < t : bool; numeric : false_t; nullable : false_t; gettable : true_t > t
  val float : float -> < t : float; numeric : true_t; nullable : false_t; gettable : true_t > t
  val string : string -> < t : string; numeric : false_t; nullable : false_t; gettable : true_t > t
end

(** standard operators (usable from user code) *)
module Op : sig
  val null : < nullable : true_t; t : _; numeric : _; gettable : true_t > t
  val nullable :
    < t : 't; numeric : 'n; gettable : 'g; nullable : false_t > t ->
    < t : 't; numeric : 'n; gettable : 'g; nullable : true_t > t
  val is_null : < nullable : true_t; .. > t ->
    < t : bool; numeric : false_t; gettable : false_t; nullable : false_t > t
  val is_not_null : < nullable : true_t; .. > t ->
    < t : bool; numeric : false_t; gettable : false_t; nullable : false_t > t

  type 'phant arith_op = 'a t -> 'b t -> 'c t
  constraint 'a = < numeric : true_t; t : 't; nullable : 'n; .. >
  constraint 'b = < numeric : true_t; t : 't; nullable : 'n; .. >
  constraint 'c = < numeric : true_t; t : 't; nullable : 'n; gettable : false_t >
  constraint 'phant = < t : 't; nullable : 'n; a : 'a; b : 'b >

  val (+) : _ arith_op
  val (-) : _ arith_op
  val (/) : _ arith_op
  val ( * ) : _ arith_op

  type 'phant comp_op = 'a t -> 'b t -> 'c t
  constraint 'a = < nullable : 'nul; t : 't; numeric : 'num; .. >
  constraint 'b = < nullable : 'nul; t : 't; numeric : 'num; .. >
  constraint 'c = < nullable : 'nul; t : bool; numeric : false_t; gettable : false_t >
  constraint 'phant = < nullable : 'nul; t : 't; numeric : 'num; a : 'a; b : 'b >

  val (<) : _ comp_op
  val (<=) : _ comp_op
  val (=) : _ comp_op
  val (<>) : _ comp_op
  val (>=) : _ comp_op
  val (>) : _ comp_op
  val is_distinct_from :
    < nullable : 'n; t : 't; .. > t ->
    < nullable : 'n; t : 't; .. > t ->
    < nullable : false_t; t : bool; gettable : false_t; numeric : false_t > t
  val is_not_distinct_from :
    < nullable : 'n; t : 't; .. > t ->
    < nullable : 'n; t : 't; .. > t ->
    < nullable : false_t; t : bool; gettable : false_t; numeric : false_t > t

  type 'phant logic_op = 'a t -> 'b t -> 'c t
  constraint 'a = < t : bool; nullable : 'n; .. >
  constraint 'b = < t : bool; nullable : 'n; .. >
  constraint 'c = < t : bool; nullable : 'n; numeric : false_t; gettable : false_t >
  constraint 'phant = < nullable : 'n; a : 'a; b : 'b >

  val (&&) : _ logic_op
  val (||) : _ logic_op
  val not :
    < t : bool; nullable : 'n; .. > t ->
    < t : bool; nullable : 'n; numeric : false_t; gettable : false_t > t

  val count : _ group ->
    < t : int; numeric : true_t; nullable : false_t; gettable : false_t > t
  val max :
    < t : 't; numeric : true_t; nullable : 'n; gettable : _ > group ->
    < t : 't; numeric : true_t; nullable : 'n; gettable : false_t > t
  val sum :
    < t : 't; numeric : true_t; nullable : 'n; gettable : _ > group ->
    < t : 't; numeric : true_t; nullable : 'n; gettable : false_t > t
end
