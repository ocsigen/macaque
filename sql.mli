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

open Sql_base

type nullable
type non_nullable

type ('a, 'b) witness
val non_nullable_witness : (non_nullable, bool) witness
val nullable_witness : (nullable, bool) witness

class type ['t] type_info = object method typ : 't end
class type numeric_t = object method numeric : unit end

class type int_t = object inherit [int] type_info inherit numeric_t end
class type bool_t = object inherit [bool] type_info end
class type float_t = object inherit [float] type_info inherit numeric_t end
class type string_t = object inherit [string] type_info end

class type ['row] row_t = object inherit ['row] type_info end

(* used in some coercicions scenario, eg. update *)
type 't type_info_only = < t : 't type_info >

type +'a t

type 'phant binary_op = 'a t -> 'b t -> 'c t
constraint 'a = < t : 'input_t; nul : 'n; .. >
constraint 'b = < t : 'input_t; nul : 'n; .. >
constraint 'c = < t : 'output_t; nul : 'n >
constraint 'phant =
  < input_t : 'input_t; output_t : 'output_t; nul : 'n; a : 'a; b : 'b >

type 'a result_parser = string array * int ref -> 'a

(** access functions *)
val get : < get : _; nul : non_nullable; t : 't #type_info > t -> 't
val getn : < get : _; nul : nullable; t : 't #type_info > t -> 't option

(** parse function *)
val parse : 'a t -> 'a t result_parser

(** untyped access *)
type untyped
val untyped_t : 'a t -> untyped t

type +'a view
val untyped_view : 'a view -> untyped view

(** unsafe constructors *)
type +'a unsafe
val unsafe : 'a -> 'a unsafe

val force_gettable :
  < t : 't; nul : 'nul; .. > t unsafe -> < t : 't; nul : 'nul; get : unit > t

val field :
  < t : 'a #row_t; nul : non_nullable; .. > t ->
  string list unsafe ->
  ('a -> < t : 't; nul : 'n; ..> t) unsafe ->
  <t : 't; nul : 'n> t

val row :
  string unsafe -> 'a view -> < t : < typ : 'a >; nul : non_nullable > t
(* < typ : 'a > instead of 'a row_t to lighten error reporting *)

val tuple :
  untyped t tuple unsafe ->
  'tup result_parser unsafe ->
  < t : < typ : 'tup >; nul : non_nullable > t
(* < typ : 'a > instead of 'a row_t to lighten error reporting *)

(** select and view building *)
type +'a result

type from = untyped view tuple
type where = untyped t list

val view : 'a result -> from -> where -> 'a view
val simple_select : < t : 'a #row_t; .. > t -> 'a result

(** group by and accumulators *)
type grouped_row
val grouped_row : grouped_row

type +'a group
type +'a accum
val accum : 'a t -> 'a accum
val group_of_accum : 'a accum -> 'a group

val group :
  < t : 'const #row_t; .. > t -> < t : 'res #row_t; .. > t -> 'res result


(** tables *)
type +'a table

type +'a sql_type
val untyped_type : 'a sql_type -> untyped sql_type

val get_type : 'a t -> 'a sql_type

type poly_parser =
  { of_type : 'a . 'a sql_type -> 'a t result_parser }

val table :
  untyped sql_type tuple ->
  (poly_parser -> 'row result_parser) ->
  (string option * string) ->
  'row table

(** standard SQL field types
    (in pa_descr, ie. <:table< .. >>) *)
module Table_type : sig
  val integer : ('nul, bool) witness ->
    < get : unit; nul : 'nul; t : int_t > sql_type
  val boolean : ('nul, bool) witness ->
    < get : unit; nul : 'nul; t : bool_t > sql_type
  val text : ('nul, bool) witness ->
    < get : unit; nul : 'nul; t : string_t > sql_type
end

(** final query building *)
type +'a query

val select : 'a view -> 'a list query
val insert : 'a table -> 'a view -> unit query
val delete :
  'a table -> string unsafe -> < t : #bool_t; .. > t list -> unit query
val update :
  'a table -> string unsafe ->
  'b t -> ('a -> 'b, bool) witness unsafe ->
  < t : #bool_t; .. > t list -> unit query

(** query printing *)
val sql_of_query : 'a query -> string
val sql_of_view : 'a view -> string

(** handle result from PGOCaml call *)
val handle_query_results : 'a query -> string option list list unsafe -> 'a

(** standard SQL value types
    (usable from user code, in pa_comp value antiquotations) *)
module Data : sig
  val int : int -> < t : int_t; get : unit; nul : _ > t
  val bool : bool -> < t : bool_t; get : unit; nul : _ > t
  val float : float -> < t : float_t; get : unit; nul : _ > t
  val string : string -> < t : string_t; get : unit; nul : _ > t
end

(** standard SQL operators
    (usable from user code, in pa_comp expressions) *)
module Op : sig
  val null :
    < t : < typ : 'a; numeric : unit >; nul : nullable; get : unit > t
  val nullable :
    < t : 't; nul : non_nullable; .. > t -> < t : 't; nul : nullable > t
  val is_null :
    < nul : nullable; .. > t -> < t : bool_t; nul : non_nullable > t
  val is_not_null :
    < nul : nullable; .. > t -> < t : bool_t; nul : non_nullable > t

  type 'phant arith_op = 'phant binary_op
  constraint 'phant = < input_t : #numeric_t as 't; output_t : 't; .. >

  val (+) : _ arith_op
  val (-) : _ arith_op
  val (/) : _ arith_op
  val ( * ) : _ arith_op

  type 'phant comp_op = 'phant binary_op
  constraint 'phant = < output_t : bool_t; .. >

  val (<) : _ comp_op
  val (<=) : _ comp_op
  val (=) : _ comp_op
  val (<>) : _ comp_op
  val (>=) : _ comp_op
  val (>) : _ comp_op
  val is_distinct_from :
    < nul : 'n; t : 't; .. > t ->
    < nul : 'n; t : 't; .. > t ->
    < nul : non_nullable; t : bool_t > t
  val is_not_distinct_from :
    < nul : 'n; t : 't; .. > t ->
    < nul : 'n; t : 't; .. > t ->
    < nul : non_nullable; t : bool_t > t

  type 'phant logic_op = 'phant binary_op
  constraint 'phant = < input_t : #bool_t as 't; output_t : 't; .. >

  val (&&) : _ logic_op
  val (||) : _ logic_op
  val not :
    < t : #bool_t; nul : 'n; .. > t -> < t : bool_t; nul : 'n; > t

  val count :
    _ group -> < t : int_t; nul : non_nullable > t
  val max :
    < t : #numeric_t as 't; nul : 'n; .. > group -> < t : 't; nul : 'n > t
  val sum :
    < t : #numeric_t as 't; nul : 'n; .. > group -> < t : 't; nul : 'n > t
end

(** standard view operators
   (in pa_comp, view antiquotations) *)
module View : sig
  val table : 'a table -> 'a view

  val one : < t : 'a #row_t; .. > t -> 'a view
end
