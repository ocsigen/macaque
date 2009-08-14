(* macaque : sql_types.ml
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

class type ['t] type_info = object method typ : 't end
class type numeric_t = object method numeric : unit end

class type int_t = object inherit [int] type_info inherit numeric_t end
class type bool_t = object inherit [bool] type_info end
class type float_t = object inherit [float] type_info inherit numeric_t end
class type string_t = object inherit [string] type_info end

class type ['row] row_t = object inherit ['row] type_info end

type 't type_info_only = < t : 't type_info >

type +'a t = Sql_internals.value
let untyped_t x = x

type 'phant binary_op = 'a t -> 'b t -> 'c t
constraint 'a = < t : 'input_t; nul : 'n; .. >
constraint 'b = < t : 'input_t; nul : 'n; .. >
constraint 'c = < t : 'output_t; nul : 'n >
constraint 'phant =
  < input_t : 'input_t; output_t : 'output_t; nul : 'n; a : 'a; b : 'b >

type +'a view = Sql_internals.view
let untyped_view view = view

type +'a query = Sql_internals.query
type where = Sql_internals.where
type from = Sql_internals.from

type 'a sql_type = Sql_internals.sql_type
let untyped_type x = x

let get_type (_, t) = t


type +'a result = Sql_internals.result

type 'a unsafe = 'a
let unsafe (x : 'a) = (x : 'a unsafe)

let force_gettable (x : < .. > t) = (x : < get : unit; .. > t)

type ('a, 'b) witness = 'b

let nullable_witness = true
let non_nullable_witness = false

type 'a atom = Sql_internals.atom

let get_val : < get : _; t : 'a #type_info; .. > atom -> 'a =
  let (!?) = Obj.magic in
  (* the magic is correct by type safety of 'a t *)
  function
    | Sql_internals.Int i -> !?i
    | Sql_internals.Float x -> !?x
    | Sql_internals.Bool b -> !?b
    | Sql_internals.String s -> !?s
    | Sql_internals.Record {Sql_internals.instance = o} -> !?o

let get ((r, t) : 'a t) =
  match r with
    | Sql_internals.Atom (v : 'a atom) -> get_val v
    | _ -> invalid_arg "get"

let getn ((r, t) : 'a t) = match r with
  | Sql_internals.Null -> None
  | Sql_internals.Atom (v : 'a atom) -> Some (get_val v)
  | _ -> invalid_arg "getn"

type grouped_row = unit
let grouped_row = ()

type 'a accum = 'a t
type 'a group = 'a t

let accum x = x
let group_of_accum x = x

let handle_query_results : 'a query -> string array list -> 'a =
  let (!?) = Obj.magic in
  (* the magic is correct by type safety of 'a query *)
  fun query result ->
    match query with
      | Sql_internals.Select comp ->
          !? (List.map (Sql_parsers.parser_of_comp comp) result)
      | _ -> !? ()

type +'a table = Sql_internals.table

type poly_parser =
  { of_type : 'a . 'a sql_type -> 'a t result_parser }

let poly_parser : poly_parser =
  { of_type = fun _typ ->
      Sql_parsers.use_unsafe_parser (Sql_parsers.parser_of_type _typ) }
