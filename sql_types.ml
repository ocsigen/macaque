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

type +'a t = Sql_internals.reference
let untyped_t x = x

let get_reference r = r
let get_type (_, t) = t

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

(* TODO : create sql_base.ml for that stuff (type 'a result_parser, etc.) *)
type untyped = Sql_internals.untyped
type 'a result_parser = 'a Sql_internals.result_parser

type +'a result = Sql_internals.result

type 'a unsafe = 'a
let unsafe (x : 'a) = (x : 'a unsafe)

let force_gettable (x : < .. > t) = (x : < get : unit; .. > t)

type ('a, 'b) witness = 'b

let nullable_witness = true
let non_nullable_witness = false

let get_val =
  (* correct by type safety, see get/getn interfaces *)
  let (!?) = Obj.magic in
  function
    | Sql_internals.Int i -> !?i
    | Sql_internals.Float x -> !?x
    | Sql_internals.Bool b -> !?b
    | Sql_internals.String s -> !?s
    | Sql_internals.Record {Sql_internals.instance = o} -> !?o

let get (r, t) =
  match r with
    | Sql_internals.Value v -> get_val v
    | _ -> invalid_arg "get"

let getn (r, t) = match r with
  | Sql_internals.Null -> None
  | Sql_internals.Value v -> Some (get_val v)
  | _ -> invalid_arg "getn"

type grouped_row = unit
let grouped_row = ()

type 'a accum = 'a t
type 'a group = 'a t

let accum x = x
let group_of_accum x = x

let handle_query_results sql_query result =
  let parse row_parser = fun row ->
    let nullable = function
      | None -> "NULL"
      | Some str -> str in
    row_parser (Array.of_list (List.map nullable row)) in
  let (!?) = Obj.magic in
  match sql_query with
    | Sql_internals.Select comp ->
        !? (List.map (parse (Sql_parsers.parser_of_comp comp)) result)
    | _ -> !? ()
