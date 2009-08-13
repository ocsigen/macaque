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

open Sql_internals
module Sql_P = Sql_parsers

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

type 'a t = reference
let untyped_t x = x

let get_reference r = r
let get_type (_, t) = t

type 'phant binary_op = 'a t -> 'b t -> 'c t
constraint 'a = < t : 'input_t; nul : 'n; .. >
constraint 'b = < t : 'input_t; nul : 'n; .. >
constraint 'c = < t : 'output_t; nul : 'n >
constraint 'phant =
  < input_t : 'input_t; output_t : 'output_t; nul : 'n; a : 'a; b : 'b >

let untyped_view view =
  { view with result_parser =
      Sql_P.unsafe_parser view.result_parser }

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
    | Int i -> !?i
    | Float x -> !?x
    | Bool b -> !?b
    | String s -> !?s
    | Record (o, _) -> !?o

let get (r, t) =
  match r with
    | Value v -> get_val v
    | _ -> invalid_arg "get"

let getn (r, t) = match r with
  | Null -> None
  | Value v -> Some (get_val v)
  | _ -> invalid_arg "getn"

(* TODO : move back in Sql.ml *)
let parse ref =
  Sql_P.use_unsafe_parser (Sql_P.parser_of_type (get_type ref))


type grouped_row = unit
let grouped_row = ()

type 'a accum = 'a t
type 'a group = 'a t

let accum x = x
let group_of_accum x = x


(* TODO : move back in Sql.ml or find a place for magic stuff *)
let handle_query_results sql_query result =
  let parse row_parser = fun row ->
    let nullable = function
      | None -> "NULL"
      | Some str -> str in
    row_parser (Array.of_list (List.map nullable row)) in
  match sql_query with
    | Select comp ->
        Obj.magic (List.map (parse (Sql_P.parser_of_comp comp)) result)
    | _ -> Obj.magic ()
