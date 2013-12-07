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
module SQLI = Sql_internals

type 'a writable
type non_writable

type nullable
type non_nullable

class type ['t] type_info = object method typ : 't end
class type numeric_t = object method numeric : unit end

class type unit_t = object inherit [unit] type_info end
class type bool_t = object inherit [bool] type_info end
class type int16_t = object inherit [int16] type_info inherit numeric_t end
class type int32_t = object inherit [int32] type_info inherit numeric_t end
class type int64_t = object inherit [int64] type_info inherit numeric_t end
class type float_t = object inherit [float] type_info inherit numeric_t end
class type string_t = object inherit [string] type_info end
class type bytea_t = object inherit [bytea] type_info end
class type time_t = object inherit [time] type_info end
class type date_t = object inherit [date] type_info end
class type timestamp_t = object inherit [timestamp] type_info end
class type timestamptz_t = object inherit [timestamptz] type_info end
class type interval_t = object inherit [interval] type_info end
class type int32_array_t = object inherit [int32 array] type_info end
class type string_array_t = object inherit [string array] type_info end

class type ['row] row_t = object inherit ['row] type_info end

type 't type_info_only = < t : 't type_info >

type +'a t = SQLI.value
let untyped_t x = x

type 'phant binary_op = 'a t -> 'b t -> 'c t
constraint 'a = < t : 'in_t; nul : 'n; .. >
constraint 'b = < t : 'in_t; nul : 'n; .. >
constraint 'c = < t : 'out_t; nul : 'n >
constraint 'phant =
  < in_t : 'in_t; out_t : 'out_t; nul : 'n; a : 'a; b : 'b >

type 'a record_parser = 'a SQLI.record_parser

type (+'a, 'w) view = SQLI.view
let untyped_view view = view

type +'a query = SQLI.query
type where = SQLI.where
type from = SQLI.from

type 'a sql_type = SQLI.sql_type
let untyped_type x = x
let recover_type x y =
  assert (SQLI.is_unifiable x y); y

let get_type (_, t) = t


type +'a result = SQLI.result
constraint 'a = < .. >

type 'a unsafe = 'a
let unsafe (x : 'a) = (x : 'a unsafe)

let force_gettable (x : < .. > t) = (x : < get : unit; .. > t)

type ('a, 'b) witness = 'b
type 'n nul_witness = ('n, bool) witness

let nullable_witness = true
let non_nullable_witness = false

type 'a atom = SQLI.atom

let get_val : < get : _; t : 'a #type_info; .. > atom -> 'a =
  let (!?) = Obj.magic in
  (* the magic is correct by type safety of 'a t *)
  function
    | SQLI.Unit u -> !?u
    | SQLI.Bool b -> !?b
    | SQLI.Int16 i -> !?i
    | SQLI.Int32 i -> !?i
    | SQLI.Int64 i -> !?i
    | SQLI.Float x -> !?x
    | SQLI.Bytea t -> !?t
    | SQLI.String s -> !?s
    | SQLI.Time t -> !?t
    | SQLI.Date d -> !?d
    | SQLI.Timestamp t -> !?t
    | SQLI.Timestamptz t -> !?t
    | SQLI.Interval i -> !?i
    | SQLI.Int32_array js -> !?js
    | SQLI.String_array js -> !?js
    | SQLI.Record o -> !?o

let get ((r, t) : 'a t) =
  match r with
    | SQLI.Atom (v : 'a atom) -> get_val v
    | _ -> invalid_arg "get"

let getn ((r, t) : 'a t) = match r with
  | SQLI.Null -> None
  | SQLI.Atom (v : 'a atom) -> Some (get_val v)
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
      | SQLI.Select comp ->
          !? (List.map (Sql_parsers.parser_of_comp comp) result)
      | SQLI.Value value ->
        begin match result with
          (* the failure cases below correspond to a case where the
             query for a single SQL value would return multiple
             results or not. The typed interface of the Sql module
             should guarantee that this can't happen -- that would
             correspond to an ill-defined value constructor. But it's
             possible that a bug there or a bogus SQL server reach
             them, and it would be nice to have some better failure
             reporting here.

             The null case is especially a good candidate for queries
             gone wrong; for example, a bad SQL server could report no
             error but still return nothing.  *)
          | [] -> assert false (* TODO *)
          | _ :: _ :: _ -> assert false (* TODO *)
          | [line] ->
            !? (Sql_parsers.parser_of_type (get_type value) (line, ref 0))
        end
      | SQLI.Insert _ | SQLI.Update _ | SQLI.Delete _ -> !? ()

let break x = x
let break_view x = x
