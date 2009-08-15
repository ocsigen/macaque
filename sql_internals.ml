(* macaque : sql_internals.ml
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

type 'a generic_view =
  { descr : descr;
    result_parser : untyped result_parser;
    data : 'a }
and view = concrete_view generic_view
and table = table_name generic_view
and concrete_view =
  | Table of table_name
  | Selection of select
and select = { select : select_result; from : from; where : where }
and select_result =
  | Simple_select of row
  | Group_by of row * row
and group_by = (row * row)
and from = (row_name * view) list
and where = value list
and row = value
and value = value' * sql_type
and value' =
  | Null
  | Atom of atom
  | Field of value * field_name list
  (* | Cast of value * atom_type TODO *)
  | Op of value list * string * value list
  | Row of (row_name * view)
  | Tuple of value tuple
  | Case of (value * value) list * value (* [when ..  then ..]+ else ..*)
and atom =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Record of untyped (* runtime object instance *)
and table_name = string option * string
and row_name = string
and field_name = string
and descr = sql_type tuple
and sql_type =
  | Non_nullable of atom_type
  | Nullable of atom_type option
and atom_type =
  | TInt
  | TFloat
  | TString
  | TBool
  | TRecord of unit generic_view

let rec get_sql_type ref_type = function
  | [] -> ref_type
  | name :: path_rest ->
      match ref_type with
        | Nullable None -> Nullable None
        | Non_nullable (TRecord {descr=descr})
        | Nullable Some (TRecord {descr=descr})->
            get_sql_type (List.assoc name descr) path_rest
        | _ -> invalid_arg "get_sql_type"

let atom_type_of_string = function
  | "integer" -> TInt
  | "text" -> TString
  | "boolean" -> TBool
  | "double" -> TFloat
  | other -> failwith ("unknown sql type " ^ other)
let string_of_atom_type = function
  | TInt -> "integer"
  | TString -> "text"
  | TBool -> "boolean"
  | TFloat -> "double"
  | TRecord _ -> "record"

type query =
  | Select of view
  | Insert of (table * view)
  | Delete of (table * row_name * where)
  | Update of (table * row_name * value * where)

type result = select_result * sql_type
