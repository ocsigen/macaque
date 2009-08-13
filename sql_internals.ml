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

type view =
  { descr : types_descr;
    result_parser : untyped result_parser;
    concrete : concrete_view }
and concrete_view =
  | Table of table_name
  | Selection of select
and select = { select : select_result; from : from; where : where }
and select_result =
  | Simple_select of row
  | Group_by of row * row
and group_by = (row * row)
and from = (row_name * view) list
and where = reference list
and row = reference
and reference = reference' * field_type
and reference' =
  | Null
  | Value of value
  | Field of reference * field_name list
  | Binop of string * reference * reference
  | Unop of string * reference
  | Prefixop of string * reference
  | Row of (row_name * view)
  | Tuple of reference tuple
and value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Record of record
and record =
  { instance : untyped;
    ast_builder : untyped -> reference }
and table_name = string option * string
and row_name = string
and 'a tuple = (field_name * 'a) list
and field_name = string
and descr = types_descr * untyped result_parser
and 'a result_parser = string array * int ref -> 'a
and types_descr = field_type tuple
and field_type =
  | Non_nullable of sql_type
  | Nullable of sql_type option
and sql_type =
  | TInt
  | TFloat
  | TString
  | TBool
  | TRecord of descr * (untyped -> reference)
and untyped = Obj.t

let rec get_field_type ref_type = function
  | [] -> ref_type
  | name :: path_rest ->
      match ref_type with
        | Nullable None -> Nullable None
        | Non_nullable (TRecord ((descr, _), _))
        | Nullable Some (TRecord ((descr, _), _)) ->
            get_field_type (List.assoc name descr) path_rest
        | _ -> invalid_arg "get_field_type"

let sql_type_of_string = function
  | "integer" -> TInt
  | "text" -> TString
  | "boolean" -> TBool
  | "double" -> TFloat
  | other -> failwith ("unknown sql type " ^ other)
let string_of_sql_type = function
  | TInt -> "integer"
  | TString -> "text"
  | TBool -> "boolean"
  | TFloat -> "double"
  | TRecord (_, _) -> "record"

type query =
  | Select of view
  | Insert of (table_name * view)
  | Delete of (table_name * row_name * where)
  | Update of (table_name * row_name * reference * where)

type result = select_result * field_type
