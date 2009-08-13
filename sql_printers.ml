(* macaque : sql_printers.ml
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

open Printf

let string_of_list printer sep li = String.concat sep (List.map printer li)

let rec string_of_view view = string_of_concrete view.concrete
and string_of_concrete = function
| Selection q -> sprintf "(%s)" (string_of_selection q)
| Table table_name -> string_of_table_name table_name
and string_of_selection q =
  sprintf "SELECT %s%s%s%s"
    (string_of_row (match q.select with
                      | Simple_select result
                      | Group_by (result, _) -> result))
    (string_of_from q.from)
    (string_of_where q.where)
    (match q.select with
       | Group_by (result, (Tuple (_::_ as const), _)) ->
           " GROUP BY " ^
             string_of_list (fun (_, r) -> string_of_reference r) ", " const
       | _ -> "")
and string_of_from = function
  | [] -> ""
  | from -> " FROM " ^ string_of_list string_of_table ", " from
and string_of_where = function
  | [] -> ""
  | where -> " WHERE " ^ string_of_list string_of_reference " AND " where
and string_of_row (ref, ref_type) = match ref with
  | Tuple tup ->
      if tup = [] then "NULL"
      else
        let binding (id, ref) =
          (* recursive call instead of string_of_reference
             as there may be flattened subtuples *)
          let ref_str = string_of_row ref in
          match (fst ref) with
            | Row _ | Tuple _ -> ref_str
            | _ -> sprintf "%s AS %s" ref_str id in
        string_of_list binding ", " tup
  | _ -> string_of_reference (ref, ref_type)
and string_of_assoc (assoc, _) =
  match assoc with
    | Tuple tup ->
        let binding (id, ref) = sprintf "%s = %s" id (string_of_reference ref) in
        string_of_list binding ", " tup
    | _ -> invalid_arg "string_of_assoc"
and string_of_reference (ref, _) =
  match ref with
    | Value v -> string_of_value v
    | Null -> "NULL"
    | Unop (op, a) ->
        sprintf "%s(%s)" op (string_of_reference a)
    | Prefixop (op, a) -> sprintf "(%s %s)" (string_of_reference a) op
    | Binop (op, a, b) -> sprintf "(%s %s %s)"
        (string_of_reference a) op (string_of_reference b)
    | Field ((Row (row_name, _), _), fields) ->
        sprintf "%s.%s" row_name (String.concat "__" fields)
    | Field (_, _) -> failwith "string_of_reference : invalid field access"
    | Row (row_name, _) -> row_name
    | Tuple tup ->
        sprintf "ROW(%s)"
          (string_of_list (fun (_, r) -> string_of_reference r) ", " tup)
and string_of_field (row, name) = match name with
  | field_name when true -> sprintf "%s.%s" row field_name
  | _ -> assert false
and string_of_table (row_name, table) =
  sprintf "%s AS %s" (string_of_view table) row_name
and string_of_table_name = function
  | (None, table) -> table
  | (Some schema, table) -> sprintf "%s.%s" schema table
and string_of_value = function
  | Int i -> string_of_int i
  | String s -> sprintf "'%s'" (String.escaped s)
  | Bool b -> string_of_bool b
  | Float x -> string_of_float x
  | Record (obj, ast_builder) -> string_of_reference (ast_builder obj)


let rec string_of_query = function
  | Select view -> string_of_view view
  | Insert (table, view) ->
      sprintf "INSERT INTO %s (%s)"
        (string_of_table_name table)
        (string_of_view view)
  | Delete (table, row, where) ->
      sprintf "DELETE FROM %s AS %s%s"
        (string_of_table_name table) row
        (string_of_where where)
  | Update (table, row, set, where) ->
      sprintf "UPDATE %s AS %s SET %s%s"
        (string_of_table_name table) row
        (string_of_assoc set)
        (string_of_where where)

(* TODO : move back in sql.ml *)
open Sql_flatten
open Sql_types

let sql_of_query q = string_of_query (flatten_query q)
let sql_of_view v = sql_of_query (Select (untyped_view v))


