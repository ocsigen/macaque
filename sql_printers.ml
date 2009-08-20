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

open Sql_base
open Sql_internals

open Printf

let string_of_list printer sep li = String.concat sep (List.map printer li)

let rec string_of_view view = string_of_concrete view.data
and string_of_concrete = function
  | Selection q -> sprintf "(%s)" (string_of_selection q)
  | Table table_name -> string_of_table_name table_name
and string_of_selection q =
  let result = match q.select with
    | Simple_select result
    | Group_by (result, _) -> result in
  let group_by = match q.select with
    | Group_by (result, (Tuple (_::_ as const), _)) ->
        " GROUP BY " ^
          string_of_list (fun (_, r) -> string_of_value r) ", " const
    | _ -> "" in
  sprintf "SELECT %s%s%s%s"
    (string_of_row result)
    (string_of_from q.from)
    (string_of_where q.where)
    group_by
and string_of_from = function
  | [] -> ""
  | from -> " FROM " ^ string_of_list string_of_from_item ", " from
and string_of_where = function
  | [] -> ""
  | where -> " WHERE " ^ string_of_list string_of_value " AND " where
and string_of_row (row, row_type) = match row with
  | Tuple tup ->
      if tup = [] then "NULL"
      else
        let item (id, value) =
          (* recursive call instead of string_of_value
             as there may be flattened subtuples *)
          let value_str = string_of_row value in
          match fst value with
            | Row _ | Tuple _ -> value_str
            | _ -> sprintf "%s AS %s" value_str id in
        string_of_list item ", " tup
  | _ -> string_of_value (row, row_type)
and string_of_assoc (assoc, _) =
  match assoc with
    | Tuple tup ->
        let item (id, value) = sprintf "%s = %s" id (string_of_value value) in
        string_of_list item ", " tup
    | _ -> invalid_arg "string_of_assoc"
and string_of_value (value, _) =
  match value with
    | Atom v -> string_of_atom v
    | Null -> "NULL"
    | Row (row_name, _) -> row_name
    | Cast (v, t) ->
        sprintf "CAST(%s AS %s)" (string_of_value v) (string_of_atom_type t)
    | Field ((Row (row_name, _), _), fields) ->
        sprintf "%s.%s" row_name (String.concat path_separator fields)
    | Field (v, _) ->
        failwith (Printf.sprintf "string_of_value : invalid field access (%s)"
                    (string_of_value v))
    | Tuple tup ->
        sprintf "ROW(%s)"
          (string_of_list (fun (_, r) -> string_of_value r) ", " tup)
    | Op ([], op, [v]) -> (* specific unary operator syntax *)
        sprintf "%s(%s)" op (string_of_value v)
    | Op (left, op, right) ->
        sprintf "(%s%s%s)"
          (match left with
             | [] -> ""
             | li -> string_of_list string_of_value " " left ^ " ")
          op
          (match right with
             | [] -> ""
             | li -> " " ^ string_of_list string_of_value " " right)
    | Case ([], default) -> string_of_value default
    | Case (cases, default) ->
        let string_of_case (cond, case) = 
          sprintf "WHEN %s THEN %s"
            (string_of_value cond) (string_of_value case) in
        sprintf "(CASE %s ELSE %s END)"
          (string_of_list string_of_case " " cases)
          (string_of_value default)
and string_of_field (row, name) = match name with
  | field_name when true -> sprintf "%s.%s" row field_name
  | _ -> assert false
and string_of_from_item (row_name, table) =
  sprintf "%s AS %s" (string_of_view table) row_name
and string_of_table table = string_of_table_name table.data
and string_of_table_name = function
  | (None, table) -> table
  | (Some schema, table) -> sprintf "%s.%s" schema table
and string_of_atom = function
  | Int i -> string_of_int i
  | String s -> sprintf "'%s'" (String.escaped s)
  | Bool b -> string_of_bool b
  | Float x -> string_of_float x
  | Record t ->
      (* all records should have been expanded,
         that's the !atom-records flatten postcondition *)
      assert false

let rec string_of_query = function
  | Select view -> string_of_view view
  | Insert (table, view) ->
      sprintf "INSERT INTO %s (%s)"
        (string_of_table table)
        (string_of_view view)
  | Delete (table, row, where) ->
      sprintf "DELETE FROM %s AS %s%s"
        (string_of_table table) row
        (string_of_where where)
  | Update (table, row, set, where) ->
      sprintf "UPDATE %s AS %s SET %s%s"
        (string_of_table table) row
        (string_of_assoc set)
        (string_of_where where)

