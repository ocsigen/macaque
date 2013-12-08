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

let keyword_safe = Sql_keywords.keyword_safe

open Printf

let string_of_list printer sep li = String.concat sep (List.map printer li)

(* for now we roll our own string escaping function (String.escaped
   does not work as it escapes double-quotes). It would be better to
   use PostgreSQL string escaping routines directly, but as far as
   I know PG'OCaml do not export them. *)
let escape_string s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '\'' ->
      Buffer.add_char b '\''; Buffer.add_char b '\''
    | '\\' ->
      Buffer.add_char b '\\'; Buffer.add_char b '\\'
    | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let string_of_fields tuple =
  string_of_list (fun (field, _) -> keyword_safe field) "," tuple

let rec string_of_view view = string_of_concrete view.data
and string_of_concrete = function
  | Selection q -> sprintf "(%s)" (string_of_selection q)
  | View_op (v1, op, v2) -> sprintf "(%s %s %s)"
    (string_of_concrete v1) op (string_of_concrete v2)
  | Table table_data -> string_of_table_name table_data.name
and string_of_selection q =
  let result = match q.select with
    | Simple_select result
    | Group_by (result, _) -> result in
  let group_by = match q.select with
    | Group_by (result, (Tuple (_::_ as const), _)) ->
        " GROUP BY " ^
          string_of_list (fun (_, r) -> string_of_value r) ", " const
    | _ -> "" in
  "SELECT "
  ^ (string_of_row result)
  ^ (string_of_from q.from)
  ^ (string_of_where q.where)
  ^ group_by
  ^ (string_of_order_by q.order_by)
  ^ (string_of_limit q.limit)
  ^ (string_of_offset q.offset)
and string_of_from = function
  | [] -> ""
  | from -> " FROM " ^ string_of_list string_of_from_item ", " from
and string_of_using = function
  | [] -> ""
  | using -> " USING " ^ string_of_list string_of_from_item ", " using
and string_of_where = function
  | [] -> ""
  | where -> " WHERE " ^ string_of_list string_of_value " AND " where
and string_of_order_by = function
  | None -> ""
  | Some ordering ->
      let string_of_order (value, order) =
        sprintf "%s %s" (string_of_value value)
          (match order with Asc -> "ASC" | Desc -> "DESC") in
      " ORDER BY " ^ string_of_list string_of_order ", " ordering
and string_of_limit = function
  | None -> ""
  | Some v -> " LIMIT " ^ string_of_value v
and string_of_offset = function
  | None -> ""
  | Some v -> " OFFSET " ^ string_of_value v
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
    | Field ((Null, _), _) ->
        (* NULL.foo is considered equivalent to NULL *)
        "NULL"
    | Row (row_name, _) -> keyword_safe row_name
    | Cast (v, t) ->
        sprintf "CAST(%s AS %s)" (string_of_value v) (string_of_atom_type t)
    | Field ((Row (row_name, _), _), fields) ->
        sprintf "%s.%s" (keyword_safe row_name)
          (String.concat path_separator (List.map keyword_safe fields))
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
    | OpTuple (_, _, [], Some default) -> string_of_value default
    | OpTuple (_, op, [], None) ->
        failwith
          (Printf.sprintf
             "The operator '%s' needs a non-empty right parameter"
             op
          )
    | OpTuple (left, op, right, _) ->
        sprintf "(%s %s (%s))"
          (string_of_value left)
          op
          (string_of_list string_of_value ", " right)
    | Case ([], default) -> string_of_value default
    | Case (cases, default) ->
        let string_of_case (cond, case) =
          sprintf "WHEN %s THEN %s"
            (string_of_value cond) (string_of_value case) in
        sprintf "(CASE %s ELSE %s END)"
          (string_of_list string_of_case " " cases)
          (string_of_value default)
and string_of_from_item (row_name, table) =
  sprintf "%s AS %s" (string_of_view table) (keyword_safe row_name)
and string_of_table (table : table) = string_of_table_name table.data.name
and string_of_table_name = function
  | (None, table) -> keyword_safe table
  | (Some schema, table) -> sprintf "%s.%s" (keyword_safe schema) (keyword_safe table)
and string_of_atom =
  let quote printer value = sprintf "E'%s'" (printer value) in
  function
    | Unit u -> PGOCaml.string_of_unit u
    | Bool b -> macaque_string_of_bool b
    | Int16 i -> PGOCaml.string_of_int16 i
    | Int32 i -> PGOCaml.string_of_int32 i
    | Int64 i -> PGOCaml.string_of_int64 i
    | Float x -> macaque_string_of_float x
    | String s -> quote escape_string s
    | Bytea i -> macaque_string_of_bytea i
    | Time i -> quote PGOCaml.string_of_time i
    | Date i -> quote PGOCaml.string_of_date i
    | Timestamp i -> quote PGOCaml.string_of_timestamp i
    | Timestamptz i -> quote PGOCaml.string_of_timestamptz i
    | Interval i -> quote PGOCaml.string_of_interval i
    | Bool_array js -> quote PGOCaml.string_of_bool_array js
    | Int32_array js -> quote PGOCaml.string_of_int32_array js
    | Int64_array js -> quote PGOCaml.string_of_int64_array js
    | Float_array js -> quote PGOCaml.string_of_float_array js
    | String_array js -> quote PGOCaml.string_of_string_array js
    | Record t ->
        (* all records should have been expanded,
           that's the !atom-records flatten postcondition *)
        assert false
and macaque_string_of_bool b =
  if b then "TRUE" else "FALSE"
and macaque_string_of_float x =
  let litteral str = sprintf "CAST('%s' as %s)" str (string_of_atom_type TFloat) in
  match classify_float x with
  | FP_normal | FP_subnormal | FP_zero -> string_of_float x
  | FP_nan -> litteral "NaN"
  | FP_infinite -> litteral (if x = infinity then "Infinity" else "-Infinity")
and macaque_string_of_bytea octets =
  let buf = Buffer.create (String.length octets * 2 + 8) in
  Buffer.add_string buf "E'\\\\x";
  String.iter (fun ch -> Printf.bprintf buf "%02x" (Char.code ch)) octets;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let rec string_of_query = function
  | Select view -> string_of_view view
  | Value value ->
      sprintf "SELECT (%s)" (string_of_value value)
  | Insert (table, view) ->
      sprintf "INSERT INTO %s (%s) (%s)"
        (string_of_table table)
        (string_of_fields view.descr)
        (string_of_view view)
  | Delete (table, row, from, where) ->
      sprintf "DELETE FROM %s AS %s%s%s"
        (string_of_table table) row
        (string_of_using from)
        (string_of_where where)
  | Update (table, row, set, from, where) ->
      sprintf "UPDATE %s AS %s SET %s%s%s"
        (string_of_table table) row
        (string_of_assoc set)
        (string_of_from from)
        (string_of_where where)
