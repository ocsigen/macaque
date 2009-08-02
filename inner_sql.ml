(* macaque : inner_sql.ml
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

type 'a view =
  { descr : types_descr;
    result_parser : 'a result_parser;
    concrete : concrete_view }
and concrete_view =
  | Table of table_name
  | Selection of select
and select = { select : select_result; from : from; where : where }
and select_result =
  | Simple_select of row
  | Group_by of row * row
and group_by = (row * row)
and from = (row_name * concrete_view) list
and where = reference list
and row = reference
and reference = reference' * field_type
and reference' =
  | Null
  | Value of value
  | Field of reference * field_name list
  | Binop of string * reference * reference
  | Unop of string * reference
  | Row of (row_name * untyped view)
  | Tuple of reference tuple
and value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Record of untyped * (untyped -> reference)
and table_name = string option * string
and row_name = string
and 'a tuple = (field_name * 'a) list
and field_name = string
and 'a descr = types_descr * 'a result_parser
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
  | TRecord of untyped descr * (untyped -> reference)
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



(** untyped parsers *)
let unsafe_parser input_parser : untyped result_parser =
  fun input -> Obj.repr (input_parser input)

let (&&&) ptr_action safe_parser (input, input_ptr) =
  let cur_ptr = !input_ptr in
  ptr_action input_ptr;
  let input_str = input.(cur_ptr) in
  try safe_parser input_str
  with exn -> failwith
    (Printf.sprintf "Parser error [%s] on input %d [%s]"
       (Printexc.to_string exn) cur_ptr input_str)

let use_unsafe_parser unsafe_parser input = Obj.obj (unsafe_parser input)

let pack value value_type = Value value, Non_nullable value_type

let stringref_of_string s =
  pack (String (PGOCaml.string_of_string s)) TString
let intref_of_string s =
  pack (Int (PGOCaml.int_of_string s)) TInt
let floatref_of_string s =
  pack (Float (PGOCaml.float_of_string s)) TFloat
let boolref_of_string s =
  pack (Bool (PGOCaml.bool_of_string s)) TBool

let bool_field_parser = unsafe_parser (incr &&& boolref_of_string)
let int_field_parser = unsafe_parser (incr &&& intref_of_string)
let float_field_parser = unsafe_parser (incr &&& floatref_of_string)
let string_field_parser = unsafe_parser (incr &&& stringref_of_string)
let error_field_parser= unsafe_parser (ignore &&& (fun _ -> failwith "Error parser"))

let option_field_parser (field_parser : untyped result_parser) : untyped result_parser =
  unsafe_parser
    (function (input_tab, input_ptr) as input ->
       if input_tab.(!input_ptr) = "NULL" then (incr input_ptr; (Null, Nullable None))
       else
         let r, t = use_unsafe_parser field_parser input in
         r, match t with
            | Non_nullable t -> Nullable (Some t)
            | _ -> invalid_arg "option_field_parser")

let null_field_parser = option_field_parser error_field_parser

let record_parser (descr, row_parser) ast_builder =
  unsafe_parser
    (fun input ->
       Value (Record (Obj.repr (row_parser input), ast_builder)),
       TRecord ((descr, row_parser), ast_builder))

let parser_of_type =
  let parser_of_sql_type = function
    | TInt -> int_field_parser
    | TFloat -> float_field_parser
    | TString -> string_field_parser
    | TBool -> bool_field_parser
    | TRecord (full_descr, ast_builder) ->
        record_parser full_descr ast_builder in
  function
  | Non_nullable typ -> parser_of_sql_type typ
  | Nullable None -> null_field_parser
  | Nullable (Some typ) -> option_field_parser (parser_of_sql_type typ)

let unsafe_view view =
  { view with result_parser = unsafe_parser view.result_parser }




(** SQL composite types flattening *)
let rec flatten_concrete = function
  | Table t -> Table t
  | Selection q -> Selection (flatten_selection q)
and flatten_selection q =
  { select = flatten_select q.select;
    from = List.map flatten_table q.from;
    where = flatten_where q.where }
and flatten_where w = List.map flatten_reference w
and flatten_select = function
  | Simple_select row -> Simple_select (flatten_reference row)
  | Group_by (result, group) ->
      Group_by (flatten_reference result, flatten_reference group)
and flatten_reference ref =
  let rec flatten = function
    | Null, t -> Null, t
    (* termination : those first recursive calls have inferior
       reference depth *)
    | Field (row, []), _ -> flatten row
    | Field ((Tuple tup, t), field::path), _ ->
        flatten (Field (List.assoc field tup, path),
                 get_field_type t [field])
    | Field ((Field (row, path), _), path'), t ->
        flatten (Field (row, path @ path'), t)
    | Tuple tup, t ->
        let field (name, ref) = match flatten ref with
          | Tuple tup, _ ->
              let child (child_name, child_ref) =
                (name ^ "__" ^ child_name, child_ref) in
              List.map child tup
          | flat_val -> [(name, flat_val)] in
        Tuple (List.flatten (List.map field tup)), t
    (* termination : this pattern case will never match more than once
       on the same row, because we change the row type to Null *)
    | row, (( Non_nullable (TRecord((descr, _), _))
            | Nullable (Some (TRecord((descr, _), _))))  as t) ->
        let field (name, child_t) =
          name, flatten (Field ((row, Nullable None), [name]), child_t) in
        Tuple (List.map field descr), t
    (* row whose type was set Null before *)
    | (Row _), _ as flattened_row -> flattened_row
    | Value v, flat_t -> Value v, flat_t
    (* termination : subcalls on inferior reference depth *)
    | Unop (op, a), t -> Unop (op, flatten a), t
    | Binop (op, a, b), t ->
        Binop (op, flatten a, flatten b), t
    | Field (row, path), t ->
        match flatten row with
          | (Tuple _ | Field _), _ as reductible ->
              flatten (Field (reductible, path), t)
          | final -> Field (final, path), t in
  flatten ref
and flatten_table (name, comp) = (name, flatten_concrete comp)


(** SQL Query printing *)
open Printf

let string_of_list printer sep li = String.concat sep (List.map printer li)

let rec string_of_concrete_view = function
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
  sprintf "%s AS %s" (string_of_concrete_view table) row_name
and string_of_table_name = function
  | (None, table) -> table
  | (Some schema, table) -> sprintf "%s.%s" schema table
and string_of_value = function
  | Int i -> string_of_int i
  | String s -> sprintf "'%s'" (String.escaped s)
  | Bool b -> string_of_bool b
  | Float x -> string_of_float x
  | Record (obj, ast_builder) -> string_of_reference (ast_builder obj)

let parser_of_comp comp input_tab =
  comp.result_parser (input_tab, ref 0)
