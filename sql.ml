(* macaque : sql.ml
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

type true_t
type false_t

open Inner_sql

type 'a t = reference

type 'a result_parser = 'a Inner_sql.result_parser
type untyped = Inner_sql.untyped

let untyped_view = Inner_sql.unsafe_view

type +'a view = 'a Inner_sql.view
and from = (string * untyped view) list
and where = untyped t list

let get_reference r = r
let get_type (_, t) = t

let get_val =
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

let parse ref =
  use_unsafe_parser (parser_of_type (get_type ref))

type 'a unsafe = 'a
let unsafe x = x
let untyped x = x
let force_gettable x = x

let field row path checker =
  ignore checker;
  (Field (get_reference row, path),
   get_field_type (get_type row) path)

let row name view =
  let view = unsafe_view view in
  let descr = (view.descr, view.result_parser) in
  (* we need a recursive value, as the vast_builder has to return
     the value itself *)
  let rec reference =
    (Row (name, view),
     Non_nullable (TRecord (descr, fun _ -> reference))) in
  reference

let tuple fields result_parser =
  let field_ref (name, field) = (name, get_reference field) in
  let field_typ (name, field) = (name, get_type field) in
  let descr = List.map field_typ fields, unsafe_parser result_parser in
  (* rec : see "row" comment *)
  let rec reference =
    (Tuple (List.map field_ref fields),
     Non_nullable (TRecord (descr, fun _ -> reference))) in
  reference

module Data = struct
  let bool b = Value (Bool b), Non_nullable TBool
  let int i = Value (Int i), Non_nullable TInt
  let float x = Value (Float x), Non_nullable TFloat
  let string s = Value (String s), Non_nullable TString
end

module Op = struct
  let nullable (r, t) =
    r, match t with
       | Non_nullable t -> Nullable (Some t)
       | Nullable t -> Nullable t

  let null = Null, Nullable None
  let is_null ref = Prefixop("IS NULL", ref), Non_nullable TBool
  let is_not_null ref = Prefixop("IS NOT NULL", ref), Non_nullable TBool

  let option constr = function
    | None -> null
    | Some x -> nullable (constr x)

  let op type_fun op a b =
    match get_type a, get_type b with
      | Non_nullable t, Non_nullable t' ->
          (* none of them is nullable *)
          assert (t = t');
          Binop(op, a, b), Non_nullable (type_fun t)
      | t, t' ->
          (* at least one of them is nullable *)
          let some_t = function
            | Non_nullable t | Nullable (Some t) -> Some t
            | Nullable None -> None in
          let op, t = match some_t t, some_t t' with
            | Some t, Some t' ->
                assert (t = t');
                Binop(op, a, b), Some (type_fun t)
            | Some t, None | None, Some t ->
                Binop(op, a, b), Some (type_fun t)
            | None, None -> Null, None in
          op, Nullable t

  let same_op = op (fun t -> t)
  let mono_op t = op (fun t' -> assert (t = t'); t)
  let poly_op return_t = op (fun _ -> return_t)

  type 'phant arith_op = 'a t -> 'b t -> 'c t
  constraint 'a = < numeric : true_t; t : 't; nullable : 'n; .. >
  constraint 'b = < numeric : true_t; t : 't; nullable : 'n; .. >
  constraint 'c = < numeric : true_t; t : 't; nullable : 'n; gettable : false_t >
  constraint 'phant = < t : 't; nullable : 'n; a : 'a; b : 'b >
  let arith op = same_op op

  let (+), (-), (/), ( * ) =
    arith "+", arith "-", arith "/", arith "*"

  type 'phant comp_op = 'a t -> 'b t -> 'c t
  constraint 'a = < nullable : 'nul; t : 't; numeric : 'num; .. >
  constraint 'b = < nullable : 'nul; t : 't; numeric : 'num; .. >
  constraint 'c = < nullable : 'nul; t : bool; numeric : false_t; gettable : false_t >
  constraint 'phant = < nullable : 'nul; t : 't; numeric : 'num; a : 'a; b : 'b >
  let comp op = poly_op TBool op

  let (<), (<=), (<>), (=), (>=), (>) =
    comp "<", comp "<=", comp "<>", comp "=", comp ">=", comp ">"
  let is_distinct_from a b = Binop ("IS DISTINCT FROM", a, b), Non_nullable TBool
  let is_not_distinct_from a b = Binop ("IS NOT DISTINCT FROM", a, b), Non_nullable TBool

  type 'phant logic_op = 'a t -> 'b t -> 'c t
  constraint 'a = < t : bool; nullable : 'n; .. >
  constraint 'b = < t : bool; nullable : 'n; .. >
  constraint 'c = < t : bool; nullable : 'n; numeric : false_t; gettable : false_t >
  constraint 'phant = < nullable : 'n; a : 'a; b : 'b >
  let logic op = mono_op TBool op

  let (&&), (||) = logic "AND", logic "OR"

  let not (ref, typ) = Unop ("NOT", (ref, typ)), typ

  let count x = Unop ("count", x), Non_nullable TInt
  let max (v, t) = Unop ("max", (v, t)), t
  let sum (v, t) = Unop ("sum", (v, t)), t
end

type 'a result = select_result * field_type

let view (select, select_type) from where =
  let from = List.map (fun (name, view) -> (name, view.concrete)) from in
  let query = { select = select; from = from; where = where } in
  match select_type with
    | Non_nullable (TRecord ((descr, result_parser), _))
    | Nullable (Some (TRecord ((descr, result_parser), _))) ->
        { descr = descr;
          result_parser = use_unsafe_parser result_parser;
          concrete = Selection query }
    | _ -> assert false

let simple_select row = Simple_select (get_reference row), get_type row

type grouped_row = unit
let grouped_row = ()

type 'a accum = 'a t
type 'a group = 'a t

let accum x = x
let group_of_accum x = x

let group group_part result_part =
  Group_by (result_part, group_part), get_type result_part

let get_table_name = function
  | {concrete = Table name} -> name
  | _ -> invalid_arg "get_table_name"
let get_where = List.map get_reference


(** Query code *)
type 'a query =
  | Select of untyped view
  | Insert of (table_name * concrete_view)
  | Delete of (table_name * row_name * where)
  | Update of (table_name * row_name * reference * where)

let flatten_query = function
  | Select view -> Select {view with concrete = flatten_concrete view.concrete}
  | Insert (table, concrete) -> Insert (table, flatten_concrete concrete)
  | Delete (table, row, where) -> Delete (table, row, flatten_where where)
  | Update (table, row, set, where) ->
      Update (table, row, flatten_reference set, flatten_where where)

open Printf

let rec string_of_query = function
  | Select view -> string_of_concrete_view view.concrete
  | Insert (table, view) ->
      sprintf "INSERT INTO %s (%s)"
        (string_of_table_name table)
        (string_of_concrete_view view)
  | Delete (table, row, where) ->
      sprintf "DELETE FROM %s AS %s%s"
        (string_of_table_name table) row
        (string_of_where where)
  | Update (table, row, set, where) ->
      sprintf "UPDATE %s AS %s SET %s%s"
        (string_of_table_name table) row
        (string_of_assoc set)
        (string_of_where where)

let sql_of_query q = string_of_query (flatten_query q)
let sql_of_view v = sql_of_query (Select (unsafe_view v))

let select view = Select (unsafe_view view)
let insert table inserted_view =
  Insert (get_table_name table, inserted_view.concrete)
let delete table row where =
  Delete (get_table_name table, row, get_where where)
let update table row set subtype_witness where =
  ignore subtype_witness;
  Update (get_table_name table, row, get_reference set, get_where where)

let handle_query_results sql_query result =
  match sql_query with
    | Select comp ->
        Obj.magic (List.map (parser_of_comp comp) result)
    | _ -> Obj.magic ()



