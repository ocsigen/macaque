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
    producer : untyped -> value tuple;
    record_parser : untyped record_parser;
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
  | Case of (value * value) list * value (* [when .. then ..]+ else ..*)
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
and 'a record_parser = descr -> 'a result_parser

let unsafe_producer producer : (untyped -> value tuple) =
  fun obj -> producer (Obj.obj obj)

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

let get_record_type = function
  | Non_nullable (TRecord t) | Nullable (Some (TRecord t)) -> t
  | _ -> raise Not_found

let is_record_type record =
  try ignore (get_record_type record); true
  with Not_found -> false

let rec unify t t' =
  let unify_atom a a' = match a, a' with
    (* identity unifications *)
    | TInt, TInt -> TInt
    | TBool, TBool -> TBool
    | TFloat, TFloat -> TFloat
    | TString, TString -> TString
    | TRecord r, TRecord r' ->
        let fields descr = List.sort compare (List.map fst descr) in
        let d, d' = r.descr, r'.descr in
        if fields d <> fields d' then failwith "unify";
        let unified_descr =
          let unify_item (id, t) (id', t') =
            assert (id = id'); (id, unify t t') in
          let assoc (id, t) d' = (id, List.assoc id d') in
          List.map (fun item -> unify_item item (assoc item d')) d in
        TRecord  { r with descr = unified_descr }

    (* numeric unifications *)
    | TInt, TFloat | TFloat, TInt -> TFloat

    (* failure *)
    | (TInt | TBool | TFloat | TString | TRecord _), _ ->
        failwith
          (Printf.sprintf "unify (%s and %s)"
             (string_of_atom_type a)
             (string_of_atom_type a')) in
  match t, t' with
    | Non_nullable a, Non_nullable a' ->
        (* none of them is nullable *)
        Non_nullable (unify_atom a a')
    | t, t' ->
        (* at least one of them is nullable

           Nullability unification :
           we choose to unify eg. (Nullable (Some t), Non_nullable t)
           instead of throwing an error because the type system
           actually doesn't guarantee that this runtime nullability
           information is accurate : litterate atoms are tagged
           Non_nullable but have a polymorphic nullability field for
           convenience reasons *)
        let atom = function
          | Non_nullable a | Nullable (Some a) -> Some a
          | Nullable None -> None in
        Nullable
          (match atom t, atom t' with
             | None, t | t, None -> t
             | Some a, Some a' -> Some (unify_atom a a'))

let is_unifiable t t' =
  try ignore (unify t t'); true
  with Failure "unify" -> false
