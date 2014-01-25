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
and table = table_data generic_view
and table_data =
  { name : table_name;
    defaults : value tuple }
and concrete_view =
  | Table of table_data
  | Selection of select
  | View_op of concrete_view * string * concrete_view
and select =
  { select : select_result;
    from : from;
    where : where;
    order_by : (value * order) list option;
    limit : value option;
    offset : value option }
and order = Asc | Desc
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
  | Cast of value * atom_type
  | Op of value list * string * value list
  | OpTuple of value * string * value list * value option
  | Row of (row_name * view)
  | Tuple of value tuple
  | Case of (value * value) list * value (* [when .. then ..]+ else ..*)
and atom =
  | Unit of unit
  | Bool of bool
  | Int16 of int16
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | String of string
  | Bytea of bytea
  | Time of time
  | Date of date
  | Timestamp of timestamp
  | Timestamptz of timestamptz
  | Interval of interval
  | Bool_array of bool option list
  | Int32_array of int32 option list
  | Int64_array of int64 option list
  | Float_array of float option list
  | String_array of string option list
  | Record of untyped (* runtime object instance *)
and table_name = string option * string
and row_name = string
and field_name = string
and descr = sql_type tuple
and sql_type =
  | Non_nullable of atom_type
  | Nullable of atom_type option
and atom_type =
  | TUnit
  | TBool
  | TInt16
  | TInt32
  | TInt64
  | TFloat
  | TString
  | TBytea
  | TTime
  | TDate
  | TTimestamp
  | TTimestamptz
  | TInterval
  | TArray of atom_type
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
  | "unit" -> TUnit
  | "boolean" -> TBool
  | "smallint" -> TInt16
  | "integer" -> TInt32
  | "bigint" -> TInt64
  | "double precision" -> TFloat
  | "text" -> TString
  | "bytea" -> TBytea
  | "time" -> TTime
  | "date" -> TDate
  | "timestamp" -> TTimestamp
  | "timestamptz" -> TTimestamptz
  | "interval" -> TInterval
  | "bool_array" -> TArray TBool
  | "int32_array" -> TArray TInt32
  | "int64_array" -> TArray TInt64
  | "float_array" -> TArray TFloat
  | "string_array" -> TArray TString
  | other -> failwith ("unknown sql type " ^ other)
let string_of_atom_type = function
  | TUnit -> "unit"
  | TBool -> "boolean"
  | TInt16 -> "smallint"
  | TInt32 -> "integer"
  | TInt64 -> "bigint"
  | TFloat -> "double precision"
  | TString -> "text"
  | TBytea -> "bytea"
  | TTime -> "time"
  | TDate -> "date"
  | TTimestamp -> "timestamp"
  | TTimestamptz -> "timestamptz"
  | TInterval -> "interval"
  | TArray TBool -> "boolean[]"
  | TArray TInt32 -> "integer[]"
  | TArray TInt64 -> "bigint[]"
  | TArray TFloat -> "double precision[]"
  | TArray TString -> "text[]"
  | TArray _ -> assert false
  | TRecord _ -> "record"

type query =
  | Select of view
  | Insert of (table * view)
  | Delete of (table * row_name * from * where)
  | Update of (table * row_name * value * from * where)
  | Value of value
  (* the 'Value' case is for selection of only one value, no FROM part;
     this special case can get typed more finely as
     - the user won't have to get a list of result, only a single
       result, in a type-safe way (rather than Query.view_one which
       does a dynamic check)
     - we won't request the value to be a record type (useful in
       particular to get the value of a counter), unlike View.one
  *)

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
    | ( TUnit | TBool | TInt16 | TInt32 | TInt64 | TFloat
      | TString | TBytea | TTime | TDate | TInterval | TArray _
      | TTimestamp | TTimestamptz) as t, t' when t = t' -> t
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

    (* failure *)
    | ( TUnit | TBool | TInt16 | TInt32 | TInt64 | TFloat
      | TString | TBytea | TTime | TDate | TInterval | TArray _
      | TTimestamp | TTimestamptz | TRecord _), _ ->
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
  with Failure _ -> false

let unify_descr d1 d2 =
  List.map (fun (n,t1) ->
    let t2 = List.assoc n d2 in
    (n, unify t1 t2)) d1

let is_null_type = (=) (Nullable None)

let null = Null, Nullable None
