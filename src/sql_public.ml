(* macaque : sql_public.ml
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
  open Sql_types

let sql_of_query q =
  Sql_printers.string_of_query (Sql_flatten.flatten_query q)
let sql_of_view v = sql_of_query (Select v)

let parse ty =
  Sql_parsers.use_unsafe_parser
    (Sql_parsers.parser_of_type ty)

module Value = struct
  let unit () = Atom (Unit ()), Non_nullable TUnit
  let bool b = Atom (Bool b), Non_nullable TBool
  let int16 i = Atom (Int16 i), Non_nullable TInt16
  let int32 i = Atom (Int32 i), Non_nullable TInt32
  let int64 i = Atom (Int64 i), Non_nullable TInt64
  let float x = Atom (Float x), Non_nullable TFloat
  let string s = Atom (String s), Non_nullable TString
  let bytea i = Atom (Bytea i), Non_nullable TBytea
  let time i = Atom (Time i), Non_nullable TTime
  let date i = Atom (Date i), Non_nullable TDate
  let timestamp i = Atom (Timestamp i), Non_nullable TTimestamp
  let timestamptz i = Atom (Timestamptz i), Non_nullable TTimestamptz
  let interval i = Atom (Interval i), Non_nullable TInterval
  let bool_array js = Atom (Bool_array js), Non_nullable (TArray TBool)
  let int32_array js = Atom (Int32_array js), Non_nullable (TArray TInt32)
  let int64_array js = Atom (Int64_array js), Non_nullable (TArray TInt64)
  let float_array js = Atom (Float_array js), Non_nullable (TArray TFloat)
  let string_array js = Atom (String_array js), Non_nullable (TArray TString)
end

type 'a sequence = string * atom_type
module Sequence = struct
  let serial seq_name = seq_name, TInt32
  let bigserial seq_name = seq_name, TInt64
  let sequence = bigserial
end

module Op = struct
  open Sql_builders

  let nullable (r, t) =
    r, match t with
       | Non_nullable t -> Nullable (Some t)
       | Nullable t -> Nullable t

  let null = null
  let postfixop value op = Op ([null_workaround value], op, [])
  let is_null value = postfixop value "IS NULL", Non_nullable TBool
  let is_not_null value = postfixop value "IS NOT NULL", Non_nullable TBool

  let of_option = function
    | None -> null
    | Some v -> nullable v

  let same_op op_str = op (fun t -> t) op_str
  let mono_op t op_str = op (unify (Non_nullable t)) op_str
  let poly_op return_t op_str =
    let type_fun = function
      | Non_nullable _ -> Non_nullable return_t
      | Nullable _ -> Nullable (Some return_t) in
    op type_fun op_str

  type 'phant arith_op = 'phant binary_op
  constraint 'phant = < in_t : #numeric_t as 't; out_t : 't; .. >

  let arith op = same_op op

  let (+), (-), (/), ( * ) =
    arith "+", arith "-", arith "/", arith "*"

  type 'phant comp_op = 'phant binary_op
  constraint 'phant = < out_t : bool_t; .. >

  let comp op = poly_op TBool op

  let (<), (<=), (<>), (=), (>=), (>) =
    comp "<", comp "<=", comp "<>", comp "=", comp ">=", comp ">"
  let is_distinct_from a b =
    fixed_op "IS DISTINCT FROM" a b (Non_nullable TBool)
  let is_not_distinct_from a b =
    fixed_op "IS NOT DISTINCT FROM" a b (Non_nullable TBool)
  let in' ((_, t) as v) l =
    let change_ty = function
      | Non_nullable _ -> Non_nullable TBool
      | Nullable None -> Nullable None
      | Nullable (Some _) -> Nullable (Some TBool)
    in
    let v = null_workaround v in
    let l = List.map null_workaround l in
    let t = List.fold_left (fun acc (_, x) -> unify acc x) t l in
    let default = (Atom (Bool false), Non_nullable TBool) in
    OpTuple (v, "IN", l, Some default), change_ty t

  type 'phant logic_op = 'phant binary_op
  constraint 'phant = < in_t : #bool_t as 't; out_t : 't; .. >

  let logic op = mono_op TBool op

  let (&&), (||) = logic "AND", logic "OR"

  let prefixop op v = Op ([], op, [null_workaround v])
  let not (value, typ) = prefixop "NOT" (value, typ), typ

  let count x = prefixop "count" x, Non_nullable TInt64
  let min (v, t) = nullable (prefixop "min" (v, t), t)
  let max (v, t) = nullable (prefixop "max" (v, t), t)
  let sum (v, t) = nullable (prefixop "sum" (v, t), t)
  let md5 (v, t) = prefixop "md5" (v, t), t
  let array_agg (v, t) =
    let to_array = function
      | Nullable None -> Nullable None
      | Nullable (Some t)
      | Non_nullable t -> Nullable (Some (TArray t))
    in
    prefixop "array_agg" (v, t), to_array t

  let label seq_name = Atom (String seq_name), Non_nullable TString
  let nextval (seq_name, typ) =
    prefixop "nextval" (label seq_name), Non_nullable typ
  let currval (seq_name, typ) =
    prefixop "currval" (label seq_name), Non_nullable typ

  let current_timestamp u =
    check_atom_type (get_type u) TUnit;
    Op ([], "current_timestamp", []), Non_nullable TTimestamptz

  let localtimestamp u =
    check_atom_type (get_type u) TUnit;
    Op ([], "localtimestamp", []), Non_nullable TTimestamp
end

module Table_type = struct
  let _type t = function
    | true -> Nullable (Some t)
    | false -> Non_nullable t
  let boolean = _type TBool
  let smallint = _type TInt16
  let integer = _type TInt32
  let bigint = _type TInt64
  let double = _type TFloat
  let text = _type TString
  let bytea = _type TBytea
  let time = _type TTime
  let date = _type TDate
  let timestamp = _type TTimestamp
  let timestamptz = _type TTimestamptz
  let interval = _type TInterval
  let bool_array = _type (TArray TBool)
  let int32_array = _type (TArray TInt32)
  let int64_array = _type (TArray TInt64)
  let float_array = _type (TArray TFloat)
  let string_array = _type (TArray TString)
end

module View = struct
  open Sql_builders
  let one t = view (simple_select t) [] []
end

module ViewOp = struct
  let binop op v1 v2 =
    {v1 with
      descr = unify_descr v1.descr v2.descr;
      data = View_op(v1.data, op, v2.data);
    }

  let union = binop "UNION"
  let union_all = binop "UNION ALL"
  let intersect = binop "INTERSECT"
  let intersect_all = binop "INTERSECT ALL"
  let except = binop "EXCEPT"
  let except_all = binop "EXCEPT ALL"
end

type 'a nullable_data = < get : unit; t : 'a; nul : nullable > t
type 'a non_nullable_data = < get : unit; t : 'a; nul : non_nullable > t
