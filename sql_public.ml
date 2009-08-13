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

module Data = struct
  let bool b = Value (Bool b), Non_nullable TBool
  let int i = Value (Int i), Non_nullable TInt
  let float x = Value (Float x), Non_nullable TFloat
  let string s = Value (String s), Non_nullable TString
end

module Op = struct
  open Sql_types
  open Sql_builders

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

  let same_op = op (fun t -> t)
  let mono_op t = op (fun t' -> assert (t = t'); t)
  let poly_op return_t = op (fun _ -> return_t)

  type 'phant arith_op = 'phant binary_op
  constraint 'phant = < input_t : #numeric_t as 't; output_t : 't; .. >

  let arith op = same_op op

  let (+), (-), (/), ( * ) =
    arith "+", arith "-", arith "/", arith "*"

  type 'phant comp_op = 'phant binary_op
  constraint 'phant = < output_t : bool_t; .. >

  let comp op = poly_op TBool op

  let (<), (<=), (<>), (=), (>=), (>) =
    comp "<", comp "<=", comp "<>", comp "=", comp ">=", comp ">"
  let is_distinct_from a b = Binop ("IS DISTINCT FROM", a, b), Non_nullable TBool
  let is_not_distinct_from a b = Binop ("IS NOT DISTINCT FROM", a, b), Non_nullable TBool

  type 'phant logic_op = 'phant binary_op
  constraint 'phant = < input_t : #bool_t as 't; output_t : 't; .. >

  let logic op = mono_op TBool op

  let (&&), (||) = logic "AND", logic "OR"

  let not (ref, typ) = Unop ("NOT", (ref, typ)), typ

  let count x = Unop ("count", x), Non_nullable TInt
  let max (v, t) = Unop ("max", (v, t)), t
  let sum (v, t) = Unop ("sum", (v, t)), t
end

module Table_type = struct
  let _type t = function
    | true -> Nullable (Some t)
    | false -> Non_nullable t
  let integer = _type TInt
  let boolean = _type TBool
  let text = _type TString
end

module View = struct
  open Sql_tables
  open Sql_builders

  let table = table_view

  let one t = view (simple_select t) [] []
end
