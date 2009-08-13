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

  type 'phant binary_op = 'a t -> 'b t -> 'c t
  constraint 'a = < t : 'input_t; nul : 'n; .. >
  constraint 'b = < t : 'input_t; nul : 'n; .. >
  constraint 'c = < t : 'output_t; nul : 'n >
  constraint 'phant =
    < input_t : 'input_t; output_t : 'output_t; nul : 'n; a : 'a; b : 'b >

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
