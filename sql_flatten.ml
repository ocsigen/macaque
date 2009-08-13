(* macaque : sql_flatten.ml
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

let rec flatten_view view =
  { view with concrete = flatten_concrete view.concrete }
and flatten_concrete = function
  | Table t -> Table t
  | Selection q -> Selection (flatten_selection q)
and flatten_selection q =
  { select = flatten_select q.select;
    from = flatten_from q.from;
    where = flatten_where q.where }
and flatten_from from =
  List.map (fun (id, view) -> (id, flatten_view view)) from
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
    | Prefixop (op, a), t -> Prefixop (op, flatten a), t
    | Binop (op, a, b), t ->
        Binop (op, flatten a, flatten b), t
    | Field (row, path), t ->
        match flatten row with
          | (Tuple _ | Field _), _ as reductible ->
              flatten (Field (reductible, path), t)
          | final -> Field (final, path), t in
  flatten ref
and flatten_table (name, comp) = (name, flatten_concrete comp)


let flatten_query = function
  | Select view -> Select (flatten_view view)
  | Insert (table, view) -> Insert (table, flatten_view view)
  | Delete (table, row, where) -> Delete (table, row, flatten_where where)
  | Update (table, row, set, where) ->
      Update (table, row, flatten_reference set, flatten_where where)



