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

open Sql_base

open Sql_internals

(* Flattening is intended to transform the query in a form current SQL
   servers can understand : our view representation is very expressive
   and some authorized syntaxes must be translated to a dumber
   equivalent. For example, we do not differentiate accessing from
   a row (t.a) or from a tuple ({a=1}.a). The second form does not lead
   to valid SQL, so we reduce it to (1).

   We try not to introduce any inefficiencies during the flattening
   process, but this make a non-trivial algorithm even harder to write
   correctly. The rule of the thumb is that, although macaque allows
   a great flexibility in the use of row values in any place a non-row
   value is authorized, the translation is somewhat flaky (and, sadly,
   sometimes buggy) : it can happen that a row-returning computation
   is duplicated for each field, or worse that the SQL rejects a query
   at runtime (this is a bug and should be reported).

   Of course, the usual workflow of innocent SQL users will not suffer
   from this, and may benefit as well of the additional freedom we
   have, in a sane, effective and moderate way. Do not, however, try
   to compute the fibonacci function using the naive recursive
   algorithm and a nested-rows set-theoretic-like representation of
   natural numbers.
*)

let rec flatten_view view =
  { view with data = flatten_concrete view.data }
and flatten_concrete = function
  | Table t -> Table t
  | Selection q -> Selection (flatten_selection q)
and flatten_selection q =
  { select = flatten_select q.select;
    from = flatten_from q.from;
    where = flatten_where q.where }
and flatten_from from =
  List.map (fun (id, view) -> (id, flatten_view view)) from
and flatten_where w = List.map flatten_value w
and flatten_select = function
  | Simple_select row -> Simple_select (flatten_value row)
  | Group_by (result, group) ->
      Group_by (flatten_value result, flatten_value group)
and flatten_value value =
  (*

    Preconditions to be met by the input structures :
    - input-record-type : no records other than
    Row, Tuple, Case, Bind and Atom (Record _)

    Postconditions met by the flattened structure :
    - atom-records : all atom records are gone
    - output-field-row : only field-row (Field ((Row _, _), _)) access
    (would provoque an SQL error; enforced by Sql_printers)
    - composite-types : no nested composite types
    (useful due to the incomple mapping of Macaque tuples
    to SQL anonymous records)
    - output-record-types : record types are all bindings, direct tuples,
    or rows inside a field access (no more direct row or case)
    (would provoque an SQL error)

    Termination :

    'flatten' is an intricate recursive algorithm with non-trivial
    recursive call decisions. This is necessary to ensure both
    corectness and termination of the transformation. Termination has
    been a bit tricky to establish, and some termination arguments are
    given in the comments.

    The simpler alternative, wich would be to code "flatten" as
    a one-step transform and use a fixpoint transformator, is made
    more difficult by the presence of function closures in the Sql.t
    values, wich disable the use of the usual equality operator.
    Besides, the one-step version was found not to improve readability
    and ease of the (informal) termination proof.
  *)
  let rec flatten = function
    (* atom-record *)
    | Atom (Record obj),
      ( Non_nullable (TRecord r)
        | Nullable (Some (TRecord r)) as t) ->
        (* atom records are concrete objects produced by the parsing of a row result;
           they don't have an AST attached and thus cannot be manipulated and printed;
           they have to be converted back to equivalent tuples, using their "producer" field *)
        flatten (Tuple (r.producer obj), t)

    (* field reduction termination *)
    | Field ((Row _, _), _ :: _), t as end_access
      when not (is_record_type t) -> end_access

    (* field reductions;
       termination : reduced value depth *)
    | Field (row, []), _ -> flatten row
    | Field ((Field (row, path), _), path'), t ->
        flatten (Field (row, path @ path'), t)
    | Field ((Tuple tup, tuple_t), field::path), _ ->
        flatten ( Field (List.assoc field tup, path),
                  get_sql_type tuple_t [field] )
    | Field ((Null, _), _), t -> Null, t

    (* field-case reduction;
       termination : constant value depth, but reduced
                     max{depth(node) | node inside a field access} *)
    | Field ((Case (cases, default), _), path), t ->
        let reduce v = Field (v, path), t in
        let reduce_case (cond, case) =  cond, reduce case in
        flatten (Case (List.map reduce_case cases, reduce default), t)

    | Field (_, _), t when not (is_record_type t)->
        (* a field-access must be on a record type; this is enforced by typing;
           all legal record types according to input-record-types precondition
           have been matched earlier : this is a precondition violation;
           the only remaining case, when the accessed value is a record type,
           is left for the output-record-types expansion case *)
        assert false

    (* composite-types reduction;
       termination : reduced value depth (on subcalls) *)
    | Tuple tup, t ->
        assert (is_record_type t);
        let field (name, value) = match flatten value with
          | Tuple tup, _ ->
              let child (child_name, child_val) =
                (name ^ path_separator ^ child_name, child_val) in
              List.map child tup
          | flat_val -> [(name, flat_val)] in
        Tuple (List.flatten (List.map field tup)), t

    (* output-record-types :
       a non-tuple record must be transformed into a tuple
       termination :
       only one such call can be made at each depth level as the next call
       (earlier tuple case) will have subcalls of reduced depth *)
    | value, record_t as row when is_record_type record_t ->
        let descr = (get_record_type record_t).descr in
        let field (name, child_t) =
          name, (Field (row, [name]), child_t) in
        flatten (Tuple (List.map field descr), record_t)
    | (Field _), _ ->
        (* all non-record-type fields were matched earlier;
           record-types were just matched *)
        assert false
    | (Row _), _ ->
        (* rows must be record types; matched earlier *)
        assert false


    (* propagating the transformation to non-fields, non-records subvalues;
       termination : subcalls on inferior value depth *)
    | Null, t -> Null, t
    | Atom v, t -> Atom v, t
    | Cast (v, cast_t), t -> Cast (flatten v, cast_t), t
    | Op (left, op, right), t ->
        Op (List.map flatten left, op, List.map flatten right), t
    | Case (cases, default), t ->
        let flatten_case (cond, case) = flatten cond, flatten case in
        Case (List.map flatten_case cases, flatten default), t
  in flatten value
and flatten_table (name, comp) = (name, flatten_concrete comp)


let flatten_query = function
  | Select view -> Select (flatten_view view)
  | Insert (table, view) -> Insert (table, flatten_view view)
  | Delete (table, row, where) -> Delete (table, row, flatten_where where)
  | Update (table, row, set, where) ->
      Update (table, row, flatten_value set, flatten_where where)
