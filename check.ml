(* macaque : check.ml
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

open Sql
open Sql_internals
open Sql_printers

open Printf

let check_description table_name descr pgsql_descr =
  let check (correct, descr) field =
    let field_name = get field#name in
    let field_type, descr =
      try Some (List.assoc field_name descr),
        List.remove_assoc field_name descr
      with Not_found -> None, descr in
    match field_type with
      | None ->
          eprintf "SQL Check Warning : In table %s, field %s undescribed\n"
            table_name field_name;
          (correct, descr)
      | Some field_type ->
          let correct = ref correct in
          let atom_type =
            atom_type_of_string (get field#data_type) in
          let nullable =
            match get field#nullable with
              | "YES" -> true
              | "NO" -> false
              | other ->
                  correct := false;
                  eprintf "SQL Check Error : In table %s, field %s \
                           has unknown 'is_nullable' value : '%s'\n"
                    table_name field_name other;
                  true in
          (match field_type with
             | Nullable _ when not nullable ->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          is described as NULL but is NOT NULL\n"
                   table_name field_name
             | Non_nullable _ when nullable ->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          is described as NOT NULL but is NULL\n"
                   table_name field_name
             | _ -> ());
          (match field_type with
             | Nullable (Some t) | Non_nullable t
               when t <> atom_type->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          has incompatible types :\n\
                          \t%s in description, %s in table\n"
                   table_name field_name
                   (string_of_atom_type t) (get field#data_type)
             | _ -> ());
          !correct, descr in
  let correct, left_descr = List.fold_left check (true, descr) pgsql_descr in
  let correct = correct && left_descr = [] in
  List.iter
    (fun (field_name, _) ->
       eprintf
         "SQL Check Error : In table %s, field %s \
          is decribed but does not exists\n"
         table_name field_name)
    left_descr;
  if not correct then
    failwith
      (Printf.sprintf
         "SQL Check : Coherence check of table %s \
          against the PGSQL database failed."
         table_name)
  else
    eprintf "SQL Check : Table %s description \
             is coherent with the PGSQL database.\n"
      table_name;
  flush stderr

let check_table (table : 'a Sql.table) =
  (* we are forced to break the abstraction, as the user will send in
     Sql values, and we need an Inner_sql value to introspect it *)
  let (table : 'a Sql_types.table) = Obj.magic table in
  let (schema, table_name) as name = table.concrete in
  let long_name = string_of_table_name name in
  let schema = match schema with
    | None -> "public"
    | Some schema -> schema in
  let pgsql_columns =
    <:table< information_schema.columns (
               table_schema text NOT NULL,
               table_name text NOT NULL,
               column_name text NOT NULL,
               data_type text NOT NULL,
               is_nullable text NOT NULL
    ) >> in
  let check_table = <:select<
    { name = info.column_name;
      data_type = info.data_type;
      nullable = info.is_nullable } |
        info in $table:pgsql_columns$;
    info.table_schema = $string:schema$;
    info.table_name = $string:table_name$ >> in
  let dbh = PGOCaml.connect () in
  let check_result =
    try `Result (check_description long_name table.descr
                   (Query.Simple.query dbh check_table))
    with exn -> `Exn exn in
  PGOCaml.close dbh;
  match check_result with
    | `Result res -> res
    | `Exn exn -> raise exn
