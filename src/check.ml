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

let perform_check check_description query description =
  let dbh = PGOCaml.connect () in
  let check_result =
    try `Result (check_description (query dbh description))
    with exn -> `Exn exn in
  PGOCaml.close dbh;
  match check_result with
    | `Result res -> res
    | `Exn exn -> raise exn

let check_table_description table_name descr pgsql_descr =
  let correct = ref true in
  let check descr field =
    let field_name = field#!column_name in
    let field_type, descr =
      try Some (List.assoc field_name descr),
        List.remove_assoc field_name descr
      with Not_found -> None, descr in
    match field_type with
      | None ->
          eprintf "SQL Check Warning : In table %s, field %s undescribed\n"
            table_name field_name;
          descr
      | Some field_type ->
          let atom_type = atom_type_of_string field#!data_type in
          (match field_type with
             | Nullable _ when not field#!is_nullable ->
                 correct := false;
                 eprintf "SQL Check Error : In table %s, field %s \
                          is described as NULL but is NOT NULL\n"
                   table_name field_name
             | Non_nullable _ when field#!is_nullable ->
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
                   (string_of_atom_type t) field#!data_type
             | _ -> ());
          descr in
  let left_descr = List.fold_left check descr pgsql_descr in
  List.iter
    (fun (field_name, _) ->
       correct := false;
       eprintf
         "SQL Check Error : In table %s, field %s is decribed \
          but does not exists in the PGSQL database\n"
         table_name field_name)
    left_descr;
  if not !correct then
    failwith
      (sprintf
         "SQL Check : Coherence check of table %s \
          against the PGSQL database failed."
         table_name)
  else
    eprintf "SQL Check : Table %s description \
             is coherent with the PGSQL database.\n"
      table_name;
  flush stderr

(* we constrain on ('a, _ writable) Sql.view, because non_writable
   views may not be actual tables in the SQL base, whereas _ writable
   views always are *)
let check_table (table : ('a, _ writable) Sql.view) =
  (* we are forced to break the abstraction, as the user will send in
     Sql values, and we need an Inner_sql value to introspect *)
  let (table : Sql_internals.view) = Obj.magic table in
  let (schema, table_name) as name = match table.data with
    | Table t -> t.name
    | Selection _ | View_op _ -> invalid_arg "check_table" in
  let long_name = string_of_table_name name in
  let schema = match schema with
    | None -> "public"
    | Some schema -> schema in
  let pgsql_columns =
    <:table< information_schema.columns
             ( table_schema text NOT NULL,
               table_name text NOT NULL,
               column_name text NOT NULL,
               data_type text NOT NULL,
               is_nullable text NOT NULL ) >> in
  let table_descr =
    << { info.column_name;
         info.data_type;
         is_nullable = (info.is_nullable = "YES") } |
           info in $pgsql_columns$;
           info.table_schema = $string:schema$;
           info.table_name = $string:table_name$ >> in
  perform_check (check_table_description long_name table.descr)
    (Query.view ?log:None) table_descr

let check_sequence_description seq_name descr_type descr =
  let correct = ref true in
  begin
    match descr with
      | None ->
          correct := false;
          eprintf "SQL Check Error : Sequence %s is described \
                  but does not exists in the PGSQL database.\n" seq_name
      | Some descr ->
          let real_type = match descr#!numeric_precision with
            | 16l -> Some TInt16
            | 32l -> Some TInt32
            | 64l -> Some TInt64
            | p ->
                correct := false;
                eprintf "SQL Check Error : unsupported \
                        numeric precision : %ld.\n" p;
                None in
          match real_type with
            | None -> ()
            | Some real_type ->
                if real_type <> descr_type then begin
                  correct := false;
                  eprintf "SQL Check Error : Sequence %s type \
                          is described as %s but is %s.\n" seq_name
                    (string_of_atom_type descr_type)
                    (string_of_atom_type real_type)
                end
  end;
  if not !correct then
    failwith
      (sprintf
         "SQL Check : Coherence check of sequence %s \
          against the PGSQL database failed."
         seq_name)
  else
    eprintf "SQL Check : Sequence %s description \
             is coherent with the PGSQL database.\n"
      seq_name;
  flush stderr

let check_sequence (seq : 'a Sql.sequence) =
  (* see check_table Obj.magic comment *)
  let (seq : 'a Sql_public.sequence) = Obj.magic seq in
  let (name, typ) = seq in
  let pgsql_sequences =
    <:table< information_schema.sequences
             ( sequence_name text NOT NULL,
               numeric_precision integer NOT NULL ) >> in
  let sequence_description =
    << t | t in $pgsql_sequences$;
           t.sequence_name = $string:name$ >> in
  perform_check (check_sequence_description name typ)
    (Query.view_opt ?log:None) sequence_description
