open Sql_internals
open Sql_types

(** values *)

let field row path checker =
  ignore checker;
  (Field (get_reference row, path),
   get_field_type (get_type row) path)

let row name view =
  let view = untyped_view view in
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
  let descr = List.map field_typ fields, Sql_parsers.unsafe_parser result_parser in
  (* rec : see "row" comment *)
  let rec reference =
    (Tuple (List.map field_ref fields),
     Non_nullable (TRecord (descr, fun _ -> reference))) in
  reference


(** views *)

let view (select, select_type) from where =
  let from = List.map (fun (name, view) -> (name, view.concrete)) from in
  let query = { select = select; from = from; where = where } in
  match select_type with
    | Non_nullable (TRecord ((descr, result_parser), _))
    | Nullable (Some (TRecord ((descr, result_parser), _))) ->
        { descr = descr;
          result_parser = Sql_parsers.use_unsafe_parser result_parser;
          concrete = Selection query }
    | _ -> assert false


(** results *)

let simple_select row = Simple_select (get_reference row), get_type row

let group group_part result_part =
  Group_by (result_part, group_part), get_type result_part


(** queries *)

let get_table_name = function
  | {concrete = Table name} -> name
  | _ -> invalid_arg "get_table_name"
let get_where = List.map get_reference

let select view = Select (untyped_view view)
let insert table inserted_view =
  Insert (get_table_name table, inserted_view.concrete)
let delete table row where =
  Delete (get_table_name table, row, get_where where)
let update table row set subtype_witness where =
  ignore subtype_witness;
  Update (get_table_name table, row, get_reference set, get_where where)
