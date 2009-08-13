open Sql_internals
open Sql_types

(** operations *)

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



(** values *)

let field row path checker =
  ignore checker;
  (Field (get_reference row, path),
   get_field_type (get_type row) path)

let row name view =
  let descr = (view.descr, view.result_parser) in
  (* we need a recursive value, as the ast_builder has to return the
     value itself *)
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

let select view = Select view
let insert table inserted_view =
  Insert (get_table_name table, inserted_view)
let delete table row where =
  Delete (get_table_name table, row, get_where where)
let update table row set subtype_witness where =
  ignore subtype_witness;
  Update (get_table_name table, row, get_reference set, get_where where)
