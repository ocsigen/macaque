open Sql_internals
open Sql_types

(** operations *)

let op type_fun op a b =
 Op ([a], op, [b]),
  match get_type a, get_type b with
    | Non_nullable t, Non_nullable t' ->
        (* none of them is nullable *)
        assert (t = t');
        Non_nullable (type_fun t)
    | t, t' ->
        (* at least one of them is nullable *)
        let some_t = function
          | Non_nullable t | Nullable (Some t) -> Some t
          | Nullable None -> None in
        Nullable
          (match some_t t, some_t t' with
             | Some t, Some t' ->
                 assert (t = t');
                 Some (type_fun t)
             | Some t, None | None, Some t -> Some (type_fun t)
             | None, None -> None)

(** values *)

let field row path checker =
  ignore checker;
  (Field (row, path),
   get_sql_type (get_type row) path)

let row name view =
  ( Row (name, view),
    Non_nullable (TRecord {view with data = ()}) )

let tuple fields result_parser =
  let record_t =
    let field_typ (name, field) = (name, get_type field) in
    { data = ();
      result_parser = Sql_parsers.unsafe_parser result_parser;
      descr = List.map field_typ fields } in
  Tuple fields, Non_nullable (TRecord record_t)

let if_then_else p a b = Case ([(p, a)], b), get_type a

let match_null matched null_case other_case =
  match get_type matched with
    | Nullable None -> null_case
        (* this special case is a work-around to the match_null
           problem reported in tests/match_null.ml
           
           it makes match_null semantic much more fragile : we must
           NOT have a non-null value with a 'Nullable None' type, wich
           is only weakly enforced in the rest of the code due to
           complex SQL nullability behaviors *)
    | _ ->
        let cond = Op ([matched], "IS NULL", []), Non_nullable TBool in
        let else_val = other_case matched in
        Case ([(cond, null_case)], else_val), get_type null_case

(** tables *)

let table descr custom_result_parser name =
  { descr = descr;
    result_parser =
      Sql_parsers.unsafe_parser (custom_result_parser poly_parser);
    data = name }


(** views *)

let view (select, select_type) from where =
  let query = { select = select; from = from; where = where } in
  match select_type with
    | Non_nullable (TRecord t) | Nullable (Some (TRecord t)) ->
        { t with data = Selection query }
    | _ -> assert false


(** results *)

let simple_select row = Simple_select row, get_type row

let group group_part result_part =
  Group_by (result_part, group_part), get_type result_part


(** queries *)
let select view = Select view
let insert table inserted_view =
  Insert (table, inserted_view)
let delete table row where =
  Delete (table, row, where)
let update table row set subtype_witness where =
  ignore subtype_witness;
  Update (table, row, set, where)
