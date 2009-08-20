open Sql_internals
open Sql_types

(** operations *)

let op type_fun op a b =
  Op ([a], op, [b]),
  type_fun (unify (get_type a) (get_type b))

(** values *)

let field row path checker =
  ignore checker;
  (Field (row, path),
   get_sql_type (get_type row) path)

let row name view =
  ( Row (name, view),
    Non_nullable (TRecord {view with data = ()}) )

let tuple obj producer record_parser =
  let fields = producer obj in
  let record_t =
    let field_typ (name, field) = (name, get_type field) in
    { data = ();
      producer = unsafe_producer producer;
      record_parser = Sql_parsers.unsafe_record_parser record_parser;
      descr = List.map field_typ fields } in
  Tuple fields, Non_nullable (TRecord record_t)

let if_then_else p a b =
  let t = unify (get_type a) (get_type b) in
  Case ([(p, a)], b), t

let match_null matched null_case other_case_fun =
  match get_type matched with
    | Nullable None when false -> null_case (* CURRENT STATE : disabled for testing *)
        (* this special case is a work-around to the match_null
           problem reported in tests/match_null.ml

           it makes match_null semantic much more fragile : we must
           NOT have a non-null value with a 'Nullable None' type, wich
           is only weakly enforced in the rest of the code due to
           complex SQL nullability behaviors

           TODO : replace by a general inference framework *)
    | _ ->
        let other_case = other_case_fun matched in
        let t = unify (get_type null_case) (get_type other_case) in
        let is_null = Op ([matched], "IS NULL", []), Non_nullable TBool in
        Case ([(is_null, null_case)], other_case), t

(** tables *)

let table descr producer record_parser name =
  { descr = descr;
    producer = unsafe_producer producer;
    record_parser = Sql_parsers.unsafe_record_parser record_parser;
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
