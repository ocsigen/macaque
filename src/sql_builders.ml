open Sql_internals
open Sql_types

(** operations *)
let null_workaround (v, t) =
  (* NULL WORKAROUND
     
     It is assumed that any value with type Nullable None is NULL.
     This can work around several PostGreSQL Typing limitations
     wrt. NULL, such as the (NULL + NULL) issue or, worse :
     SELECT NULL + e.n FROM (SELECT NULL AS n) AS e
  *)
  if is_null_type t then null
  else (v, t)

let fixed_op op a b return_t =
  let input_t = unify (get_type a) (get_type b) in
  let retype value =
    let (v, _) = null_workaround value in
    v, input_t in
  null_workaround (Op ([retype a], op, [retype b]), return_t)

let op type_fun op a b =
  let input_t = unify (get_type a) (get_type b) in
  fixed_op op a b (type_fun input_t)

(** values *)

let field row path checker =
  ignore checker;
  (Field (row, path),
   get_sql_type (get_type row) path)

let default table field checker =
  ignore checker;
  match table.data with
    | Selection _ -> invalid_arg "default"
    | Table table -> List.assoc field table.defaults

let row name view =
  ( Row (name, view),
    Non_nullable (TRecord {view with data = ()}) )

let tuple fields producer record_parser =
  let record_t =
    let field_typ (name, field) = (name, get_type field) in
    { data = ();
      producer = unsafe_producer (fun tuple -> producer ~tuple);
      record_parser = Sql_parsers.unsafe_record_parser record_parser;
      descr = List.map field_typ fields } in
  Tuple fields, Non_nullable (TRecord record_t)

let if_then_else p a b =
  let t = unify (get_type a) (get_type b) in
  Case ([(p, a)], b), t

let match_null matched null_case other_case_fun =
  match get_type matched with
    | Nullable None when false -> null_case
        (* match_null's NULL WORKAROUND

           In accordance with the general NULL WORKAROUND discipline
           (wich assumes that every value with type (Nullable None) is
           effectively a Null value), match_null values are
           precomputed. *)
    | _ ->
        let other_case = other_case_fun matched in
        let t = unify (get_type null_case) (get_type other_case) in
        let is_null = Op ([matched], "IS NULL", []), Non_nullable TBool in
        Case ([(is_null, null_case)], other_case), t

(** tables *)
let table descr producer record_parser name (obj_witness, defaults) =
  ignore obj_witness;
  { descr = descr;
    producer = unsafe_producer (fun row -> producer ~row);
    record_parser = Sql_parsers.unsafe_record_parser record_parser;
    data = Table { name = name; defaults = defaults } }

(** views *)
let view (select, select_type) ?order_by ?limit ?offset from where =
  let query =
    { select = select;
      from = from;
      where = where;
      order_by = order_by;
      limit = limit;
      offset = offset } in
  match select_type with
    | Non_nullable (TRecord t) | Nullable (Some (TRecord t)) ->
        { t with data = Selection query }
    | _ -> assert false

type order = Sql_internals.order = Asc | Desc

(** results *)

let simple_select row = Simple_select row, get_type row

let group group_part result_part =
  Group_by (result_part, group_part), get_type result_part


(** queries *)
let get_table writable_view = match writable_view.data with
  | Selection _ -> assert false
  | Table data -> { writable_view with data = data }

let select view = Select view
let insert view inserted_view =
  Insert (get_table view, inserted_view)
let delete view row from where =
  Delete (get_table view, row, from, where)
let update view row set subtype_witness from where =
  ignore subtype_witness;
  Update (get_table view, row, set, from, where)
