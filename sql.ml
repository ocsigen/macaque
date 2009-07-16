type +'a view =
  { descr : types_descr;
    result_parser : 'a result_parser;
    concrete : concrete_view }
    
and concrete_view =
  | Table of table_name
  | Query of query
and query = { select : row;
              from : (row_name * concrete_view) list;
              where : pred list }
and row =
  | Row of (row_name * types_descr)
  | Tuple of reference tuple
and field = row_name * field_name
and pred = Eq of reference * reference
and reference =
  | Null
  | Row_ref of row
  | Field of field
  | Value of value
and value =
  | Int of int
  | String of string
and table_name = string option * string
and row_name = string
and 'a tuple = (field_name * 'a) list
and field_name = string

and 'a descr = types_descr * 'a result_parser
and 'a result_parser = string array * int ref -> 'a
and types_descr = field_type tuple
and field_type =
  | Not_null of sql_type
  | Nullable of sql_type option
and sql_type =
  | TInt
  | TString
  | TRecord of unsafe_descr
and unsafe_descr = Obj.t descr
and unsafe_parser = Obj.t result_parser

let get_field_type descr name = List.assoc name descr

and sql_type_of_string = function
  | "integer" -> TInt
  | "text" -> TString
  | other -> failwith ("unknown sql type " ^ other)
let string_of_sql_type = function
  | TInt -> "integer"
  | TString -> "text"
  | TRecord (_, _) -> "record"

(** untyped parsers *)
let unsafe_parser input_parser : unsafe_parser =
  fun input -> Obj.repr (input_parser input)

let (&&&) ptr_action safe_parser (input, input_ptr) =
  let res =
    let input_str = input.(!input_ptr) in
    try safe_parser input_str
    with exn -> failwith
      (Printf.sprintf "Parser error [%s] on input %d [%s]"
         (Printexc.to_string exn) !input_ptr input_str) in
  ptr_action input_ptr;
  res

let use_unsafe_parser unsafe_parser input = Obj.obj (unsafe_parser input)

let int_field_parser = unsafe_parser (incr &&& int_of_string)
let string_field_parser = unsafe_parser (incr &&& (fun s -> s))
let error_field_parser= unsafe_parser (ignore &&& (fun _ -> failwith "Error parser"))

let option_field_parser (field_parser : unsafe_parser) : unsafe_parser =
  unsafe_parser
    (function (input_tab, input_ptr) as input ->
       if input_tab.(!input_ptr) = "NULL" then (incr input_ptr; None)
       else Some (use_unsafe_parser field_parser input))

let null_field_parser = option_field_parser error_field_parser

let row_field_parser row_parser =
  unsafe_parser row_parser  

let parser_of_type =
  let parser_of_sql_type = function
    | TInt -> int_field_parser
    | TString -> string_field_parser
    | TRecord (descr, row_parser) -> row_field_parser row_parser in
  function
  | Not_null typ -> parser_of_sql_type typ
  | Nullable None -> null_field_parser
  | Nullable (Some typ) -> option_field_parser (parser_of_sql_type typ)

let call descr field_name input =
  use_unsafe_parser (parser_of_type (get_field_type descr field_name)) input

let value_type = function
  | Int _ -> Not_null TInt
  | String _ -> Not_null TString

(** Sql-representable values *)
module Value : sig
  type 'a t
  val concrete : 'a t -> value
  val get_type : 'a t -> field_type
  val int : int -> int t
  val string : string -> string t
end = struct
  type 'a t = value
  let concrete v = v
  let int i = Int i
  let string s = String s
  let get_type v = value_type v
end

let nullable = function
  | Nullable t -> Nullable t
  | Not_null t -> Nullable (Some t)

(** SQL composite types flattening *)
let rec flatten_concrete = function
  | Query q -> Query (flatten_query q)
  | Table t -> Table t
and flatten_query q =
  { select = flatten_row q.select;
    from = List.map flatten_table q.from;
    where = List.map flatten_condition q.where }
and flatten_row = function
  | Row (table_name, descr) ->
      let rec field acc (field_name, field_type) =
        let acc = field_name :: acc in
        match field_type with
          | Not_null (TRecord (descr, _))
          | Nullable (Some (TRecord (descr, _))) ->
              (field_name, Row_ref (flatten_descr acc descr))
          | _ ->
              let path = String.concat "__" (List.rev acc) in
              (field_name, Field (table_name, path))
      and flatten_descr acc descr = Tuple (List.map (field acc) descr) in
      flatten_row (flatten_descr [] descr)
  | Tuple tup ->
      let field (field_name, field_val) = match field_val with
        | Row_ref row ->
            (match flatten_row row with
               | Row _ -> assert false (* flatten result must be tuple *)
               | Tuple child_tup ->
                   let children (child_name, child_val) =
                     (field_name ^ "__" ^ child_name, child_val) in
                   (List.map children child_tup))
        | flat_value -> [(field_name, flat_value)] in
      Tuple (List.flatten (List.map field tup))
and flatten_table (name, comp) = (name, flatten_concrete comp)
and flatten_condition c = c (* TODO *)

let flatten = flatten_concrete

(** SQL Query printing *)
open Printf

let string_of_list printer sep li = String.concat sep (List.map printer li)

let rec string_of_concrete = function
| Query q -> sprintf "(%s)" (string_of_query q)
| Table table_name -> string_of_table_name table_name
and string_of_query q =
  sprintf "SELECT %s%s%s"
    (string_of_row q.select)
    (if q.from = [] then "" else " FROM " ^ string_of_list string_of_table ", " q.from)
    (if q.where = [] then "" else " WHERE " ^ string_of_list string_of_condition " AND " q.where)
and string_of_row = function
| Tuple tup ->
    if tup = [] then "NULL"
    else string_of_list string_of_binding ", " tup
| Row _ -> invalid_arg "string_of_row : non-flattened query"
and string_of_condition = function
| Eq (a, b) -> sprintf "%s = %s" (string_of_reference a) (string_of_reference b)
and string_of_binding (name, value) =
  let v = string_of_reference value in
  match value with
    | Row_ref _ -> v (* flattened -> no binding *)
    | _ -> sprintf "%s AS %s" v name
and string_of_reference = function
| Row_ref row -> string_of_row row
| Field f -> string_of_field f
| Value (Int i) -> string_of_int i
| Value (String s) -> sprintf "'%s'" (String.escaped s)
| Null -> "NULL"
and string_of_field (row, name) = match name with
  | field_name when true -> sprintf "%s.%s" row field_name
  | _ -> assert false
and string_of_table (row_name, table) =
  sprintf "%s AS %s" (string_of_concrete table) row_name
and string_of_table_name = function
  | (None, table) -> table
  | (Some schema, table) -> sprintf "%s.%s" schema table

let sql_of_comp comp = string_of_concrete (flatten comp.concrete)
let parser_of_comp comp input_tab =
  comp.result_parser (input_tab, ref 0)
