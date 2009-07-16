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

let parse_record str =
  let debug = false in
  let string_of_charlist charlist =
    let chart = Array.of_list charlist in
    let res = String.create (Array.length chart) in
    Array.iteri (String.set res) chart;
    res in
  let len = String.length str in
  let (++) hd (tl, i) = hd :: tl, i in
  let rec space i =
    if i = len then i
    else match str.[i] with
      | ' ' | '\n' | '\t' -> space (i + 1)
      | _ -> i in
  let rec parse depth i =
    let i = space i in
    if i = len then []
    else match str.[i] with
      | '"' ->
          if debug then Printf.eprintf "Quote begin : %d\n" i;
          let depth' = parse_quote i in
          assert (depth' > depth);
          if debug then Printf.eprintf "Quote depth : %d\n" depth';
          let quoted, i = parse_quoted depth' (i + depth') in
          let quoted = string_of_charlist quoted in
          if debug then Printf.eprintf "Quote end : %S %d\n" quoted i;
          quoted :: parse depth i
      | _ ->
          if debug then Printf.eprintf "Elem begin : %d\n" i;
          let elem, i = parse_elem i in
          let elem = string_of_charlist elem in
          if debug then Printf.eprintf "Elem end : %S %d\n" elem i;
          elem :: parse depth i
  and parse_elem i =
    if i = len then [], (i + 1)
    else match str.[i] with
      | ' ' | '\n' | '\t' -> parse_elem (i + 1)
      | ',' | ')' -> [], (i + 1)
      | c -> c ++ parse_elem (i + 1)
  and parse_quote i =
    if i = len || str.[i] <> '"' then 0
    else 1 + parse_quote (i + 1)
  and parse_quoted depth i =
    match parse_quote i with
      | 0 -> str.[i] ++ parse_quoted depth (i + 1)
      | d when d = depth && List.mem str.[space (i + depth)] [','; ')'] -> [], space(i + depth + 1)
      | d ->
          let quote = Array.to_list (Array.make d '"') in
          let res, i = parse_quoted depth (i + d) in quote @ res, i
  in
  let rec record_begin i =
    if i = len || str.[i] <> '(' then i
    else record_begin (i + 1) in
  try
    Array.of_list (parse 0 (record_begin 0))
  with _ -> failwith "parse_record : Fatal error"

let row_field_parser row_parser =
(*   unsafe_parser row_parser *)
  let parse str = row_parser (parse_record str, ref 0) in
  unsafe_parser (incr &&& parse)
  

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

(** Sql-representable values *)
module Value : sig
  type 'a t
  val concrete : 'a t -> value
  val int : int -> int t
  val string : string -> string t
end = struct
  type 'a t = value
  let concrete v = v
  let int i = Int i
  let string s = String s
end

let value_type = function
  | Int _ -> Not_null TInt
  | String _ -> Not_null TString

let nullable = function
  | Nullable t -> Nullable t
  | Not_null t -> Nullable (Some t)


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
| Row (table_name, descr) ->
    let rec flatten (field_name, field_type) = match field_type with
      | Not_null (TRecord (descr, _)) | Nullable (Some (TRecord (descr, _))) ->
          List.flatten (List.map flatten descr)
      | _ -> [(field_name, Field (table_name, field_name))] in
    string_of_row (Tuple (List.flatten (List.map flatten descr)))
and string_of_condition = function
| Eq (a, b) -> sprintf "%s = %s" (string_of_reference a) (string_of_reference b)
and string_of_binding (name, value) =
  let v = string_of_reference value in
  match value with
    | Row_ref _ when false -> v (* flattened -> no binding *)
    | _ -> sprintf "%s AS %s" v name
and string_of_reference = function
| Row_ref row when false -> string_of_row row
| Row_ref (Tuple tup) -> sprintf "ROW(%s)" (string_of_list (fun (_, v) -> string_of_reference v) ", " tup)
| Row_ref (Row (table_name, descr)) -> table_name
| Field f -> string_of_field f
| Value (Int i) -> string_of_int i
| Value (String s) -> sprintf "'%s'" (String.escaped s)
| Null -> "NULL"
and string_of_field (row, name) = sprintf "%s.%s" row name
and string_of_table (row_name, table) =
  sprintf "%s AS %s" (string_of_concrete table) row_name
and string_of_table_name = function
  | (None, table) -> table
  | (Some schema, table) -> sprintf "%s.%s" schema table
