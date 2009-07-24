type +'a view =
  { descr : types_descr;
    result_parser : 'a result_parser;
    concrete : concrete_view }
and concrete_view =
  | Table of table_name
  | Query of query
and query = { select : select; from : from; where : where }
and select = row
and from = (row_name * concrete_view) list
and where = reference list
and row = reference
and reference =
  | Null
  | Value of value
  | Field of field
  | Binop of binop * reference * reference
  | Row of (row_name * untyped view)
  | Tuple of reference tuple
and field = row_name * field_name list
and value =
  | Int of int
  | String of string
  | Bool of bool
and binop = op_type * string
and op_type = Logic | Comp | Arith
and table_name = string option * string
and row_name = string
and 'a tuple = (field_name * 'a) list
and field_name = string
and 'a descr = types_descr * 'a result_parser
and 'a result_parser = string array * int ref -> 'a
and types_descr = field_type tuple
and field_type =
  | Non_nullable of sql_type
  | Nullable of sql_type option
and sql_type =
  | TInt
  | TString
  | TBool
  | TRecord of untyped descr
and untyped = Obj.t

let rec get_field_type descr = function
  | [] -> invalid_arg "get_field_type"
  | [name] -> List.assoc name descr
  | record_name :: path_rest ->
      match List.assoc record_name descr with
        | Nullable None -> Nullable None
        | Non_nullable (TRecord (descr_rest, _))
        | Nullable Some (TRecord (descr_rest, _)) ->
            get_field_type descr_rest path_rest
        | _ -> invalid_arg "get_field_type"

let sql_type_of_string = function
  | "integer" -> TInt
  | "text" -> TString
  | "boolean" -> TBool
  | other -> failwith ("unknown sql type " ^ other)
let string_of_sql_type = function
  | TInt -> "integer"
  | TString -> "text"
  | TBool -> "boolean"
  | TRecord (_, _) -> "record"

(** untyped parsers *)
let unsafe_parser input_parser : untyped result_parser =
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

let bool_field_parser = unsafe_parser (incr &&& PGOCaml.bool_of_string)
let int_field_parser = unsafe_parser (incr &&& PGOCaml.int_of_string)
let string_field_parser = unsafe_parser (incr &&& PGOCaml.string_of_string)
let error_field_parser= unsafe_parser (ignore &&& (fun _ -> failwith "Error parser"))

let option_field_parser (field_parser : untyped result_parser) : untyped result_parser =
  unsafe_parser
    (function (input_tab, input_ptr) as input ->
       if input_tab.(!input_ptr) = "NULL" then (incr input_ptr; None)
       else Some (use_unsafe_parser field_parser input))

let null_field_parser = option_field_parser error_field_parser

let parser_of_type =
  let parser_of_sql_type = function
    | TInt -> int_field_parser
    | TString -> string_field_parser
    | TBool -> bool_field_parser
    | TRecord (_, row_parser) -> row_parser in
  function
  | Non_nullable typ -> parser_of_sql_type typ
  | Nullable None -> null_field_parser
  | Nullable (Some typ) -> option_field_parser (parser_of_sql_type typ)

let parse_type field_type input =
  use_unsafe_parser (parser_of_type field_type) input

let rec value_type = function
  | Int _ -> TInt
  | String _ -> TString
  | Bool _ -> TBool


let unsafe_view view =
  { view with result_parser = unsafe_parser view.result_parser }

(** Sql-representable values *)
module Value : sig
  type nullable
  type non_nullable
  type ('a, 'nullability) t

  (** type machinery *)
  val nullable : ('a, non_nullable) t -> ('a option, nullable) t
  val untyped : ('a, 'b) t -> (untyped, untyped) t
 
  (** access *)
  val get_reference : ('a, 'n) t -> reference
  val get_type : ('a, 'n) t -> field_type

  (** data constructors *)
  val bool : bool -> (bool, non_nullable) t
  val int : int -> (int, non_nullable) t
  val string : string -> (string, non_nullable) t
  val null : ('a, nullable) t

  (** unsafe constructors *)
  type 'a unsafe
  val unsafe : 'a -> 'a unsafe
  val field : (string * string list) unsafe -> 'a view -> ('a -> 'b unsafe) -> ('b, 'n) t
  val row : string unsafe -> 'a view -> ('a, non_nullable) t
  val tuple :
    (string * (untyped, untyped) t) list unsafe
    -> 'obj result_parser unsafe
    -> ('obj, non_nullable) t

  (** data operations *)
  val (<) : ('a, 'n) t -> ('a, 'n) t -> (bool, 'n) t
  val (=) : ('a, 'n) t -> ('a, 'n) t -> (bool, 'n) t
  val (+) : (int, 'n) t -> (int, 'n) t -> (int, 'n) t
  val (||) : (bool, 'n) t -> (bool, 'n) t -> (bool, 'n) t

  (** view builder *)
  val view :  (< ..> as 'a, 'n) t -> from -> where -> 'a view
              
end = struct
  type nullable
  type non_nullable
  type ('a, 'b) t = reference * field_type

  let nullable (r, t) =
    let t = match t with
      | Non_nullable t -> Nullable (Some t)
      | Nullable t -> Nullable t in
    r, t

  let get_reference (r, _) = r
  let get_type (_, t) = t

  type 'a unsafe = 'a
  let unsafe x = x

  let untyped x = x

  type unsafe_t = (untyped, untyped) t unsafe
  let field (row, path) view checker =
    ignore checker;
    (Field (row, path), get_field_type view.descr path)

  let row name view =
    let view = unsafe_view view in
    Row (name, view),
    Non_nullable (TRecord (view.descr, view.result_parser))

  let tuple fields result_parser =
    let field_ref (name, field) = (name, get_reference field) in
    let field_typ (name, field) = (name, get_type field) in
    let record_type = List.map field_typ fields, unsafe_parser result_parser in
    (Tuple (List.map field_ref fields), Non_nullable (TRecord record_type))

  let bool b = Value (Bool b), Non_nullable TBool
  let int i = Value (Int i), Non_nullable TInt
  let string s = Value (String s), Non_nullable TString

  let null = Null, Nullable None
  let option constr = function
    | None -> null
    | Some x -> nullable (constr x)

  let op type_fun op (a, t) (b, t') =
    match t, t' with
      | Non_nullable t, Non_nullable t' ->
          (* none is nullable *)
          assert (t = t');
          Binop(op, a, b), Non_nullable (type_fun t)
      | _ ->
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

  let mono_op t = op (fun t' -> assert (t = t'); t)
  let poly_op return_t = op (fun _ -> return_t)

  let comp op = poly_op TBool (Comp, op)
  let logic op = mono_op TBool (Logic, op)
  let arith op = mono_op TInt (Arith, op)

  let (<), (=) = comp "<", comp "="
  let (||) = logic "||"
  let (+) = arith "+"

  let view select from where =
    let query = { select = get_reference select; from = from; where = where } in
    match get_type select with
      | Non_nullable (TRecord (descr, result_parser))
      | Nullable (Some (TRecord (descr, result_parser))) ->
          { descr = descr;
            result_parser = use_unsafe_parser result_parser;
            concrete = Query query }
      | _ -> assert false
end

(** SQL composite types flattening *)
let rec flatten_concrete = function
  | Query q -> Query (flatten_query q)
  | Table t -> Table t
and flatten_query q =
  { select = flatten_row q.select;
    from = List.map flatten_table q.from;
    where = List.map flatten_condition q.where }
and flatten_row = function
  | Row (table_name, {descr=table_descr}) ->
      let rec field acc (field_name, field_type) =
        let acc = field_name :: acc in
        match field_type with
          | Non_nullable (TRecord (descr, _))
          | Nullable (Some (TRecord (descr, _))) ->
              (field_name, flatten_descr acc descr)
          | _ ->
              let path = String.concat "__" (List.rev acc) in
              (field_name, Field (table_name, [path]))
      and flatten_descr acc descr = Tuple (List.map (field acc) descr) in
      flatten_row (flatten_descr [] table_descr)
  | Tuple tup ->
      let field (field_name, field_val) = match field_val with
        | (Row _ | Tuple _) as row ->
            (match flatten_row row with
               | Row _ -> assert false (* flatten result must be tuple *)
               | Tuple child_tup ->
                   let children (child_name, child_val) =
                     (field_name ^ "__" ^ child_name, child_val) in
                   (List.map children child_tup)
               | _ -> assert false)
        | flat_value -> [(field_name, flat_value)] in
      Tuple (List.flatten (List.map field tup))
  | _ -> assert false
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
    (if q.where = [] then "" else " WHERE " ^ string_of_list string_of_reference " AND " q.where)
and string_of_row row = string_of_reference row
and string_of_binding (name, value) =
  let v = string_of_reference value in
  match value with
    | (Row _ | Tuple _) -> v (* flattened -> no binding *)
    | _ -> sprintf "%s AS %s" v name
and string_of_reference = function
| Value (Int i) -> string_of_int i
| Value (String s) -> sprintf "'%s'" (String.escaped s)
| Value (Bool b) -> string_of_bool b
| Null -> "NULL"
| Binop (op, a, b) -> sprintf "(%s %s %s)"
    (string_of_reference a) (string_of_op op) (string_of_reference b)
| Field (table, fields) -> sprintf "%s.%s" table (String.concat "__" fields)
| Row _ -> invalid_arg "string_of_row : non-flattened query"
| Tuple tup ->
    if tup = [] then "NULL"
    else string_of_list string_of_binding ", " tup
and string_of_op (_, op_str) = op_str
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
