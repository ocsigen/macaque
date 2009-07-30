type 'a view =
  { descr : types_descr;
    result_parser : 'a result_parser;
    concrete : concrete_view }
and concrete_view =
  | Table of table_name
  | Selection of select
and select = { select : select_result; from : from; where : where }
and select_result =
  | Simple_select of row
  | Group_by of row * row
and group_by = (row * row)
and from = (row_name * concrete_view) list
and where = reference list
and row = reference
and reference = reference' * field_type
and reference' =
  | Null
  | Value of value
  | Field of reference * field_name list
  | Binop of binop * reference * reference
  | Unop of string * reference
  | Row of (row_name * untyped view)
  | Tuple of reference tuple
and value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Record of untyped * (untyped -> reference)
and binop = op_type * string
and op_type = Logic | Comp | Arith (* TODO : useless ? clean that up *)
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
  | TFloat
  | TString
  | TBool
  | TRecord of untyped descr * (untyped -> reference)
and untyped = Obj.t

type 'a query =
  | Select of concrete_view
  | Insert of (table_name * concrete_view)
  | Delete of (table_name * row_name * where)
  | Update of (table_name * row_name * reference * where)

let rec get_field_type ref_type = function
  | [] -> ref_type
  | name :: path_rest ->
      match ref_type with
        | Nullable None -> Nullable None
        | Non_nullable (TRecord ((descr, _), _))
        | Nullable Some (TRecord ((descr, _), _)) ->
            get_field_type (List.assoc name descr) path_rest
        | _ -> invalid_arg "get_field_type"

let sql_type_of_string = function
  | "integer" -> TInt
  | "text" -> TString
  | "boolean" -> TBool
  | "double" -> TFloat
  | other -> failwith ("unknown sql type " ^ other)
let string_of_sql_type = function
  | TInt -> "integer"
  | TString -> "text"
  | TBool -> "boolean"
  | TFloat -> "double"
  | TRecord (_, _) -> "record"


(** untyped parsers *)
let unsafe_parser input_parser : untyped result_parser =
  fun input -> Obj.repr (input_parser input)

let (&&&) ptr_action safe_parser (input, input_ptr) =
  let cur_ptr = !input_ptr in
  ptr_action input_ptr;
  let input_str = input.(cur_ptr) in
  try safe_parser input_str
  with exn -> failwith
    (Printf.sprintf "Parser error [%s] on input %d [%s]"
       (Printexc.to_string exn) cur_ptr input_str)

let use_unsafe_parser unsafe_parser input = Obj.obj (unsafe_parser input)

let pack value value_type = Value value, Non_nullable value_type

let stringref_of_string s =
  pack (String (PGOCaml.string_of_string s)) TString 
let intref_of_string s =
  pack (Int (PGOCaml.int_of_string s)) TInt
let floatref_of_string s =
  pack (Float (PGOCaml.float_of_string s)) TFloat
let boolref_of_string s =
  pack (Bool (PGOCaml.bool_of_string s)) TBool

let bool_field_parser = unsafe_parser (incr &&& boolref_of_string)
let int_field_parser = unsafe_parser (incr &&& intref_of_string)
let float_field_parser = unsafe_parser (incr &&& floatref_of_string)
let string_field_parser = unsafe_parser (incr &&& stringref_of_string)
let error_field_parser= unsafe_parser (ignore &&& (fun _ -> failwith "Error parser"))

let option_field_parser (field_parser : untyped result_parser) : untyped result_parser =
  unsafe_parser
    (function (input_tab, input_ptr) as input ->
       if input_tab.(!input_ptr) = "NULL" then (incr input_ptr; (Null, Nullable None))
       else
         let r, t = use_unsafe_parser field_parser input in
         r, match t with
            | Non_nullable t -> Nullable (Some t) 
            | _ -> invalid_arg "option_field_parser")

let null_field_parser = option_field_parser error_field_parser

let record_parser (descr, row_parser) ast_builder =
  unsafe_parser
    (fun input ->
       Value (Record (Obj.repr (row_parser input), ast_builder)),
       TRecord ((descr, row_parser), ast_builder))

let parser_of_type =
  let parser_of_sql_type = function
    | TInt -> int_field_parser
    | TFloat -> float_field_parser
    | TString -> string_field_parser
    | TBool -> bool_field_parser
    | TRecord (full_descr, ast_builder) ->
        record_parser full_descr ast_builder in
  function
  | Non_nullable typ -> parser_of_sql_type typ
  | Nullable None -> null_field_parser
  | Nullable (Some typ) -> option_field_parser (parser_of_sql_type typ)

(* let rec value_type = function *)
(*   | Int _ -> TInt *)
(*   | String _ -> TString *)
(*   | Bool _ -> TBool *)
(*   | Float _ -> TFloat *)


let unsafe_view view =
  { view with result_parser = unsafe_parser view.result_parser }


(** Sql-representable values *)
type true_t
type false_t

module Value : sig
  type +'a t

  (** access functions *)
  val get : < t : 't; gettable : true_t; nullable : false_t; .. > t -> 't
  val getn : < t : 't; gettable : true_t; nullable : true_t; .. > t -> 't option

  (** parse function *)
  val parse : 'a t -> 'a t result_parser

  (** standard data types *)
  val int : int -> < t : int; numeric : true_t; nullable : false_t; gettable : true_t > t
  val bool : bool -> < t : bool; numeric : false_t; nullable : false_t; gettable : true_t > t
  val float : float -> < t : float; numeric : true_t; nullable : false_t; gettable : true_t > t
  val string : string -> < t : string; numeric : false_t; nullable : false_t; gettable : true_t > t 

  (** nullability *)
  val null : < nullable : true_t; t : _; numeric : _; gettable : true_t > t
  val nullable :
    < t : 't; numeric : 'n; gettable : 'g; nullable : false_t > t ->
    < t : 't; numeric : 'n; gettable : 'g; nullable : true_t > t

  (** standard operators *)
  val (+) :
    (< numeric : true_t; t : 't; nullable : 'n; gettable : _ > as 'a) t -> 'a t ->
     < numeric : true_t; t : 't; nullable : 'n; gettable : false_t > t
  val (=) :
    (< nullable : 'n; t : _; numeric : _; gettable : _ > as 'a) t -> 'a t ->
     < nullable : 'n; t : bool; numeric : false_t; gettable : false_t > t
  val (&&) : (< t : bool; .. > as 'a) t -> 'a t -> 'a t

  (** untyped access *)
  val untyped : 'a t -> untyped t
  val get_reference : _ t -> reference
  val get_type : _ t -> field_type

  (** unsafe constructors *)
  type +'a unsafe
  val unsafe : 'a -> 'a unsafe

  val force_gettable :
    < t : 't; nullable : 'nul; numeric : 'num; gettable : _ > t unsafe ->
    < t : 't; nullable : 'nul; numeric : 'num; gettable : true_t > t

  val field : < t : 'a; nullable : false_t; .. > t -> string list unsafe -> ('a -> 'b t) unsafe -> 'b t
  val row :
    string unsafe ->
    'row view ->
    < t : 'row; numeric : false_t; nullable : false_t; gettable : false_t > t
  val tuple :
    (string * untyped t) list unsafe ->
    'tup result_parser unsafe ->
    < t : 'tup; numeric : false_t; nullable : false_t; gettable : false_t > t


  (** select and view building *)
  type 'a result
  val view : 'a result -> from -> where -> 'a view
  val simple_select : < t : 'a; .. > t -> 'a result

  (** group by and accumulators *)
  type grouped_row
  val grouped_row : grouped_row

  type 'a group
  val accumulate : 'a t -> 'a group
  val count : 'a group ->
    < t : int; numeric : true_t; nullable : false_t; gettable : true_t > t

  val group : 'group_const t -> 'res t -> 'res result

  (** final query building *)
  val select : 'a view -> 'a query
  val insert : 'a view -> 'a view -> int query
  val delete : 'a view -> string unsafe -> < t : bool; .. > t list -> int query
  val update :
    'a view ->
    string unsafe ->
    'b t ->
    (< t : 'a; .. > t -> 'b t) unsafe ->
    < t : bool; .. > t list ->
    int query
end = struct
  type 'a t = reference

  let nullable (r, t) =
    let t = match t with
      | Non_nullable t -> Nullable (Some t)
      | Nullable t -> Nullable t in
    r, t

  let get_reference r = r
  let get_type (_, t) = t

  let get_val =
    let (!?) = Obj.magic in
    function
      | Int i -> !?i
      | Float x -> !?x
      | Bool b -> !?b
      | String s -> !?s
      | Record (o, _) -> !?o

  let get (r, t) =
    match r with
      | Value v -> get_val v
      | _ -> invalid_arg "get"

  let getn (r, t) = match r with
      | Null -> None
      | Value v -> Some (get_val v)
      | _ -> invalid_arg "getn"

  let parse ref =
    use_unsafe_parser (parser_of_type (get_type ref))

  type 'a unsafe = 'a
  let unsafe x = x
  let untyped x = x
  let force_gettable x = x

  let field row path checker =
    ignore checker;
    (Field (get_reference row, path),
     get_field_type (get_type row) path)

  let row name view =
    let view = unsafe_view view in
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
    let descr = List.map field_typ fields, unsafe_parser result_parser in
    (* rec : see "row" comment *)
    let rec reference =
      (Tuple (List.map field_ref fields),
       Non_nullable (TRecord (descr, fun _ -> reference))) in
    reference

  let bool b = Value (Bool b), Non_nullable TBool
  let int i = Value (Int i), Non_nullable TInt
  let float x = Value (Float x), Non_nullable TFloat
  let string s = Value (String s), Non_nullable TString

  let null = Null, Nullable None
  let option constr = function
    | None -> null
    | Some x -> nullable (constr x)

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

  let mono_op t = op (fun t' -> assert (t = t'); t)
  let poly_op return_t = op (fun _ -> return_t)

  let comp op = poly_op TBool (Comp, op)
  let logic op = mono_op TBool (Logic, op)
  let arith op = mono_op TInt (Arith, op)

  let (<), (=) = comp "<", comp "="
  let (&&) = logic "&&"
  let (||) = logic "||"
  let (+) = arith "+"

  type 'a result = select_result * field_type

  let view (select, select_type) from where =
    let query = { select = select; from = from; where = where } in
    match select_type with
      | Non_nullable (TRecord ((descr, result_parser), _))
      | Nullable (Some (TRecord ((descr, result_parser), _))) ->
          { descr = descr;
            result_parser = use_unsafe_parser result_parser;
            concrete = Selection query }
      | _ -> assert false

  let simple_select row = Simple_select (get_reference row), get_type row

  type grouped_row = unit
  let grouped_row = ()

  type 'a group = 'a t

  let accumulate x = x
  let count x = Unop ("count", x), Non_nullable TInt

  let group group_part result_part =
    Group_by (result_part, group_part), get_type result_part

  let get_table_name = function
    | {concrete = Table name} -> name
    | _ -> invalid_arg "get_table_name"
  let get_where = List.map get_reference

  let select view = Select view.concrete
  let insert table inserted_view =
    Insert (get_table_name table, inserted_view.concrete)
  let delete table row where =
    Delete (get_table_name table, row, get_where where)
  let update table row set subtype_witness where =
    ignore subtype_witness;
    Update (get_table_name table, row, get_reference set, get_where where)
end


(** SQL composite types flattening *)
let rec flatten_query = function
  | Select concrete -> Select (flatten_concrete concrete)
  | Insert (table, concrete) -> Insert (table, flatten_concrete concrete)
  | Delete (table, row, where) -> Delete (table, row, flatten_where where)
  | Update (table, row, set, where) ->
      Update (table, row, flatten_reference set, flatten_where where)
and flatten_concrete = function
  | Table t -> Table t
  | Selection q -> Selection (flatten_selection q)
and flatten_selection q =
  { select = flatten_select q.select;
    from = List.map flatten_table q.from;
    where = flatten_where q.where }
and flatten_where w = List.map flatten_reference w
and flatten_select = function
  | Simple_select row -> Simple_select (flatten_reference row)
  | Group_by (result, group) ->
      Group_by (flatten_reference result, flatten_reference group)
and flatten_reference ref =
  let rec flatten = function
    | Null, t -> Null, t
    (* termination : those first recursive calls have inferior
       reference depth *)
    | Field (row, []), _ -> flatten row
    | Field ((Tuple tup, t), field::path), _ ->
        flatten (Field (List.assoc field tup, path),
                 get_field_type t [field])
    | Field ((Field (row, path), _), path'), t ->
        flatten (Field (row, path @ path'), t)
    | Tuple tup, t ->
        let field (name, ref) = match flatten ref with
          | Tuple tup, _ ->
              let child (child_name, child_ref) =
                (name ^ "__" ^ child_name, child_ref) in
              List.map child tup
          | flat_val -> [(name, flat_val)] in
        Tuple (List.flatten (List.map field tup)), t
    (* termination : this pattern case will never match more than once
       on the same row, because we change the row type to Null *)
    | row, (( Non_nullable (TRecord((descr, _), _))
            | Nullable (Some (TRecord((descr, _), _))))  as t) ->
        let field (name, child_t) =
          name, flatten (Field ((row, Nullable None), [name]), child_t) in
        Tuple (List.map field descr), t
    (* row whose type was set Null before *)
    | (Row _), _ as flattened_row -> flattened_row
    | Value v, flat_t -> Value v, flat_t
    (* termination : subcalls on inferior reference depth *)
    | Unop (op, a), t -> Unop (op, flatten a), t
    | Binop (op, a, b), t ->
        Binop (op, flatten a, flatten b), t
    | Field (row, path), t ->
        match flatten row with
          | (Tuple _ | Field _), _ as reductible ->
              flatten (Field (reductible, path), t)
          | final -> Field (final, path), t in
  flatten ref
and flatten_table (name, comp) = (name, flatten_concrete comp)


(** SQL Query printing *)
open Printf

let string_of_list printer sep li = String.concat sep (List.map printer li)

let rec string_of_query = function
  | Select view -> string_of_concrete_view view
  | Insert (table, view) ->
      sprintf "INSERT INTO %s (%s)"
        (string_of_table_name table)
        (string_of_concrete_view view)
  | Delete (table, row, where) ->
      sprintf "DELETE FROM %s AS %s%s"
        (string_of_table_name table) row
        (string_of_where where)
  | Update (table, row, set, where) ->
      sprintf "UPDATE %s AS %s SET %s%s"
        (string_of_table_name table) row
        (let string_of_binding (id, v) =
           sprintf "%s = %s" id (string_of_reference v) in
         string_of_reference ~string_of_binding set)
        (string_of_where where)
and string_of_concrete_view = function
| Selection q -> sprintf "(%s)" (string_of_selection q)
| Table table_name -> string_of_table_name table_name
and string_of_selection q =
  sprintf "SELECT %s%s%s%s"
    (string_of_row (match q.select with
                      | Simple_select result
                      | Group_by (result, _) -> result))
    (string_of_from q.from)
    (string_of_where q.where)
    (match q.select with
       | Group_by (result, (Tuple (_::_ as const), _)) ->
           " GROUP BY " ^
             string_of_list (fun (_, r) -> string_of_reference r) ", " const
       | _ -> "")
and string_of_from = function
  | [] -> ""
  | from -> " FROM " ^ string_of_list string_of_table ", " from
and string_of_where = function
  | [] -> ""
  | where -> " WHERE " ^ string_of_list string_of_reference " AND " where
and string_of_row row = string_of_reference row
and string_of_binding (name, value) =
  let v = string_of_reference value in
  match value with
    | (Row _ | Tuple _), _ -> v (* flattened -> no binding *)
    | _ -> sprintf "%s AS %s" v name
and string_of_reference ?(string_of_binding = string_of_binding) (ref, _) =
  let silent_string_of_reference =
        let string_of_binding (name, value) = string_of_reference value in
        string_of_reference ~string_of_binding in
  match ref with
    | Value v -> string_of_value v
    | Null -> "NULL"
    | Unop (op, a) ->
        sprintf "%s(%s)" op (silent_string_of_reference a)
    | Binop ((_, op), a, b) -> sprintf "(%s %s %s)"
        (silent_string_of_reference a) op (silent_string_of_reference b)
    | Field ((Row (row_name, _), _), fields) ->
        sprintf "%s.%s" row_name (String.concat "__" fields)
    | Field (_, _) -> invalid_arg "string_of_row : invalid field access"
    | Row _ -> invalid_arg "string_of_row : non-flattened query"
    | Tuple tup ->
        if tup = [] then "NULL"
        else string_of_list string_of_binding ", " tup
and string_of_field (row, name) = match name with
  | field_name when true -> sprintf "%s.%s" row field_name
  | _ -> assert false
and string_of_table (row_name, table) =
  sprintf "%s AS %s" (string_of_concrete_view table) row_name
and string_of_table_name = function
  | (None, table) -> table
  | (Some schema, table) -> sprintf "%s.%s" schema table
and string_of_value = function
  | Int i -> string_of_int i
  | String s -> sprintf "'%s'" (String.escaped s)
  | Bool b -> string_of_bool b
  | Float x -> string_of_float x
  | Record (obj, ast_builder) -> string_of_reference (ast_builder obj)

let sql_of_query q = string_of_query (flatten_query q)
let sql_of_comp v = sql_of_query (Select v.concrete)
let parser_of_comp comp input_tab =
  comp.result_parser (input_tab, ref 0)
