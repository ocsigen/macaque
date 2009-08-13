open Sql_internals

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
       Value (Record { instance = Obj.repr (row_parser input);
                       ast_builder = ast_builder }),
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

let parser_of_comp comp input_tab =
  comp.result_parser (input_tab, ref 0)
