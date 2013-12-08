open Sql_base
open Sql_internals

let (&&&) ptr_action safe_parser (input, input_ptr) =
  let cur_ptr = !input_ptr in
  ptr_action input_ptr;
  let input_str = input.(cur_ptr) in
  try safe_parser input_str
  with exn -> failwith
    (Printf.sprintf "Parser error [%s] on input %d [%s]"
       (Printexc.to_string exn) cur_ptr input_str)

let unsafe_parser input_parser : untyped result_parser =
  fun input -> Obj.repr (input_parser input)
let use_unsafe_parser unsafe_parser input = Obj.obj (unsafe_parser input)

let unsafe_record_parser record_parser : untyped record_parser =
  fun descr -> unsafe_parser (record_parser descr)

let pack atom atom_type : value = Atom atom, Non_nullable atom_type

let unitval_of_string s =
  pack (Unit (PGOCaml.unit_of_string s)) TBool
let boolval_of_string s =
  pack (Bool (PGOCaml.bool_of_string s)) TBool
let int16val_of_string s =
  pack (Int16 (PGOCaml.int16_of_string s)) TInt16
let int32val_of_string s =
  pack (Int32 (PGOCaml.int32_of_string s)) TInt32
let int64val_of_string s =
  pack (Int64 (PGOCaml.int64_of_string s)) TInt64
let floatval_of_string s =
  pack (Float (PGOCaml.float_of_string s)) TFloat
let stringval_of_string s =
  pack (String (PGOCaml.string_of_string s)) TString
let byteaval_of_string s =
  pack (Bytea (PGOCaml.bytea_of_string s)) TBytea
let timeval_of_string s =
  pack (Time (PGOCaml.time_of_string s)) TTime
let dateval_of_string s =
  pack (Date (PGOCaml.date_of_string s)) TDate
let timestampval_of_string s =
  pack (Timestamp (PGOCaml.timestamp_of_string s)) TTimestamp
let timestamptzval_of_string s =
  pack (Timestamptz (PGOCaml.timestamptz_of_string s)) TTimestamptz
let intervalval_of_string s =
  pack (Interval (PGOCaml.interval_of_string s)) TInterval
let int32_array_of_string s =
  pack (Int32_array (PGOCaml.int32_array_of_string s)) (TArray TInt32)
let string_array_of_string s =
  pack (String_array (PGOCaml.string_array_of_string s)) (TArray TString)

let unit_field_parser = unsafe_parser (incr &&& unitval_of_string)
let bool_field_parser = unsafe_parser (incr &&& boolval_of_string)
let int16_field_parser = unsafe_parser (incr &&& int16val_of_string)
let int32_field_parser = unsafe_parser (incr &&& int32val_of_string)
let int64_field_parser = unsafe_parser (incr &&& int64val_of_string)
let float_field_parser = unsafe_parser (incr &&& floatval_of_string)
let string_field_parser = unsafe_parser (incr &&& stringval_of_string)
let bytea_field_parser = unsafe_parser (incr &&& byteaval_of_string)
let time_field_parser = unsafe_parser (incr &&& timeval_of_string)
let date_field_parser = unsafe_parser (incr &&& dateval_of_string)
let timestamp_field_parser = unsafe_parser (incr &&& timestampval_of_string)
let timestamptz_field_parser = unsafe_parser (incr &&& timestamptzval_of_string)
let interval_field_parser = unsafe_parser (incr &&& intervalval_of_string)
let int32_array_field_parser = unsafe_parser (incr &&& int32_array_of_string)
let string_array_field_parser = unsafe_parser (incr &&& string_array_of_string)

let error_field_parser=
  unsafe_parser (ignore &&& (fun _ -> failwith "Error parser"))

let option_field_parser field_parser  =
  unsafe_parser
    (function (input_tab, input_ptr) as input ->
       if input_tab.(!input_ptr) = "NULL" then
         (incr input_ptr;
          (Null, Nullable None))
       else
         let r, t = use_unsafe_parser field_parser input in
         r, match t with
            | Non_nullable t -> Nullable (Some t)
            | _ -> invalid_arg "option_field_parser")

let null_field_parser = option_field_parser error_field_parser

let record_parser t =
  unsafe_parser
    (fun input ->
       let instance = Obj.repr (t.record_parser t.descr input) in
       pack (Record instance) (TRecord t))

let parser_of_type =
  let parser_of_sql_type = function
    | TUnit -> unit_field_parser
    | TBool -> bool_field_parser
    | TInt16 -> int16_field_parser
    | TInt32 -> int32_field_parser
    | TInt64 -> int64_field_parser
    | TFloat -> float_field_parser
    | TString -> string_field_parser
    | TBytea -> bytea_field_parser
    | TTime -> time_field_parser
    | TDate -> date_field_parser
    | TTimestamp -> timestamp_field_parser
    | TTimestamptz -> timestamptz_field_parser
    | TInterval -> interval_field_parser
    | TArray TInt32 -> int32_array_field_parser
    | TArray TString -> string_array_field_parser
    | TArray _ -> assert false
    | TRecord t -> record_parser t in
  function
  | Non_nullable typ -> parser_of_sql_type typ
  | Nullable None -> null_field_parser
  | Nullable (Some typ) -> option_field_parser (parser_of_sql_type typ)

let parser_of_comp comp input_tab =
  comp.record_parser comp.descr (input_tab, ref 0)
