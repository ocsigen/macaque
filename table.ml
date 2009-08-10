open Inner_sql

type 'a result_parser = 'a Inner_sql.result_parser

type 'a unsafe = 'a Sql.unsafe

type 'a table = 'a Inner_sql.view
let view : 'a table -> 'a Sql.view = Obj.magic
(* abstaction breaking : indeed Inner_sql.view = Sql.view *)

type 'a column_type = field_type

type untyped = Sql.untyped
let untyped_column x = x

type poly_parser =
  { of_type : 'a . 'a column_type -> 'a Sql.t result_parser }

let poly_parser : poly_parser =
  { of_type = fun _typ -> use_unsafe_parser (parser_of_type _typ) }

type ('a, 'b) witness = 'b
let nullable_witness = true
let non_nullable_witness = false

module Table_type = struct
  let _type t = function
    | true -> Nullable (Some t)
    | false -> Non_nullable t in
  let integer = _type TInt
  let boolean = _type TBool
  let text = _type TString
end

let table descr custom_result_parser name =
  let result_parser =
    custom_result_parser poly_parser in
  { descr = descr;
    result_parser = result_parser;
    concrete = Table name }
