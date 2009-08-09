open Inner_sql

type 'a result_parser = 'a Inner_sql.result_parser

type 'a unsafe = 'a Sql.unsafe

type 'a table = 'a Sql.view
let table_view : 'a table -> 'a Sql.view = fun x -> x

type 'a column_type = field_type

type untyped = Sql.untyped
let untyped_column x = x

type poly_parser =
  { of_type : 'a . 'a column_type -> 'a Sql.t result_parser }

let poly_parser : poly_parser =
  { of_type = fun _typ -> use_unsafe_parser (parser_of_type _typ) }

module Table_type (T : sig val _type : sql_type -> field_type end) = struct
  let integer = T._type TInt
  let boolean = T._type TBool
  let text = T._type TString
end

module Nullable_type =
  Table_type (struct let _type t = Nullable (Some t) end)
module Non_nullable_type =
  Table_type (struct let _type t = Non_nullable t end)

let table descr custom_result_parser name =
  let result_parser =
    custom_result_parser poly_parser in
    Obj.magic
      { descr = descr;
        result_parser = result_parser;
        concrete = Table name }
(* abstaction breaking : indeed Inner_sql.view = Sql.view *)
