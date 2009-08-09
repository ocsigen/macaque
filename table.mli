type +'a table = 'a Sql.view

type 'a result_parser = 'a Sql.result_parser
type untyped = Sql.untyped

type 'a column_type
type poly_parser =
  { of_type : 'a . 'a column_type -> 'a Sql.t result_parser }

val untyped_column : 'a column_type -> untyped column_type

val table :
  (string * untyped column_type) list ->
  (poly_parser -> 'row result_parser) ->
  (string option * string) ->
  'row table

val table_view : 'a table -> 'a Sql.view

(** Field types constructors (usable from <:table< .. >>) *)
module Non_nullable_type : sig
  val integer :
    < get : unit; nul : Sql.non_nullable; t : Sql.int_t > column_type
  val boolean :
    < get : unit; nul : Sql.non_nullable; t : Sql.bool_t > column_type
  val text :
    < get : unit; nul : Sql.non_nullable; t : Sql.string_t > column_type
end

module Nullable_type : sig
  val integer :
    < get : unit; nul : Sql.nullable; t : Sql.int_t > column_type
  val boolean :
    < get : unit; nul : Sql.nullable; t : Sql.bool_t > column_type
  val text :
    < get : unit; nul : Sql.nullable; t : Sql.string_t > column_type
end
