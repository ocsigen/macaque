type +'a table

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

val view : 'a table -> 'a Sql.view

type ('a, 'b) witness
val non_nullable_witness : (Sql.non_nullable, bool) witness
val nullable_witness : (Sql.nullable, bool) witness

(** Field types constructors (usable from <:table< .. >>) *)
module Table_type : sig
  val integer : ('nul, bool) witness ->
    < get : unit; nul : 'nul; t : Sql.int_t > column_type
  val boolean : ('nul, bool) witness ->
    < get : unit; nul : 'nul; t : Sql.bool_t > column_type
  val text : ('nul, bool) witness ->
    < get : unit; nul : 'nul; t : Sql.string_t > column_type
end
