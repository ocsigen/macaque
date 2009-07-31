type true_t
type false_t

type +'a t

type 'a result_parser = string array * int ref -> 'a

(** access functions *)
val get : < t : 't; gettable : true_t; nullable : false_t; .. > t -> 't
val getn : < t : 't; gettable : true_t; nullable : true_t; .. > t -> 't option

(** parse function *)
val parse : 'a t -> 'a t result_parser

(** standard data types (usable from user code) *)
module Data : sig
  val int : int -> < t : int; numeric : true_t; nullable : false_t; gettable : true_t > t
  val bool : bool -> < t : bool; numeric : false_t; nullable : false_t; gettable : true_t > t
  val float : float -> < t : float; numeric : true_t; nullable : false_t; gettable : true_t > t
  val string : string -> < t : string; numeric : false_t; nullable : false_t; gettable : true_t > t
end

(** standard operators (usable from user code) *)
module Op : sig
  val null : < nullable : true_t; t : _; numeric : _; gettable : true_t > t
  val nullable :
    < t : 't; numeric : 'n; gettable : 'g; nullable : false_t > t ->
    < t : 't; numeric : 'n; gettable : 'g; nullable : true_t > t

  val (+) :
    (< numeric : true_t; t : 't; nullable : 'n; gettable : _ > as 'a) t -> 'a t ->
     < numeric : true_t; t : 't; nullable : 'n; gettable : false_t > t
  val (=) :
    (< nullable : 'n; t : _; numeric : _; gettable : _ > as 'a) t -> 'a t ->
     < nullable : 'n; t : bool; numeric : false_t; gettable : false_t > t
  val (&&) : (< t : bool; .. > as 'a) t -> 'a t -> 'a t
end

(** untyped access *)
type untyped
val untyped : 'a t -> untyped t

type +'a view
val untyped_view : 'a view -> untyped view

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

type from = (string * untyped view) list
type where = untyped t list

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
type 'a query

val select : 'a view -> 'a list query
val insert : 'a view -> 'a view -> unit query
val delete : 'a view -> string unsafe -> < t : bool; .. > t list -> unit query
val update :
  'a view ->
  string unsafe ->
  'b t ->
  (< t : 'a; .. > t -> 'b t) unsafe ->
  < t : bool; .. > t list ->
  unit query

(** query printing *)
val sql_of_query : 'a query -> string
val sql_of_view : 'a view -> string

(** handle result from PGOCaml call *)
val handle_query_results : 'a query -> string array unsafe list -> 'a
