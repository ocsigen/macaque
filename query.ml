(* macaque : query.ml
    MaCaQue : Macros for Caml Queries
    Copyright (C) 2009 Gabriel Scherer, Jérôme Vouillon

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library; see the file LICENSE.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.
*)

module type THREAD = sig
  include PGOCaml_generic.THREAD
  (* TODO : add exception handling *)
end

module type QUERY = sig
  module Db : PGOCaml_generic.PGOCAML_GENERIC

  val query : _ Db.t -> ?log:out_channel -> 'a Sql.query -> 'a Db.monad
  val view : _ Db.t -> ?log:out_channel -> 'a Sql.view -> 'a list Db.monad
  val view_one : _ Db.t -> ?log:out_channel -> 'a Sql.view -> 'a Db.monad
  val view_opt : _ Db.t -> ?log:out_channel -> 'a Sql.view -> 'a option Db.monad
end

module Make_with_Db
  (Thread : THREAD)
  (Db : PGOCaml_generic.PGOCAML_GENERIC with type 'a monad = 'a Thread.t) =
struct
  module Db = Db
  let (>>=) = Thread.(>>=)

  let query dbh ?log sql_query =
    let query = Sql.sql_of_query sql_query in
    (match log with
       | None -> ()
       | Some out -> Printf.fprintf out "%s\n%!" query);
    let name = "query_result" in
    Db.prepare dbh ~query ~name () >>= fun () ->
    Db.execute dbh ~name ~params:[] () >>= fun result ->
    Db.close_statement dbh ~name () >>= fun () ->
      let result = 
        let nullable = function
          | None -> "NULL"
          | Some str -> str in
        List.map (fun row -> Array.of_list (List.map nullable row)) result in
    Thread.return (Sql.handle_query_results sql_query (Sql.unsafe result))

  let view dbh ?log v =
    query dbh ?log (Sql.select v)

  let view_opt dbh ?log v =
    view dbh ?log v >>= function
      | [] -> Thread.return None
      | [res] -> Thread.return (Some res)
      | li ->
          let error = Printf.sprintf "view_opt : %d results" (List.length li) in
          Thread.fail (Failure error)

  let view_one dbh ?log v =
    view dbh ?log v >>= function
      | [res] -> Thread.return res
      | li ->
          let error = Printf.sprintf "view_one : %d results" (List.length li) in
          Thread.fail (Failure error)
end

module Make (Thread : THREAD) =
  Make_with_Db(Thread)(PGOCaml_generic.Make(Thread))

(* TODO : try to push into PGOCaml's mainstream *)
module Simple_thread = struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f =  f v
  let fail = raise

  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel
  let open_connection = Unix.open_connection
  let output_char = output_char
  let output_binary_int = output_binary_int
  let output_string = output_string
  let flush = flush
  let input_char = input_char
  let input_binary_int = input_binary_int
  let really_input = really_input
  let close_in = close_in
end


module Simple = Make_with_Db(Simple_thread)(PGOCaml)
