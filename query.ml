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

module type DATABASE = PGOCaml_generic.PGOCAML_GENERIC

module type QUERY = sig
  type 'a t
  type 'a monad

  val query : _ t -> 'a Sql.query -> 'a monad

  val view : _ t -> 'a Sql.view -> 'a list monad
end

module Make (Db : DATABASE) = struct
  type 'a monad = 'a Db.monad
  type 'a t = 'a Db.t
  
  let (>>=) = Db.bind

  let nullable = function
    | None -> "NULL"
    | Some str -> str

  let query dbh sql_query =
    let query = Sql.sql_of_query sql_query in
    print_endline query;
    let name = "query_result" in
    Db.prepare dbh ~query ~name () >>= fun () ->
    Db.execute dbh ~name ~params:[] () >>= fun result ->
    Db.close_statement dbh ~name () >>= fun () ->
    let result =
      let prepare_row row =
        Sql.unsafe (Array.of_list (List.map nullable row)) in
      List.map prepare_row result in
    Db.return (Sql.handle_query_results sql_query result)

  let view dbh view =
    query dbh (Sql.select view)
end

module Simple = Make(PGOCaml)
