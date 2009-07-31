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

let nullable = function
  | None -> "NULL"
  | Some str -> str

let query dbh sql_query =
  let query = Sql.sql_of_query sql_query in
  print_endline query;
  let name = "query_result" in
  ignore (PGOCaml.prepare dbh ~query ~name ());
  let result =
    try `Result (PGOCaml.execute dbh ~name ~params:[] ())
    with exn -> `Exn exn in
  PGOCaml.close_statement dbh ~name ();
  let result = match result with
    | `Result res ->
        let prepare_row row =
          Sql.unsafe (Array.of_list (List.map nullable row)) in
        List.map prepare_row res
    | `Exn exn -> raise exn in
  Sql.handle_query_results sql_query result

let view dbh view =
  query dbh (Sql.select view)
