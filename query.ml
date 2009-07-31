let nullable = function
  | None -> "NULL"
  | Some str -> str

let query dbh (sql_query : 'a Sql.query) : 'a =
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
