open Sql

let nullable = function
  | None -> "NULL"
  | Some str -> str

let query dbh (sql_query : 'a query) : 'a =
  let query = sql_of_query sql_query in
  print_endline query;
  let name = "query_result" in
  ignore (PGOCaml.prepare dbh ~query ~name ());
  let result =
    try `Result (PGOCaml.execute dbh ~name ~params:[] ())
    with exn -> `Exn exn in
  PGOCaml.close_statement dbh ~name ();
  let result = match result with
    | `Result res -> res
    | `Exn exn -> raise exn in
  match sql_query with
    | Select comp ->
        let parse row =
          parser_of_comp comp (Array.of_list (List.map nullable row)) in
        Obj.magic (List.map parse result)
    | _ -> Obj.magic ()

let view dbh view =
  query dbh (Sql.Value.select view)
