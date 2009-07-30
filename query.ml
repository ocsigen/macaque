open Sql

let nullable = function
  | None -> "NULL"
  | Some str -> str

let view dbh comp =
  let query = sql_of_comp comp in
  print_endline query;
  let name = "view_result" in
  ignore (PGOCaml.prepare dbh ~query ~name ());
  let parse row =
    parser_of_comp comp (Array.of_list (List.map nullable row)) in
  List.map parse (PGOCaml.execute dbh ~name ~params:[] ())
