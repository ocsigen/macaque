open Sql

let nullable = function
  | None -> "NULL"
  | Some str -> str

let execute comp =
  let dbh = PGOCaml.connect ~host:"myhost" ~database:"base" () in
  let query = sql_of_comp comp in
  print_endline query;
  let name = "query_result" in
  ignore (PGOCaml.prepare dbh ~query ~name ());
  let rows =
    let parse row =
      parser_of_comp comp (Array.of_list (List.map nullable row)) in
    List.map parse (PGOCaml.execute dbh ~name ~params:[] ()) in
  PGOCaml.close dbh;
  rows

let iter comp f = List.iter f (execute comp)

