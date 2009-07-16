open Sql

let nullable = function
  | None -> "NULL"
  | Some str -> str

let execute comp =
  let dbh = PGOCaml.connect ~host:"myhost" ~database:"base" () in
  let query = string_of_concrete comp.concrete in
  print_endline query;
  let name = "query_result" in
  ignore (PGOCaml.prepare dbh ~query ~name ());
  let rows =
    let parse row =
      comp.result_parser (Array.of_list (List.map nullable row), ref 0) in
    List.map parse (PGOCaml.execute dbh ~name ~params:[] ()) in
  PGOCaml.close dbh;
  rows        
  
let iter comp f = List.iter f (execute comp) 

