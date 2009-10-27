(* Example of auto-incrementing emulation using SEQUENCE functions *)
let table =
  <:table< sequence_test
    ( id bigint NOT NULL,
      text text ) >>

let id_seq = <:sequence< bigserial "test_id_seq" >>

let insert text =
  <:insert< $table$ := {id = nextval $id_seq$; text = $string:text$ } >>

open Printf

let () =
  let dbh = PGOCaml.connect () in
  Query.query dbh ~log:stdout (insert "test 1");
  Query.query dbh ~log:stdout (insert "test 2");
  Query.query dbh ~log:stdout (insert "test 3");
  Printf.printf "%Ld\n"
    (Query.view_one dbh ~log:stdout << {x = currval $id_seq$} >>)#!x
