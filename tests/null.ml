let test =
  let dbh = PGOCaml.connect () in
  fun comp ->
    Printf.printf "%s --> %d result rows\n"
      (Sql.sql_of_view comp) (List.length (Query.view dbh comp))

let () =
  test << {} | >> ;
  test << {} | null = null >>
