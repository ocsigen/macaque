let test =
  let dbh = PGOCaml.connect ~database:"base" () in
  fun comp ->
    Printf.printf "%s --> %d result rows\n"
      (Sql.string_of_concrete_view comp.Sql.concrete)
      (List.length (Query.view dbh comp))

let () =
  test << {} | >> ;
  test << {} | null = null >>
