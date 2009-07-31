let comp = << {a = row.a} | row <- $ << {a = 1; row = {a = 2}} | >> $ >>

let () =
  let res = List.hd (Query.view (PGOCaml.connect ()) comp) in
  Printf.printf "a:%d\n"
    (Sql.get res#a)
