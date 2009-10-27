let comp = << {a = row.a} | row in $ << {a = 1; row = {a = 2}} >> $ >>

let () =
  let res = List.hd (Query.view (PGOCaml.connect ()) comp) in
  Printf.printf "a:%ld\n" res#!a
