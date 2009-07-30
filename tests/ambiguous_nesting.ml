let comp = << {a = row.a} | row <- $ << {a = 1; row = {a = 2}} | >> $ >>

let () =
  let res = List.hd (Query.execute comp) in
  Printf.printf "a:%d\n"
    res#a
