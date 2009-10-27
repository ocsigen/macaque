let () =
  let dbh = PGOCaml.connect () in

  let view =
    let view = << {row = {a = 1}} >> in
    let row = (Query.view_one ~log:stdout dbh view)#row in
    << $row$ >> in

  let () =  
    let res = (Query.view_one ~log:stdout dbh view) in
    PGOCaml.close dbh;
    Printf.printf "%ld\n" res#!a in
  ()
