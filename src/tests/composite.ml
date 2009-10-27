let pack table = <:select< {a = 1; row = nullable row; b = 2} | row in $table$ >>

let pack2 = <:select< {a = 10; row = {c = 30; row = {d = 40; e = 50}}; b = 20} >>

let pack3 = <:select<
  {a = 1; a_row = {b = 2; b_row = row}; f = 6} |
      row in $ << {c = 3;
                   c_row = {d = 4; d2 = 4};
                   c_row2 = {e = 5; e2 = 5}} >> $ >>


let () =
  let dbh = PGOCaml.connect ~database:"base" () in
  List.iter
  (fun x ->
     match x#?row with
       | None -> assert false
       | Some row ->
           Printf.printf "a:%ld\tb:%ld\tc:%ld\td:%ld\n%!"
             x#!a x#!b row#!c row#!d)
  (Query.query dbh (pack << {c = 3; d = 4} >>));
  List.iter
    (fun x ->
       Printf.printf "a:%ld\tb:%ld\tc:%ld\td:%ld\te:%ld\n%!"
         x#!a x#!b x#!row#!c x#!row#!row#!d x#!row#!row#!e)
    (Query.query dbh pack2);
  List.iter
    (fun x ->
       Printf.printf "a:%ld\tb:%ld\tc:%ld\td:%ld\te:%ld\tf:%ld\n%!"
         x#!a x#!a_row#!b x#!a_row#!b_row#!c
         x#!a_row#!b_row#!c_row#!d x#!a_row#!b_row#!c_row2#!e x#!f)
    (Query.query ~log:stdout dbh pack3)
