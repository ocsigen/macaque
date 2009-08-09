let pack table = <:select< {a = 1; row = nullable row; b = 2} | row <- $table$ >>

let pack2 = <:select< {a = 10; row = {c = 30; row = {d = 40; e = 50}}; b = 20} | >>

let pack3 = <:select<
  {a = 1; a_row = {b = 2; b_row = row}; f = 6} |
      row <- $ << {c = 3; 
                   c_row = {d = 4; d2 = 4};
                   c_row2 = {e = 5; e2 = 5}} | >> $ >>

let (!) = Sql.get


let () =
  let dbh = PGOCaml.connect ~database:"base" () in
  List.iter
  (fun x ->
     match Sql.getn x#row with
       | None -> assert false
       | Some row ->
           Printf.printf "a:%d\tb:%d\tc:%d\td:%d\n%!"
           !(x#a) !(x#b) !(row#c) !(row#d))
  (Query.Simple.query dbh (pack << {c = 3; d = 4} | >>));
  List.iter
    (fun x ->
       let row = !(x#row) in
       let row' = !(row#row) in
       Printf.printf "a:%d\tb:%d\tc:%d\td:%d\te:%d\n%!"
         !(x#a) !(x#b) !(row#c) !(row'#d) !(row'#e))
    (Query.Simple.query dbh pack2);
  List.iter
    (fun x ->
       let a_row = !(x#a_row) in
       let b_row = !(a_row#b_row) in
       let c_row = !(b_row#c_row) in
       let c_row2 = !(b_row#c_row2) in
       Printf.printf "a:%d\tb:%d\tc:%d\td:%d\te:%d\tf:%d\n%!"
         !(x#a) !(a_row#b) !(b_row#c) !(c_row#d) !(c_row2#e) !(x#f))
    (Query.Simple.query dbh pack3)
