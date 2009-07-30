open Sql

let foo =
  <:select<
    {ingredient = i.nom; recette = r.nom} |
      i <- $Base.ingredient$;
      r <- $Base.recette$;
      l <- $Base.liste$;
      l.ingredient = nullable i.id;
      l.recette = nullable r.id >>

let () =
  let dbh = PGOCaml.connect ~database:"base" () in
  List.iter
    (fun r ->
       let get v = match Sql.Value.getn v with
         | None -> "NULL"
         | Some thing -> thing in
       Printf.printf "(%s, %s)\n" (get r#ingredient) (get r#recette))
    (Query.query dbh foo)
