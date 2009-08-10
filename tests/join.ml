open Sql

let foo =
  <:select<
    {ingredient = i.nom; recette = r.nom} |
      i <- $table:Base.ingredient$;
      r <- $table:Base.recette$;
      l <- $table:Base.liste$;
      l.ingredient = nullable i.id;
      l.recette = nullable r.id >>

let () =
  let dbh = PGOCaml.connect () in
  List.iter
    (fun r ->
       let get v = match Sql.getn v with
         | None -> "NULL"
         | Some thing -> thing in
       Printf.printf "(%s, %s)\n" (get r#ingredient) (get r#recette))
    (Query.Simple.query dbh foo)
