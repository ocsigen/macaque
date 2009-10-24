open Sql

let foo =
  <:select<
    {ingredient = i.nom; recette = r.nom;} |
      i in $Base.ingredient$;
      r in $Base.recette$;
      l in $Base.liste$;
      l.ingredient = nullable i.id;
      l.recette = nullable r.id; >>

let () =
  let dbh = PGOCaml.connect () in
  List.iter
    (fun r ->
       let get = function
         | None -> "NULL"
         | Some thing -> thing in
       Printf.printf "(%s, %s)\n" (get r#?ingredient) (get r#?recette))
    (Query.Simple.query dbh foo)
