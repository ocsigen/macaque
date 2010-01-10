(* Check that binding rows inside update and delete queries is possible *)

let update = <:update<
  t in $Base.liste$ := {recette = t.recette} |
      r in $Base.recette$;
      t.recette = nullable r.id;
      r.nom = "foo"
>>

let delete = <:delete<
  t in $Base.liste$ |
      r in $Base.recette$;
      t.recette = nullable r.id;
      r.nom = "foo"
>>

let () =
  print_endline (Sql.sql_of_query update);
  print_endline (Sql.sql_of_query delete)
 

