let table = <:table< test (
  recette text NOT NULL,
  nom text NOT NULL,
  poids bigint NOT NULL
    DEFAULT($ <:value< 0L >> $),
  categorie text NOT NULL
    DEFAULT($ <:value< "aucune categorie specifiee" >> $)
) >>

let insert name =
  <:insert< $table$ :=
    { recette = "foo";
      nom = $string:name$;
      poids = table?poids;
      categorie = table?categorie } >>

let count =
  << group {count = count[t]} |
      t in $table$; t.recette = "foo" >>

let () =
  Random.self_init ();
  let name = Printf.sprintf "bar_%d" (Random.bits ()) in
  let dbh = PGOCaml.connect () in
  let log = stderr in
  Printf.printf "count before insert : %Ld\n%!" (Query.view_one ~log dbh count)#!count;
  Query.query ~log dbh (insert name);
  Printf.printf "count after insert : %Ld\n%!" (Query.view_one dbh count)#!count;
  Query.query ~log dbh <:delete< t in $table$ | t.nom = $string:name$ >> ;
  Printf.printf "count after delete : %Ld\n%!" (Query.view_one dbh count)#!count;
  PGOCaml.close dbh
