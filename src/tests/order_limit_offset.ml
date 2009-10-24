let simple_order =
  << t order by t.id desc | t in $Base.recette$ >>

let order_limit_offset =
  << t order by t.id desc
     limit 2 offset $int32:3l$ |
       t in $Base.ingredient$ >>

let complex_order =
  << {ingredient = i.nom; recette = r.nom}
    order by l desc, r.id asc |
      i in $Base.ingredient$;
      r in $Base.recette$;
      l in $Base.liste$;
      l.ingredient = nullable i.id;
      l.recette = nullable r.id >>


let () =
  let dbh = PGOCaml.connect () in
  let get = function None -> "NULL" | Some thing -> thing in
  let query x = Query.Simple.view dbh ~log:stdout x in
  print_endline "simple order :";
  let res = query simple_order in
  List.iter (fun r -> Printf.printf "\t%ld : %s\n" r#!id (get r#?nom)) res;
  print_endline "\norder limit offset :";
  let res = query order_limit_offset in
  List.iter (fun r -> Printf.printf "\t%ld : %s\n" r#!id (get r#?nom)) res;
  print_endline "\ncomplex order :";
  let res = query complex_order in
  List.iter (fun r -> Printf.printf "\t(%s, %s)\n" (get r#?recette) (get r#?ingredient)) res
