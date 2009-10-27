let full table = <:select< row | row in $table$ >>

let recette = <:table< recette ( nom text NOT NULL ) >>

let () =
  let do_row row = print_endline row#!nom in
  List.iter do_row (Query.query
                      (PGOCaml.connect ())
                      (full recette))
