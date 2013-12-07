(* This is an example of table description
    
    An associated sql base description with some toy data is given in
    base.sql
*)

let recette = <:table< recette (
  id integer NOT NULL,
  nom text
) >>

let ingredient = <:table< ingredient (
  id integer NOT NULL,
  nom text
) >>

let liste = <:table< liste (
  recette integer,
  ingredient integer
) >>

let test1 = <:table< test1 ( text text ) >>
let test2 = <:table< test2 ( text text ) >>
