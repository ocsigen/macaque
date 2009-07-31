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
