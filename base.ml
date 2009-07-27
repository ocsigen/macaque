open Sql

let string_of_string (x : string) = x

let recette = <:sql_table_descr<
  TABLE recette (
    id integer NOT NULL,
    nom text
  ) >>

let ingredient = <:sql_table_descr<
  TABLE ingredient (
    id integer NOT NULL,
    nom text
  ) >>

let liste = <:sql_table_descr<
  TABLE liste (
    recette integer,
    ingredient integer
  ) >>
