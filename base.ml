open Sql

let string_of_string (x : string) = x

let recette = <:sql_table_descr<
  TABLE test.recette (
    id integer NOT NULL,
    nom text
  ) >>

let ingredient = <:sql_table_descr<
  TABLE test.ingredient (
    id integer NOT NULL,
    nom text
  ) >>

let liste = <:sql_table_descr<
  TABLE test.liste (
    recette integer,
    ingredient integer
  ) >>
