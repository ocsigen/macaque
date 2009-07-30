open Sql

let id_table table = << t | t <- $table$ >>

let id_value x = <:value< $x$ >>
