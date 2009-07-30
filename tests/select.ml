open Sql

let select table id = << t | t <- $table$; t.id = $id$ >>

let select_and_project table id = << {id = t.id} | t <- $table$; t.id = $id$ >>

(*
  sh infer.sh tests/select.ml
*)
