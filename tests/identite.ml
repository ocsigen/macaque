open Sql

let id table = << t | t <- $table$ >>

(*
  sh infer.sh tests/identite.ml
*)
