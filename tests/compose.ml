open Sql

let ids table = << {id = t.id} | t in $table$ >>

let select id table =  << t | t in $ids table$; t.id = $int:id$ >>

let exists table =
  let ok = <:value< 1 >> in
  << {exists = $ok$} | t in $table$ >>

let result =
  List.iter (fun t -> if t#!exists = 1 then Printf.printf "exists !\n")
    (Query.Simple.view (PGOCaml.connect ())
       (exists (select
                  (read_int ())
                  (Sql.View.table Base.recette))))
  
(*
  sh infer.sh tests/compose.ml

  ocamlbuild tests/compose.byte
  ./compose.byte

*)
