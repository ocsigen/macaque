open Sql

let ids table = << {id = t.id} | t in $table$ >>

let select id table =  << t | t in $ids table$; t.id = $int32:id$ >>

let exists table =
  let ok = <:value< 1 >> in
  << {exists = $ok$} | t in $table$ >>

let result =
  List.iter (fun t -> if t#!exists = 1l then Printf.printf "exists !\n")
    (Query.Simple.view (PGOCaml.connect ())
       (exists (select
                  (Int32.of_int (read_int ()))
                  Base.recette)))
  
(*
  sh infer.sh tests/compose.ml

  ocamlbuild tests/compose.byte
  ./compose.byte

*)
