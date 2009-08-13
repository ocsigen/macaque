open Sql

(* let ids table = << {id = t.id} | t in $table$ >> *)
let ids table = << {id = t.id} | t in $table$ >>

(* let select id table = << t | t in $ids table$; t.id = $int:id$ >> *)
let select id table =  << t | t in $ids table$; t.id = $int:id$ >>

(* let exists table = *)
(*   let ok = <:sql_val< $int:1$ >> in *)
(*   << {exists = $ok$} | t in $table$ >> *)
let exists table =
  let ok = <:value< $int:1$ >> in
  << {exists = $ok$} | t in $table$ >>

let result =
  List.iter (fun t -> if Sql.get t#exists = 1 then Printf.printf "exists !\n")
    (Query.Simple.view (PGOCaml.connect ())
       (exists (select
                  (read_int ())
                  (Sql.View.table Base.recette))))
  
(*
  sh infer.sh tests/compose.ml

  ocamlbuild tests/compose.byte
  ./compose.byte

*)
