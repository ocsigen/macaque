open Sql

let select table id =
  << {nom = t.nom; id = t.id} |
     t in $table$;
     t.id = $int32:id$ >>

(*
  sh infer.sh tests/parametred.ml
*)
