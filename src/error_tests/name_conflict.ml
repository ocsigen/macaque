open Sql

let test table table_valides dictionnaire =
(*   << {res = r.nom} |
         e in $table$;
         r in $table_valides$;
         e.id = r.id;
         r in $dictionnaire$;
         r.nom = e.id >> *)
  << {res = r.nom_dictionnaire} |
      e in $table$;
      r in $table_valides$;
      e.id = r.id_table_valides;
      r in $dictionnaire$;
      r.nom_dictionnaire = e.id >>

(*
  camlp4o _build/pa_comp.cmo tests/name_conflict.ml
*)
