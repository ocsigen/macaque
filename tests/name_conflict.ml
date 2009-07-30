open Sql

let test table table_valides dictionnaire =
(*   << {res = r.nom} |
         e <- $table$;
         r <- $table_valides$;
         e.id = r.id;
         r <- $dictionnaire$;
         r.nom = e.id >> *)
  << {res = r.nom_dictionnaire} |
      e <- $table$;
      r <- $table_valides$;
      e.id = r.id_table_valides;
      r <- $dictionnaire$;
      r.nom_dictionnaire = e.id >>

(*
  camlp4o _build/pa_comp.cmo tests/name_conflict.ml
*)
