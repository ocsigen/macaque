open Sql

(* let singleton x = << {value = $x$} | >> *)
let singleton x = << {value = $x$} | >>

(* let singleton_option x = << {value = nullable $x$} | >> *)
let singleton_option x = << {value = nullable $x$} | >>

(* let null = << {null = null} | >> *)
let null = << {null = null} | >>

(*

  ocamlbuild -pp 'camlp4o pa_comp.cmo' tests/singleton.inferred.mli
  cat _build/tests/singleton.inferred.mli

*)

let () =
  let dbh = PGOCaml.connect () in
  List.iter (fun r -> ignore (r#?null = None)) (Query.Simple.view dbh null);
  List.iter (fun r -> ignore (r#?value = Some 1))
    (Query.Simple.view dbh (singleton_option <:value< 1 >>))
