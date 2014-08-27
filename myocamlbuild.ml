(*
  This file is Public Domain
*)

(* OASIS_START *)
(* OASIS_STOP *)

let () =
  Ocamlbuild_plugin.dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
       | After_rules ->
           let maca, bana = "src/pa_macaque.cmo", "src/pa_bananas.cmo" in
           flag ["ocaml"; "pp"; "use_macaque"] (S[A maca;A bana]);
           flag ["ocaml"; "pp"; "use_check"] (A "-check_tables");
           dep ["ocaml"; "ocamldep"; "use_macaque"] [maca; bana];
       | _ -> ()
    )
