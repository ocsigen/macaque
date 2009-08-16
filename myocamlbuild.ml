(*
  This file is Public Domain
*)

open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)

module OCamlFind =
struct
  (* these functions are not really officially exported *)
  let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
    
  let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings
  
  (* this lists all supported packages *)
  let find_packages () =
    blank_sep_strings &
      Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let  before_options () =
    (* by using Before_options one let command line options have an higher priority *)
    (* on the contrary using After_options will guarantee to have the higher priority *)
    
    (* override default commands by ocamlfind ones *)
    Options.ocamlc     := ocamlfind & A"ocamlc";
    Options.ocamlopt   := ocamlfind & A"ocamlopt";
    Options.ocamldep   := ocamlfind & A"ocamldep";
    Options.ocamldoc   := ocamlfind & A"ocamldoc";
    Options.ocamlmktop := ocamlfind & A"ocamlmktop"

  let after_rules () =
       (* When one link an OCaml library/binary/package, one should use -linkpkg *)
       flag ["ocaml"; "byte"; "program"] & A"-linkpkg";

       (* For each ocamlfind package one inject the -package option when
       	* compiling, computing dependencies, generating documentation and
       	* linking. *)
       List.iter begin fun pkg ->
         flag ["ocaml"; "infer_interface";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());

       (* Like -package but for extensions syntax. Morover -syntax is useless
       	* when linking. *)
       List.iter begin fun syntax ->
         flag ["ocaml"; "infer_interface";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
       end (find_syntaxes ());
       
       (* The default "thread" tag is not compatible with ocamlfind.
          Indeed, the default rules add the "threads.cma" or "threads.cmxa"
          options when using this tag. When using the "-linkpkg" option with
          ocamlfind, this module will then be added twice on the command line.
       
          To solve this, one approach is to add the "-thread" option when using
          the "threads" package using the previous plugin.
        *)
       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"]    (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "doc"]     (S[A "-I"; A "+threads"])
end

module Sql_syntax = struct
  let before_options () = ()
  let after_rules () =
    let comp, descr, bana =
      "pa_comp.cmo", "pa_descr.cmo", "pa_bananas.cmo" in
    flag ["ocaml"; "pp"; "use_comp"] (A comp);
    flag ["ocaml"; "pp"; "use_descr"] (A descr);
    flag ["ocaml"; "pp"; "use_bananas"] (A bana);
    flag ["ocaml"; "pp"; "use_macaque"] (S[A comp;A descr;A bana]);
    dep ["ocaml"; "ocamldep"; "use_comp"] [comp];
    dep ["ocaml"; "ocamldep"; "use_descr"] [descr];
    dep ["ocaml"; "ocamldep"; "use_bananas"] [bana];
    dep ["ocaml"; "ocamldep"; "use_macaque"] [comp;descr;bana];
    ()
end

let _ = dispatch begin function
   | Before_options ->
       OCamlFind.before_options     ();
       Sql_syntax.before_options ();
   | After_rules ->
       OCamlFind.after_rules     ();
       Sql_syntax.after_rules ();
   | _ -> ()
end
