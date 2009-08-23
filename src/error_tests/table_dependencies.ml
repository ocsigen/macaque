let view = << t2 | t in $table:Base.recette$; t2 in $one:t$ >>
let () = ignore (Query.Simple.view ~log:stdout (PGOCaml.connect ()) view)
