let insert = <:insert< $Base.ingredient$ := {id = 42; nom = nullable "reponse"} | >>

let count = <:select< group {count = count[t]} by {} | t in $Base.ingredient$ >>

let select = <:select< t | t in $Base.ingredient$ >>

let update = <:update< t in $Base.ingredient$
                       := {nom = nullable "question"}
                       | t.id = 42 >>

let delete = <:delete< t in $Base.ingredient$ | t.id = 42 >>

let () =
  let (!) query = Printf.printf "%s;\n" (Sql.sql_of_query query) in
  !count; !insert; !count;
  !select; !update; !select;
  !delete; !count
