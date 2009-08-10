let insert = <:insert< $table:Base.ingredient$ := {id = 42; nom = nullable "reponse"} | >>

let count = <:select< group {count = count[t]} by {} | t <- $table:Base.ingredient$ >>

let select = <:select< t | t <- $table:Base.ingredient$ >>

let update = <:update< t <- $table:Base.ingredient$
                       := {nom = nullable "question"}
                       | t.id = 42 >>

let delete = <:delete< t <- $table:Base.ingredient$ | t.id = 42 >>

let () =
  let (!) query = Printf.printf "%s;\n" (Sql.sql_of_query query) in
  !count; !insert; !count;
  !select; !update; !select;
  !delete; !count
