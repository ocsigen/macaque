let table = Sql.View.one <:value< {a = 1} >>
let insert = <:insert< $table$ := {a = 2} >>
