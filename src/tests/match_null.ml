let cast nullable_int =
  <:value< match nullable_int with
           | null -> 0
           | n -> n + n >>

let () =  
  let view = << {a = $cast$ null; b = $cast$ 1} >> in
  let t = Query.view_one ~log:stdout (PGOCaml.connect ()) view in
  Printf.printf "a:%ld\tb:%ld\n" t#!a t#!b

(*
  with this test case, a vicious bug occured :
  'cast <:value< null >>' expands to
 
   CASE NULL
     WHEN NULL IS NULL THEN 0
     ELSE NULL + NULL
   END

   This SQL code doesn't typecheck, because of NULL + NULL overloading
   resolution. While we can garantee that this code branch will never
   be accessed when 'n' is NULL, the SQL typer can't and raises an
   error.

   The current work-around is to output the null-case only when the
   matched value is statically known to be NULL.
   See Sql_builders.match comment.
*)
