let next seq = << {x = nextval $seq$} >>
let curr seq = << {x = currval $seq$} >>

let seq = Base.test_seq

open Printf

let () =
  let dbh = PGOCaml.connect () in
  let test x_view = (Query.Simple.view_one dbh x_view)#!x in
  for i = 1 to 5 do
    printf "next value : %Ld\n" (test (next seq))
  done;
  printf "current value : %Ld\n" (test (curr seq))

